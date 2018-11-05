%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(erlando_typeclass).

-behaviour(gen_server).

%% API
-export([register_application/1, register_modules/1]).
-export([core/1]).
-export([types/0, typeclasses/0, behaviours/0]).
-export([type_with_remote/4, type_to_patterns/1, pattern_to_clause/3]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {behaviour_modules = maps:new(), typeclasses = [],
                type_aliases = [], types = maps:new(), exported_types = sets:new(), mod_recs = dict:new()}).

-record(cache,
        {
          types = maps:new(),
          mod_recs = {mrecs, dict:new()}
        }).

%%%===================================================================
%%% API
%%%===================================================================
register_application(Application) ->
    case application:get_key(Application, modules) of
        {ok, Modules} ->
            register_modules(Modules);
        undefined ->
            {error, undefined}
    end.

register_modules(Modules) ->
    gen_server:call(?SERVER, {register_modules, Modules}).

types() ->
    gen_server:call(?SERVER, types).

typeclasses() ->
    gen_server:call(?SERVER, typeclasses).

behaviours() ->
    gen_server:call(?SERVER, behaviours).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({register_modules, Modules}, _From, 
            #state{behaviour_modules = BehaviourModules,
                   typeclasses = Typeclasses, types = Types,
                   exported_types = ETypes, mod_recs = ModRecs} = State) ->
    {NETypes, NModRecs} =
        lists:foldl(
          fun(Module, {ETypeAcc, ModRecAcc}) ->
                  update_types_and_rec_map(Module, ETypeAcc, ModRecAcc)
          end, {ETypes, ModRecs}, Modules),

    NTypeclasses = 
        lists:foldl(
          fun(Module, Acc) ->
                  Classes = superclasses(Module),
                  case Classes of
                      [] ->
                          Acc;
                      _ ->
                          ordsets:add_element(Module, Acc)
                  end
          end, Typeclasses, Modules),
    {NTypes, NBehaviourModules} = 
        lists:foldl(
          fun(Module, {TIAcc, TBMAcc}) ->
                  {TypeInstanceMap, TypeBehaviourModuleMap} = 
                      module_type_info(Module, NTypeclasses, NETypes, NModRecs),
                  {merge_type_instance(TIAcc, TypeInstanceMap),
                   maps:merge(TBMAcc, TypeBehaviourModuleMap)}
          end, {Types, BehaviourModules}, Modules),
    NNTypes = 
        maps:map(
          fun(Type, undefined) ->
                  [{tuple,[{atom, Type},any]}];
             (_Type, Patterns) ->
                  Patterns
          end, NTypes),
    do_load_module(NNTypes, NTypeclasses, NBehaviourModules),
    {reply, ok, State#state{behaviour_modules = NBehaviourModules,
                            typeclasses = NTypeclasses, types = NNTypes,
                            exported_types = NETypes, mod_recs = NModRecs}};

handle_call(types, _From, #state{types = Types} = State) ->
    {reply, {ok, Types}, State};
handle_call(typeclasses, _From, #state{typeclasses = Typeclasses} = State) ->
    {reply, {ok, Typeclasses}, State};
handle_call(behaviours, _From, #state{behaviour_modules = Behaviours} = State) ->
    {reply, {ok, Behaviours}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
type_with_remote(Module, Type, Args, Modules) ->
    {ExportedTypes, TRecDict} = 
        lists:foldl(
          fun(Mod, {TypeAcc, RecAcc}) ->
                  update_types_and_rec_map(Mod, TypeAcc, RecAcc)
          end, {sets:new(), dict:new()}, [Module|Modules]),
    type_with_remote(Module, Type, Args, ExportedTypes, TRecDict).

type_with_remote(Module, Type, Args, ExportedTypes, TRecMap) ->
    RecMap = case dict:find(Module, TRecMap) of
                 {ok, Val} ->
                     Val;
                 error ->
                     #{}
             end,
    Type0 = {type, Type, Args},
    Type1 = {type, {Module, Type, Args}},
    case maps:find(Type0, RecMap) of
        {ok, {{Module, _FileLine, TypeForm, _ArgNames}, _}} ->
            Cache = #cache{mod_recs = {mrecs, TRecMap}},
            {CType, _NCache} = erl_types_R20:t_from_form(TypeForm, ExportedTypes, Type1, undefined, #{}, Cache),
            {ok, CType};
        error ->
            {error, undefined_type}
    end.

type_to_patterns({c, tuple, Tuples, _}) ->
    TupleLists = 
        lists:foldl(
          fun(TupleValue, Accs) ->
              Patterns = type_to_patterns(TupleValue),
                  case Accs of
                      [] ->
                          lists:map(
                            fun(Pattern) ->
                                    [Pattern]
                            end, Patterns);
                      Accs ->
                          [[Pattern|AccValue] || 
                              AccValue <- Accs,
                              Pattern <- Patterns
                          ]
                  end
          end, [], Tuples),
    lists:map(
      fun(TupleList) ->
              {tuple, lists:reverse(TupleList)}
      end, TupleLists);
type_to_patterns({c, function, _Function, _}) ->
    [{guard, is_function}];
type_to_patterns({c, atom, Atoms, _}) ->
    lists:map(fun(Atom) -> {atom, Atom} end, Atoms);
type_to_patterns({c, tuple_set, [{_N, Sets}], _}) ->
    lists:foldl(fun(Item, Acc) -> type_to_patterns(Item) ++ Acc end, [],Sets);
type_to_patterns({c, union, Unions, _}) ->
    lists:foldl(fun(Item, Acc) -> type_to_patterns(Item) ++ Acc end, [],Unions);
type_to_patterns({c, list, _, _}) ->
    [{guard, is_list}];
type_to_patterns({c, map, _, _}) ->
    [{guard, is_map}];
type_to_patterns({c, binary, _, _}) ->
    [{guard, is_binary}];
type_to_patterns({c, var, _, _}) ->
    [any];
type_to_patterns(any) ->
    [any];
type_to_patterns(none) ->
    [];
type_to_patterns({c, _Type, _Body, _Qualifier}) ->
    [].

pattern_to_clause(Line, Type, Pattern) ->
    {NPattern, Guards, _} = 
        pattern_to_pattern_gurads(Line, Pattern, [], 1),
    GuardTest = 
        case Guards of
            [] ->
                [];
            _ ->
                [Guards]
        end,
    {clause, Line, [NPattern], GuardTest, [{atom, Line, Type}]}.

pattern_to_pattern_gurads(Line, {tuple, Tuples}, Guards, Offset) ->
    {TupleList, NGuards, NOffset} = 
        lists:foldl(
          fun(Element, {PatternAcc, GuardAcc, OffsetAcc}) ->
                  {Pattern, NGuardAcc, NOffsetAcc} = 
                      pattern_to_pattern_gurads(Line, Element, GuardAcc, OffsetAcc),
                  {[Pattern|PatternAcc], NGuardAcc, NOffsetAcc}
          end, {[], Guards, Offset}, Tuples),
    {{tuple, Line, lists:reverse(TupleList)}, NGuards, NOffset};
pattern_to_pattern_gurads(Line, any, Guards, Offset) ->
    {{var, Line, '_'}, Guards, Offset};
pattern_to_pattern_gurads(Line, {atom, Atom}, Guards, Offset) ->
    {{atom, Line, Atom}, Guards, Offset};
pattern_to_pattern_gurads(Line, {guard, Guard}, Guards, Offset) ->
    ArgName = list_to_atom("Args" ++ integer_to_list(Offset)),
    {{var, Line, ArgName},
     [{call, Line, {atom, Line, Guard}, [{var, Line, ArgName}]}|Guards], Offset + 1}.

module_type_info(Module, Typeclasses, ETypes, ModRecs) ->
    TypeAttrs = types(Module),
    Behaviours = behaviours(Module),
    TypeInstanceMap = 
        lists:foldl(
          fun({Type, UsedTypes}, Acc1) ->
                  Patterns = type_patterns(Module, UsedTypes, ETypes, ModRecs),
                  maps:put(Type, Patterns, Acc1);
             (Type, Acc1) when is_atom(Type) ->
                  case maps:find(Type, Acc1) of
                      {ok, _Patterns} ->
                          Acc1;
                      error ->
                          maps:put(Type, undefined, Acc1)
                  end
          end, maps:new(), TypeAttrs),
    Types = maps:keys(TypeInstanceMap),
    TypeBehaviourMap = 
        lists:foldl(
          fun(Type, Acc1) ->
                  lists:foldl(
                    fun(Behaviour, Acc2) ->
                            case ordsets:is_element(Behaviour, Typeclasses) of
                                true ->
                                    maps:put({Type, Behaviour}, Module, Acc2);
                                false ->
                                    Acc2
                            end
                    end, Acc1, Behaviours)
          end, maps:new(), Types),
    {TypeInstanceMap, TypeBehaviourMap}.

merge_type_instance(TypeInstanceMap, NTypeInstanceMap) ->
    maps:fold(
      fun(Type, Pattern, Acc) ->
              case maps:find(Type, Acc) of
                  {ok, undefined} ->
                      maps:put(Type, Pattern, Acc);
                  {ok, _} ->
                      Acc;
                  error ->
                      maps:put(Type, Pattern, Acc)
              end
      end, TypeInstanceMap, NTypeInstanceMap).

superclasses(Module) ->
    ast_traverse:module_attributes(superclass, Module).

types(Module) ->
    lists:flatten(ast_traverse:module_attributes(erlando_type, Module)).

behaviours(Module) ->
    lists:flatten(ast_traverse:module_attributes(behaviour, Module)).

do_load_module(Types, Typeclasses, BehaviourModules) ->
    TypeclassModule = {attribute,0,module,typeclass},
    Export = {attribute,0,export,[{module,2}, {is_typeclass, 1}, {type, 1}]},
    TypesFun = generate_type(Types),
    IsTypeClass = generate_is_typeclass(Typeclasses),
    Module = generate_module(BehaviourModules),
    {ok, Mod, Bin} = compile:forms([TypeclassModule, Export, TypesFun, IsTypeClass, Module]),
    code:load_binary(Mod, [], Bin).

generate_type(Types) ->
    Clauses = 
        maps:fold(
          fun(Type, Patterns, Acc) ->
                  NPatterns = 
                      case Patterns of
                          undefined ->
                              [{tuple,[{atom, Type},any]}];
                          _ ->
                              Patterns
                      end,
                  lists:map(
                    fun(Pattern) ->
                            pattern_to_clause(0, Type, Pattern)
                    end, NPatterns) ++ Acc
          end, [], Types),
    LastClause = {clause, 0, [{var, 0, '_'}], [], [{atom, 0, undefined}]},
    {function, 0, type, 1, Clauses ++ [LastClause]}.

generate_is_typeclass(Typeclasses) ->
   Clauses = 
        lists:foldl(
          fun(Typeclass, Acc) ->
                  [is_typeclass_clause(0, Typeclass)|Acc]
          end, [], Typeclasses),
    LastClause = {clause, 0, [{var, 0, '_A'}], [], [{atom, 0, false}]},
    {function, 0, is_typeclass, 1, lists:reverse([LastClause|Clauses])}.

generate_module(BehaviourModules) ->
    Clauses = 
        maps:fold(
          fun({Type, Behaviour}, Module, Acc) ->
                  [module_clause(0, Type, Behaviour, Module)|Acc]
          end, [], BehaviourModules),
    LastClause = {clause, 0, [{var, 0, 'A'}, {var, 0, 'B'}], [], 
                  [{call, 0, {atom, 0, exit}, 
                    [{tuple, 0, [{atom, 0, unregisted_module}, {tuple, 0, [{var, 0, 'A'}, {var, 0, 'B'}]}]}]}]},
    {function, 0, module, 2, lists:reverse([LastClause|Clauses])}.

   
is_typeclass_clause(Line, Typeclass) ->
    {clause, Line, [{atom, Line, Typeclass}], [], [{atom, Line, true}]}.

module_clause(Line, Type, Behaviour, Module) ->
    {clause, 1, [{atom, Line, Type}, {atom, Line, Behaviour}], [],
     [{atom, Line, Module}]}.

type_patterns(Module, Types, ETypes, ModRecs) ->
    lists:foldl(
      fun({Type, Arity}, Acc) ->
              case type_with_remote(Module, Type, Arity, ETypes, ModRecs) of
                  {ok, CType} ->
                      Patterns = type_to_patterns(CType),
                      lists:usort(Patterns ++ Acc);
                  {error, _} ->
                      Acc
              end
      end, [], Types).

core(Module) ->
    case code:get_object_code(Module) of
        {Module, _, Beam} ->
            dialyzer_utils_R20:get_core_from_beam(Beam);
        error ->
            {error, {not_loaded, Module}}
    end.

update_types_and_rec_map(Module, Types, MRecDict) ->
    case core(Module) of
        {ok, Core} ->
            case rec_map(Core) of
                {ok, RecMap} ->
                    MTypes = exported_types(Core),
                    NETypeAcc = sets:union(MTypes, Types),
                    NMRecDict = 
                        case maps:size(RecMap) of
                            0 ->
                                MRecDict;
                            _ ->
                                dict:store(Module, RecMap, MRecDict)
                        end,
                    {NETypeAcc, NMRecDict};
                {error, _Reason} ->

                    {Types, MRecDict}
            end;
        {error, _Reason} ->
            {Types, MRecDict}
    end.

exported_types(Core) ->
    Attrs = cerl:module_attrs(Core),
    ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs, cerl:is_literal(L1),
                                      cerl:is_literal(L2),
                                      cerl:concrete(L1) =:= 'export_type'],
    ExpTypes2 = lists:flatten(ExpTypes1),
    M = cerl:atom_val(cerl:module_name(Core)),
    sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

rec_map(Core) ->
    dialyzer_utils_R20:get_record_and_type_info(Core).

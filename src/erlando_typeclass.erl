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
-export([module/2]).
-export([register_application/1, register_modules/1]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {behaviour_modules = maps:new(), typeclasses = [],
                type_aliases = [], types = maps:new()}).

%%%===================================================================
%%% API
%%%===================================================================

module(Type, Behaviour) ->
    typeclass:module(Type, Behaviour).

register_application(Application) ->
    case application:get_key(Application, modules) of
        {ok, Modules} ->
            register_modules(Modules);
        undefined ->
            {error, undefined}
    end.

register_modules(Modules) ->
    gen_server:call(?SERVER, {register_modules, Modules}).

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
                   typeclasses = Typeclasses, types = Types} = State) ->
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
                      module_type_info(Module, NTypeclasses),
                  {maps:merge(TypeInstanceMap, TIAcc),
                   maps:merge(TypeBehaviourModuleMap, TBMAcc)}
          end, {Types, BehaviourModules}, Modules),
    do_load_module(NTypes, NTypeclasses, NBehaviourModules),
    {reply, ok, State#state{behaviour_modules = NBehaviourModules,
                            typeclasses = NTypeclasses, types = NTypes}};

handle_call({module, Type, Behaviour}, _From,
            #state{behaviour_modules = BehaviourModules} = State) ->
    Reply = 
        case maps:find({Type, Behaviour}, BehaviourModules) of
            {ok, Module} ->
                {just, Module};
            error ->
                nothing
        end,
    {reply, Reply, State}; 

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
module_type_info(Module, Typeclasses) ->
    TypeAttrs = types(Module),
    Behaviours = behaviours(Module),
    TypeInstanceMap = 
        lists:foldl(
          fun({Type, Patterns}, Acc1) ->
                  maps:put(Type, Patterns, Acc1);
             (Type, Acc1) when is_atom(Type) ->
                  case maps:find(Type, Acc1) of
                      {ok, _Patterns} ->
                          Acc1;
                      error ->
                          maps:put(Type, undefined, Acc1)
                  end
          end, maps:new(), TypeAttrs),
    NTypeInstanceMap = 
        maps:map(
          fun(Type, undefined) ->
                  [{Type, '_'}];
             (_Type, Patterns) ->
                  Patterns
          end, TypeInstanceMap),
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
    {NTypeInstanceMap, TypeBehaviourMap}.

superclasses(Module) ->
    attributes(superclass, Module).

types(Module) ->
    lists:flatten(attributes(erlando_type, Module)).

behaviours(Module) ->
    lists:flatten(attributes(behaviour, Module)).

attributes(Attribute, Module) ->
    Attributes = Module:module_info(attributes),
    lists:foldl(
      fun({Attr, Value}, Acc) when Attr == Attribute ->
              [Value|Acc];
         (_Other, Acc) ->
              Acc
      end, [], Attributes).

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
                              [{Type, '_'}];
                          _ ->
                              Patterns
                      end,
                  lists:map(
                    fun(Pattern) ->
                            type_clause(0, Pattern, Type)
                    end, NPatterns) ++ Acc
          end, [], Types),
    FunClause = {clause, 0, [{var, 0, 'Fun'}], 
                 [[{call, 0, {atom, 0, is_function}, [{var, 0, 'Fun'}]}]],
                 [{atom, 0, function}]},
    ListClause = {clause, 0, [{var, 0, 'List'}], 
                 [[{call, 0, {atom, 0, is_list}, [{var, 0, 'List'}]}]],
                 [{atom, 0, list}]},
    LastClause = {clause, 0, [{var, 0, '_'}], [], [{atom, 0, undefined}]},
    {function, 0, type, 1, [FunClause, ListClause|Clauses] ++ [LastClause]}.

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

type_clause(Line, Pattern, Type) ->
    {clause, Line, [ast_traverse:from_value(Line, Pattern)], [], [{atom, Line, Type}]}.
   
is_typeclass_clause(Line, Typeclass) ->
    {clause, Line, [{atom, Line, Typeclass}], [], [{atom, Line, true}]}.

module_clause(Line, Type, Behaviour, Module) ->
    {clause, 1, [{atom, Line, Type}, {atom, Line, Behaviour}], [],
     [{atom, Line, Module}]}.

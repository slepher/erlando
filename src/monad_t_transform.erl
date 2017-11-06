%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_t_transform).

%% API
-export([parse_transform/2]).
%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    NForms = transform_functions(Forms),
    transform_behaviours(NForms).

transform_functions(Forms) ->
    MFs = ast_traverse:attributes_with_line(transform, Forms),
    {AllExports, AllFunctions} = 
        lists:foldl(
          fun({Line, Transform}, {GAcc, FAcc}) ->
                  {GExports, GForms} = generate_forms(Line, Transform),
                  {[{Line, GExports}|GAcc], GForms ++ FAcc}
          end, {[], []}, MFs),
    GenFunctionExports = export_funs(AllExports),
    NForms = insert_exports(GenFunctionExports, Forms, []),
    insert_functions(AllFunctions, NForms, []).

transform_behaviours(Forms) ->
    BFs = ast_traverse:attributes_with_line(transform_behaviour, Forms),
    lists:foldl(
      fun({Line, Transform}, FormsAcc) ->
              BForms = generate_behaviour_forms(Line, Transform),
              insert_bforms(BForms, FormsAcc)
      end, Forms, BFs).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
generate_behaviour_forms(Line, {Module, BehaviourPatterns, ExtraArgs, Behaviour}) when is_atom(Behaviour) ->
    generate_behaviour_forms(Line, {Module, BehaviourPatterns, ExtraArgs, [Behaviour]});
generate_behaviour_forms(Line, {Module, BehaviourPatterns, ExtraArgs, Behaviours}) ->
    lists:foldl(
      fun(Behaviour, Acc) ->
              Callbacks = Behaviour:behaviour_info(callbacks),
              Forms = 
                  lists:map(
                    fun({FName, Arity}) ->
                            NArity = Arity - length(ExtraArgs),
                            UArity = NArity + length(BehaviourPatterns),
                            Patterns = 
                                lists:map(
                                  fun(N) ->
                                          {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
                                  end, lists:seq(1, NArity)),
                            BPatterns = 
                                lists:map(
                                  fun(BehaviourPattern) ->
                                          ast_traverse:from_value(Line, BehaviourPattern)
                                  end, BehaviourPatterns),
                            GPatterns = Patterns ++ BPatterns,
                            GGuards = [],
                            GCall = [gen_call(Module, FName, NArity, Line, ExtraArgs, undefined)],
                            
                            {function, Line, FName, UArity,
                             [{clause, Line, GPatterns, GGuards, GCall}]}
                    end, Callbacks),
              Forms ++ Acc
      end, [], Behaviours).
                
insert_bforms(BForms, Forms) ->
    lists:foldl(fun insert_bform/2, Forms, BForms).

insert_bform(BForm, Forms) ->
    insert_bform(BForm, Forms, []).

insert_bform({function, _Line, FName, Arity, Clauses}, [{function, Line, FName, Arity, FClauses}|T], Heads) ->
    lists:reverse(Heads) ++ [{function, Line, FName, Arity, Clauses ++ FClauses}|T];
insert_bform({function, Line, FName, Arity, _Clauses} = BForm, [{eof, _ELine} = EOF|T], Heads) ->
    Exports = [{Line, [{FName, Arity}]}],
    GExports = export_funs(Exports),
    NForm = lists:reverse(Heads) ++ [BForm, EOF|T],
    insert_exports(GExports, NForm, []);
insert_bform(BForm, [H|T], Heads) ->
    insert_bform(BForm, T, [H|Heads]).

export_funs(FWithLines) ->
    [{attribute,Line,export,[{F,A} || {F,A} <- Functions]} || {Line, Functions} <- FWithLines].

insert_exports(Exports, [{attribute,_Line,module,_Mod} = Module|T], Acc) ->
    lists:reverse(Acc) ++ [Module|Exports] ++ T;
insert_exports(Exports, [Form|Forms], Acc) ->
    insert_exports(Exports, Forms, [Form|Acc]).

generate_forms(Line, {Module,Functions}) ->
    generate_forms(Line, {Module, false, Functions});
generate_forms(Line, {Module, ExtraArgs, Functions}) ->
    generate_forms(Line, {Module, ExtraArgs, undefined, Functions});
generate_forms(Line, {Module, true, ExtraCall, Functions}) ->
    generate_forms(Line, {Module, identity, ExtraCall, Functions});
generate_forms(Line, {Module, false, ExtraCall,  Functions}) ->
    generate_forms(Line, {Module, [], ExtraCall,  Functions});
generate_forms(Line, {Module, Monad, ExtraCall,  Functions}) when is_atom(Monad) ->
    generate_forms(Line, {Module, [{Module, Monad}], ExtraCall,  Functions});
generate_forms(Line, {Module, ExtraArgs, false,  Functions}) ->
    generate_forms(Line, {Module, ExtraArgs, undefined,  Functions});
generate_forms(Line, {Module, ExtraArgs, true,  Functions}) ->
    generate_forms(Line, {Module, ExtraArgs, {identity, run},  Functions});
generate_forms(Line, {Module, ExtraArgs, identity_run,  Functions}) ->
    generate_forms(Line, {Module, ExtraArgs, {identity, run},  Functions});
generate_forms(Line, {Module, ExtraArgs, ExtraCall, Functions}) ->
    GenFunctionsForms = 
        lists:map(
          fun({FName, Arity}) ->
                  gen_function(Module, FName, Arity, Line, ExtraArgs, ExtraCall)
          end, Functions),
    {Functions, GenFunctionsForms}.

insert_functions(Functions, [{eof, _Line} = EOF|T], Acc) ->
    lists:reverse(Acc) ++ Functions ++ [EOF|T];
insert_functions(Functions, [Form|Forms], Acc) ->
    insert_functions(Functions, Forms, [Form|Acc]).

gen_function(Module, FName, Arity, Line, ExtraArgs, ExtraCall) ->
    {function, Line, FName, Arity, 
     [{clause, Line, 
       lists:map(
         fun(N) ->
                 {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
         end, lists:seq(1, Arity))
       ,
       [],
       [gen_call(Module, FName, Arity, Line, ExtraArgs, ExtraCall)]}]}.

gen_call(Module, FName, Arity, Line, ExtraArgs, {Remote, Function}) ->
    {call, Line, {remote, Line, {atom, Line, Remote}, {atom, Line, Function}},
     [gen_call(Module, FName, Arity, Line, ExtraArgs, undefined)]};
gen_call(Module, FName, Arity, Line, ExtraArgs, undefined) ->
    Args = 
        lists:map(
          fun(ExtraArg) ->
                  from_value(Line, ExtraArg)
          end, ExtraArgs),
    {call, Line, {remote, Line, {atom, Line,  Module}, {atom, Line, FName}},
     lists:map(
       fun(N) ->
               {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
       end, lists:seq(1, Arity)) ++ Args
    }.    

from_value(Line, Tuple) when is_tuple(Tuple) ->
    {tuple, Line, lists:map(fun(Element) -> from_value(Line, Element) end, tuple_to_list(Tuple))};
from_value(Line, [H|T]) ->
    {cons, Line, from_value(Line, H), from_value(Line, T)};
from_value(Line, []) ->
    {nil, Line};
from_value(Line, Value) when is_atom(Value) ->
    {atom, Line, Value};
from_value(Line, Value) when is_integer(Value) ->
    {integer, Line, Value};
from_value(Line, Value) when is_float(Value) ->
    {float, Line, Value}.





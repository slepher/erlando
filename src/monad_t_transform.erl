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
    case ast_traverse:attributes_with_line(transform, Forms) of
        [] ->
            exit(no_transformers_defined);
        MFs ->
            {AllExports, AllFunctions} = 
                lists:foldl(
                  fun({Line, Transform}, {GAcc, FAcc}) ->
                          {GExports, GForms} = generate_forms(Line, Transform),
                          {[{Line, GExports}|GAcc], GForms ++ FAcc}
                  end, {[], []}, MFs),
            GenFunctionExports = export_funs(AllExports),
            NForms = insert_exports(GenFunctionExports, Forms, []),
            insert_functions(AllFunctions, NForms, [])
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
export_funs(FWithLines) ->
    [{attribute,Line,export,[{F,A} || {F,A} <- Functions]} || {Line, Functions} <- FWithLines].

insert_exports(Exports, [{attribute,_Line,module,_Mod} = Module|T], Acc) ->
    lists:reverse(Acc) ++ [Module|Exports] ++ T;
insert_exports(Exports, [Form|Forms], Acc) ->
    insert_exports(Exports, Forms, [Form|Acc]).


generate_forms(Line, {Module,Functions}) ->
    generate_forms(Line, {Module, false, Functions});
generate_forms(Line, {Module, ExtraArgs, Functions}) ->
    generate_forms(Line, {Module, undefined, ExtraArgs, Functions});
generate_forms(Line, {Module, ExtraCall, true, Functions}) ->
    generate_forms(Line, {Module, ExtraCall, [{Module, identity}], Functions});
generate_forms(Line, {Module, ExtraCall, false, Functions}) ->
    generate_forms(Line, {Module, ExtraCall, [], Functions});
generate_forms(Line, {Module, ExtraCall, ExtraArgs, Functions}) ->
    Exports = Module:module_info(exports),
    ArgsLen = length(ExtraArgs),
    GenFunctions = 
        lists:foldl(
          fun({FName, Arity}, Acc) ->
                  NArity = Arity + ArgsLen,
                  case lists:member({FName, NArity}, Exports) of
                      true ->
                          [{FName, Arity}|Acc];
                      false ->
                          exit(undefined_function, {Module, FName, NArity})
                  end
          end, [], Functions),
    GenFunctionsForms = 
        lists:map(
          fun({FName, Arity}) ->
                  gen_function(Module, FName, Arity, Line, ExtraCall, ExtraArgs)
          end, GenFunctions),
    {GenFunctions, GenFunctionsForms}.

insert_functions(Functions, [{eof, _Line} = EOF|T], Acc) ->
    lists:reverse(Acc) ++ Functions ++ [EOF|T];
insert_functions(Functions, [Form|Forms], Acc) ->
    insert_functions(Functions, Forms, [Form|Acc]).

gen_function(Module, FName, Arity, Line, ExtraCall, ExtraArgs) ->
    {function, Line, FName, Arity, 
     [{clause, Line, 
       lists:map(
         fun(N) ->
                 {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
         end, lists:seq(1, Arity))
       ,
       [],
       [gen_call(Module, FName, Arity, Line, ExtraCall, ExtraArgs)]}]}.

gen_call(Module, FName, Arity, Line, true, ExtraArgs) ->
    gen_call(Module, FName, Arity, Line, {identity, run}, ExtraArgs);
gen_call(Module, FName, Arity, Line, ExtraCall, true) ->
    gen_call(Module, FName, Arity, Line, ExtraCall, [{Module, identity}]);
gen_call(Module, FName, Arity, Line, {Remote, Function}, ExtraArgs) ->
    {call, Line, {remote, Line, {atom, Line, Remote}, {atom, Line, Function}},
     [gen_call(Module, FName, Arity, Line, undefined, ExtraArgs)]};
gen_call(Module, FName, Arity, Line, undefined, ExtraArgs) ->
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





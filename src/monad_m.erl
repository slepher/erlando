%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(monad_m).

%% API
-export([parse_transform/2]).
%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, Opts) ->
    C = parse_trans:initial_context(Forms, Opts),
    Module = parse_trans:do_inspect(fun inspect/4, undefined, Forms, C),
    Exports = Module:module_info(exports),
    Exclues = [new, list_to_atom("run_" ++ atom_to_list(Module)), Module, module_info, lift],
    GenFunctions = 
        lists:foldl(
          fun({FName, Arity}, Acc) ->
                  case lists:member(FName, Exclues) of
                      false ->
                          [{FName, Arity - 1}|Acc];
                      true ->
                          Acc
                  end
          end, [], Exports),
    GenFunctionExports = export_funs(GenFunctions),
    GenFunctionForms =
        lists:map(
          fun(N) ->
                  {FName, Arity} = lists:nth(N, GenFunctions),
                  gen_function(Module, FName, Arity, N)
          end, lists:seq(1, length(GenFunctions))),
    NForms = parse_trans:do_insert_forms(below, GenFunctionForms, Forms, C),
    parse_trans:do_insert_forms(above, GenFunctionExports, NForms, C).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
inspect(attribute, Form, _C, Acc) ->
    case attr_name(Form) of
        transformer ->
            Module = transformer(Form),
            {false, Module};
        _ ->
            {false, Acc}
    end;
inspect(_, _, _, Acc) ->
    {false, Acc}.

transformer(Form) ->
    erl_syntax:atom_value(hd(erl_syntax:attribute_arguments(Form))).

attr_name(F) ->
    erl_syntax:atom_value(erl_syntax:attribute_name(F)).

gen_function(Module, FName, Arity, Line) ->
    {function, Line, FName, Arity, 
     [{clause, Line, 
       lists:map(
         fun(N) ->
                 {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
         end, lists:seq(1, Arity))
       ,
       [],
       [{call, 1, {remote, Line, {atom, Line,  Module}, {atom, Line, FName}},
        lists:map(
          fun(N) ->
                  {var, Line, list_to_atom("Args" ++ integer_to_list(N))}
          end, lists:seq(1, Arity)) ++ [{tuple, Line, [{atom, Line, Module}, {atom, Line, identity_m}]}]
        }]}]}.
     
export_funs(Functions) ->
    [{attribute,1,export,[{F,A} || {F,A} <- Functions]}].

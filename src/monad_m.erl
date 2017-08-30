%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 25 Jul 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_m).

%% API
-export([parse_transform/2]).
%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Opts) ->
    case ast_traverse:map_reduce(fun transformer/2, ok, Forms) of
        {error, Module} ->
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
            NForms = insert_exports(GenFunctionExports, Forms, []),
            insert_functions(GenFunctionForms, NForms, []);
        {ok, _} ->
            Forms
    end.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% Internal functions
%%%===================================================================
insert_exports(Exports, [{attribute,_Line,module,_Mod} = Module|T], Acc) ->
    lists:reverse(Acc) ++ [Module|Exports] ++ T;
insert_exports(Exports, [Form|Forms], Acc) ->
    insert_exports(Exports, Forms, [Form|Acc]).

insert_functions(Functions, [{eof, _Line} = EOF|T], Acc) ->
    lists:reverse(Acc) ++ Functions ++ [EOF|T];
insert_functions(Functions, [Form|Forms], Acc) ->
    insert_functions(Functions, Forms, [Form|Acc]).

transformer({attribute,_Line,transformer, Transformer}, _Acc) ->
    {error, Transformer};
transformer(Other, Acc) ->
    {ok, {Other, Acc}}.

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

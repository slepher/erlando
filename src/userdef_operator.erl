%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(userdef_operator).

%% API
-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================

parse_transform(Forms, _Opts) ->
    ParseOps = parse_ops(Forms),
    ast_traverse:map(fun(Type, Node) -> walk(Type, Node, ParseOps) end, Forms).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_ops(Forms) ->
    Overloads = ast_traverse:attributes(overloads, Forms),
    lists:foldl(
      fun(Overload, Acc0) ->
              lists:foldl(
                fun({Module, Ops}, Acc1) when is_list(Ops) ->
                        lists:foldl(
                          fun(Op, Acc2) ->
                                  maps:put(Op, Module, Acc2)
                          end, Acc1, Ops);
                   ({Module, Op}, Acc1) when is_atom(Op) ->
                        maps:put(Op, Module, Acc1)
                end, Acc0, Overload)
      end, maps:new(), Overloads).

walk(pre, {op, Line ,'/', {op, Line1, '/', A , {atom, _Line2, Op} = OpFun}, B} = Node, Ops) ->
    case maps:find(Op, Ops) of
        {ok, Module} ->
            {call, Line, {remote, Line1, {atom, Line1, Module}, OpFun}, [A, B]};
        error ->
            Node
    end;
walk(_Type, Node, _Ops) ->
    Node.

%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(overload_op).

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
    lists:flatten(Overloads).

walk(pre, {op, Line ,'/', {op, _Line1, '/', A , {atom, _Line2, Op} = OpFun}, B} = Node, Ops) ->
    case lists:member(Op, Ops) of
        true ->
            {call, Line, OpFun, [A, B]};
        false ->
            Node
    end;
walk(_Type, Node, _Ops) ->
    Node.

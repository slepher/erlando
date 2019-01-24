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

-include_lib("astranaut/include/quote.hrl").

-export([parse_transform/2]).

%%%===================================================================
%%% API
%%%===================================================================

parse_transform(Forms, _Opts) ->
    ParseOps = parse_ops(Forms),
    astranaut_traverse:map(fun(Node, Attr) -> walk(Node, Attr, ParseOps) end, Forms, #{traverse => pre}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

parse_ops(Forms) ->
    Overloads = astranaut:attributes(overloads, Forms),
    lists:foldl(
      fun(Overload, Acc0) ->
              lists:foldl(
                fun({Module, Ops}, Acc1) ->
                        add_op(Module, Ops, Acc1);
                   (Ops, Acc1) ->
                        add_op(undefined, Ops, Acc1)
                end, Acc0, Overload)
      end, maps:new(), Overloads).

add_op(Module, Ops, Map) when is_list(Ops) ->
    lists:foldl(
      fun(Op, Acc) ->
              maps:put(Op, Module, Acc)
      end, Map, Ops);
add_op(Module, Op, Map) when is_atom(Op) ->
    add_op(Module, [Op], Map).

walk({op, Line ,'/', {op, _Line1, '/', A , {atom, _Line2, Op} = OpFun}, B} = Node, _Attr, Ops) ->
    case maps:find(Op, Ops) of
        {ok, undefined} ->
            quote(_@OpFun(unquote(A), unquote(B)), Line);
        {ok, Module} ->
            quote(_A@Module:_@OpFun(unquote(A), unquote(B)), Line);
        %{call, Line, {remote, Line1, {atom, Line1, Module}, OpFun}, [A, B]};
        error ->
            Node
    end;
walk(Node, _Attr, _Ops) ->
    Node.

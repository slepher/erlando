%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(either).

-compile({parse_transform, cut}).

%% API
-export([either/2, either/3]).
-export([swap/0]).
-export([left/0, left/1, right/0, right/1]).

%%%===================================================================
%%% API
%%%===================================================================

either(FAC, FBC) ->
    either(FAC, FBC, _).

either(FAC, FBC, EAB) ->
    case EAB of
        {left, A} ->
            FAC(A);
        {right, B} ->
            FBC(B)
    end.

swap() ->
    either(right(), left()).

left() ->
    left(_).

left(A) ->
    {left, A}.

right() ->
    right(_).

right(B) ->
    {right, B}.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

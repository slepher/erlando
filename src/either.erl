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

-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).

%% API
-export([type/0]).
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([either/2, either/3]).
-export([swap/0]).
-export([left/0, left/1, right/0, right/1]).

%%%===================================================================
%%% API
%%%===================================================================
type() ->
    type:default_type(?MODULE).

fmap(_, {left, L}) ->
    {left, L};
fmap(F, {right, R}) ->
    {right, F(R)}.

'<$'(FB, FA) ->
    functor:'default_<$'(FB, FA, ?MODULE).

pure(A) ->
    {right, A}.

'<*>'({left, L}, _) ->
    {left, L};
'<*>'(_, {left, L}) ->
    {left, L};
'<*>'({right, F}, {right, A}) ->
    {right, F(A)}.

lift_a2(FAB, AA, AB) ->
    applicative:default_lift_a2(FAB, AA, AB, ?MODULE).

'*>'(AA, AB) ->
    applicative:'default_*>'(AA, AB, ?MODULE).

'<*'(AA, AB) ->
    applicative:'default_<*'(AA, AB, ?MODULE).

'>>='({left, L}, _) ->
    {left, L};
'>>='({right, R}, KAMB) ->
    KAMB(R).

'>>'(MA, MB) ->
    monad:'default_>>'(MA, MB, ?MODULE).

return(A) ->
    monad:default_return(A, ?MODULE).

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

%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(either).

-erlando_type(?MODULE).

-compile({parse_transform, cut}).
-compile({parse_transform, monad_t_transform}).

-define(TYPE, ?MODULE).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).

%% API
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([fail/2]).
-export([either/2, either/3]).
-export([swap/0]).
-export([left/0, left/1, right/0, right/1]).

-transform(#{args => [?TYPE], behaviours => [functor, applicative, monad, monad_fail]}).

%%%===================================================================
%%% API
%%%===================================================================
fmap(_, {left, L}, ?TYPE) ->
    {left, L};
fmap(F, {right, R}, ?TYPE) ->
    {right, F(R)}.

'<$'(FB, FA, ?TYPE) ->
    functor:'default_<$'(FB, FA, ?TYPE).

pure(A, ?TYPE) ->
    {right, A}.

'<*>'({left, L}, _, ?TYPE) ->
    {left, L};
'<*>'(_, {left, L}, ?TYPE) ->
    {left, L};
'<*>'({right, F}, {right, A}, ?TYPE) ->
    {right, F(A)}.

lift_a2(FAB, AA, AB, ?TYPE) ->
    applicative:default_lift_a2(FAB, AA, AB, ?TYPE).

'*>'(AA, AB, ?TYPE) ->
    applicative:'default_*>'(AA, AB, ?TYPE).

'<*'(AA, AB, ?TYPE) ->
    applicative:'default_<*'(AA, AB, ?TYPE).

'>>='({left, L}, _, ?TYPE) ->
    {left, L};
'>>='({right, R}, KAMB, ?TYPE) ->
    KAMB(R).

'>>'(MA, MB, ?TYPE) ->
    monad:'default_>>'(MA, MB, ?TYPE).

return(A, ?TYPE) ->
    monad:default_return(A, ?TYPE).

fail(E, ?TYPE) ->
    {left, E}.

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

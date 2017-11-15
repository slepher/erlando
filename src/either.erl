%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(either).

-erlando_type({?MODULE, [{left, '_'}, {right, '_'}]}).

-compile({parse_transform, cut}).
-compile({parse_transform, monad_t_transform}).

-define(TYPE, ?MODULE).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_runner).

-include("erlando.hrl").

%% API
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([fail/1]).
-export([run_nargs/0, run_m/2]).
-export([run/1]).
-export([either/2, either/3]).
-export([swap/0]).
-export([left/0, left/1, right/0, right/1]).

-transform(#{patterns => [?TYPE], gbehaviours => [functor, applicative, monad, monad_fail]}).

%%%===================================================================
%%% API
%%%===================================================================
fmap(_, {left, L}) ->
    {left, L};
fmap(F, {right, R}) ->
    {right, F(R)}.

'<$'(FB, FA) ->
    functor:'default_<$'(FB, FA, ?TYPE).

pure(A) ->
    {right, A}.

'<*>'({left, L}, _) ->
    {left, L};
'<*>'(_, {left, L}) ->
    {left, L};
'<*>'({right, F}, {right, A}) ->
    {right, F(A)}.

lift_a2(FAB, AA, AB) ->
    applicative:default_lift_a2(FAB, AA, AB, ?TYPE).

'*>'(AA, AB) ->
    applicative:'default_*>'(AA, AB, ?TYPE).

'<*'(AA, AB) ->
    applicative:'default_<*'(AA, AB, ?TYPE).

'>>='({left, L}, _) ->
    {left, L};
'>>='({right, R}, KAMB) ->
    KAMB(R).

'>>'(MA, MB) ->
    monad:'default_>>'(MA, MB, ?TYPE).

return(A) ->
    monad:default_return(A, ?TYPE).

fail(E) ->
    {left, E}.

run_nargs() ->
    0.

run_m(EA, []) ->
    run(EA).

run(#undetermined{} = UEA) ->
    undetermined:run(UEA, either);
run(Either) ->
    Either.

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

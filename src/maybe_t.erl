%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 14 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(maybe_t).
-compile({parse_transform, do}).

-define(MAYBE_T_MONAD, {?MODULE, monad}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_trans).

-behaviour(monad_plus_trans).

-export_type([maybe_t/2]).

-opaque maybe_t(M, A) :: {maybe_t, inner_maybe_t(M, A)}.
-type inner_maybe_t(M, A) :: monad:monadic(M, maybe_m:maybe(A)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).


-export([fmap/2]).
-export(['<*>'/2, pure/1]).
-export(['>>='/2, return/1]).
-export([fail/1]).
-export([lift/1]).

%% API
-export([new/1, maybe_t/1, run_maybe_t/1]).
%% impl of functor
-export([fmap/3]).
%% impl of monad
-export(['>>='/3, return/2, fail/2]).
%% impl of monad transformer
-export([lift/2]).
%% impl of monad plus
-export([mzero/1, mplus/3]).
-export([run_maybe/2, map_maybe/3]).

fmap(F, MTA) ->
    map_maybe(
      fun(IA) ->
              functor:fmap(F, IA)
      end, MTA).

'<*>'(MTF, MTA) ->
    maybe_t(
      do([monad ||
             AF <- run_maybe_t(MTF),
             case AF of
                 {just, NAF} ->
                     do([monad ||
                            AA <- run_maybe_t(MTA),
                            case AA of
                                {just, NAA} ->
                                    return({just, applicative:'<*>'(NAF, NAA)});
                                nothing ->
                                    return(nothing)
                            end
                        ]);
                 nothing ->
                     return(nothing)
             end
         ])).

pure(A) ->
    return(A).

'>>='(MTA, K) ->
    '>>='(MTA, K, ?MAYBE_T_MONAD).

return(A) ->
    return(A, ?MAYBE_T_MONAD).

fail(E) ->
    fail(E, ?MAYBE_T_MONAD).

lift(X) ->
    maybe_t(functor:fmap(fun(A) -> {just, A} end, X)).

-spec new(M) -> t(M).
new(IM) ->
    {?MODULE, IM}.

-spec maybe_t(inner_maybe_t(M, A)) -> maybe_t(M, A).
maybe_t(Inner) ->
    {?MODULE, Inner}.

-spec run_maybe_t(maybe_t(M, A)) -> inner_maybe_t(M, A).
run_maybe_t({?MODULE, Inner}) ->
    Inner;
run_maybe_t(Other) ->
    exit({invalid_maybe_t, Other}).

-spec fmap(fun((A) -> B), maybe_t(M, A), t(M)) -> maybe_t(M, B).
fmap(F, X, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             M <- X,
             maybe_m:fmap(F, M)
         ])).

-spec '>>='(maybe_t(M, A), fun((A) -> maybe_t(M, B)), t(M)) -> maybe_t(M, B).
'>>='(X, Fun, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             M <- X,
             case M of
                 nothing ->
                     return(nothing);
                 {just, V} ->
                     run_maybe_t(Fun(V))
             end
         ])).

-spec return(A, t(M)) -> maybe_t(M, A).
return(A, {?MODULE, IM}) ->
    maybe_t(monad:return(IM, maybe_m:return(A))).

-spec fail(_E, t(M)) -> maybe_t(M, _A).
fail(E, {?MODULE, IM}) ->
    maybe_t(monad:return(IM, maybe_m:fail(E))).

-spec lift(monad:monadic(M, A), t(M)) -> maybe_t(M, A).
lift(X, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             M <- X,
             return(maybe_m:return(M))
         ])).

-spec mzero(t(M)) -> maybe_t(M, _A).
mzero({?MODULE, IM}) ->
    maybe_t(monad:return(IM, maybe_m:mzero())).

-spec mplus(maybe_t(M, A), maybe_t(M, A), t(M)) -> maybe_t(M, A).
mplus(MA, MB, {?MODULE, IM}) ->
    maybe_t(
      do([IM ||
             A <- run_maybe_t(MA),
             %% could be 
             %% A <- run_maybe_t(MA),
             %% B <- run_maybe_t(MB)
             %% return(maybe_m:mplus(A, B))
             %% but erlang is not lazy as haskell
             %% I have to expand it
             case A of
                 nothing ->
                     run_maybe_t(MB);
                 {just, _V} ->
                     return(A)
             end
       ])).



-spec run_maybe(maybe_t(M, A), t(M)) -> inner_maybe_t(M, A).
run_maybe(X, {?MODULE, _IM}) ->
    run_maybe_t(X).

-spec map_maybe(fun((monad:monadic(M, A)) -> monad:monadic(N, B)), maybe_t(M, A)) -> maybe_t(N, B).
map_maybe(F, X) ->
    maybe_t(F(run_maybe_t(X))).

-spec map_maybe(fun((monad:monadic(M, A)) -> monad:monadic(N, B)), maybe_t(M, A), t(M)) -> maybe_t(N, B).
map_maybe(F, X, {?MODULE, _IM}) ->
    map_maybe(F, X).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(list_t).

-export_type([list_t/2]).
-behaviour(monad_trans).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).

-opaque list_t(M, A) :: {list_t, inner_list_t(M, A)}.
-type inner_list_t(M, A) :: monad:monadic(M, step(M, A)).
-type step(M, A) :: [A|list_t(M, A)] | [].

-type t(IM) :: monad_trans:monad_trans(?MODULE, IM).

%% API
-export([new/1, list_t/1, run_list_t/1]).
-export([fmap/3]).
-export([mappend/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([run_list/2]).

-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec list_t(inner_list_t(M, A)) -> list_t(M, A).
list_t(Inner) ->
    {?MODULE, Inner}.

-spec run_list_t(list_t(M, A)) -> inner_list_t(M, A).
run_list_t({?MODULE, Inner}) ->
    Inner;
run_list_t(Other) ->
    exit({invalid_list_t, Other}).

%%%===================================================================
%%% API
%%%===================================================================
-spec fmap(fun((A) -> B), list_t(M, A), t(M)) -> list_t(M, B).
fmap(F, X, {?MODULE, IM} = AT) ->
    list_t(
      do([IM ||
             V <- run_list_t(X),
             case V of
                 [] ->
                     return([]);
                 [H|T] ->
                     return([F(H)|fmap(F, T, AT)])
             end
         ])).

-spec mappend(list_t(M, A), list_t(M, A), t(M)) -> list_t(M, A).
mappend(X1, X2, {?MODULE, IM} = LT) ->
    list_t(
      do([IM ||
             V <- run_list_t(X1),
             case V of
                 [] ->
                     run_list_t(X2);
                 [H|T] ->
                     return([H|mappend(T, X2, LT)])
             end
         ])).

-spec '>>='(list_t(M, A), fun((A) -> list_t(M, B)), t(M)) -> list_t(M, B).
'>>='(X, Fun, {?MODULE, IM} = LT) ->
    list_t(
      do([IM || 
             Values <- run_list_t(X),
             case Values of
                 [] ->
                     return([]);
                 [H|T] ->
                     run_list_t(mappend(Fun(H), '>>='(T, Fun, LT), LT))
             end
         ])).
           
-spec return(A, t(M)) -> list_t(M, A).
return(A, {?MODULE, IM}) ->
    list_t(IM:return([A])).

-spec fail(_E, t(M)) -> list_t(M, _A).
fail(E, {?MODULE, IM}) ->
    list_t(IM:fail(E)).

-spec lift(monad:monadic(M, A), t(M)) -> monad:monaic(t(M), A).
lift(X, {?MODULE, IM}) ->
    list_t(
      do([IM || 
             A <- X,
             return([A])
         ])).

-spec run_list(list_t(M, A), monad_trans:monad_trans(list_t, M)) ->
                      monad:monadic(M, A) when M :: monad:monadic().
run_list(X, {?MODULE, IM} = LT) ->
    do([IM ||
           V <- run_list_t(X),
           case V of
               [] ->
                   return([]);
               [H|MT] ->
                   monad:fmap(IM, fun(T) -> [H|T] end, run_list(MT, LT))
           end
       ]).
           
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

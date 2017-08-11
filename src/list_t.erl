%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(list_t).

-export_type([list_t/2]).
-behaviour(monad_trans).
-compile({parse_transform, do}).
-compile({parse_transform, cut}).


-opaque list_t(M, A) :: {list_t, inner_list_t(M, A)}.
-type inner_list_t(M, A) :: monad:monadic(M, [A]).

-type t(IM) :: {?MODULE, IM}.
%% API
-export([new/1, list_t/1, run_list_t/1]).
-export([fmap/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([run_list/2, map_list/3]).

-spec new(M) -> t(M) when M :: monad:monadic(M).
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
'>>='(X, Fun, {?MODULE, IM}) ->
    list_t(
      do([IM || 
             Values <- run_list_t(X),
             lists:foldl(
               fun(Value, IMAcc) ->
                       do([IM || 
                              Acc <- IMAcc,
                              NValues <- run_list_t(Fun(Value)),
                              return(NValues ++ Acc)
                          ])
               end, IM:return([]), Values)
         ])).
           
return(A, {?MODULE, IM}) ->
    list_t(IM:return([A])).

fail(E, {?MODULE, IM}) ->
    list_t(IM:fail(E)).

lift(X, {?MODULE, IM}) ->
    list_t(
      do([IM || 
             A <- X,
             return([A])
         ])).

-spec fmap(fun((A) -> B), list_t(M, A), t(M)) -> list_t(M, B).
fmap(F, X, {?MODULE, IM} = LT) ->
    map_list(IM:fmap(lists:map(F, _), _), X, LT).

run_list(X, {?MODULE, _IM}) ->
    run_list_t(X).

-spec map_list(fun((monad:monadic(M, [A])) -> monad:monadic(N, [B])), list_t(M, A), t(M)) -> list_t(N, B).
map_list(F, X, {?MODULE, _IM}) ->
    list_t(F(run_list_t(X))).


           
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

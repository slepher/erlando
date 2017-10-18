%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(function_instance).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_reader).
-behaviour(monad_runner).

%% API
% functor instance.
-export([fmap/2, '<$'/2]).
% applicative instance.
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
% monad instance.
-export(['>>='/2, return/1]).
% monad reader instance.
-export([ask/0, reader/1, local/2]).
% monad runner instance.
-export([run_nargs/0, run/2]).

-export(['.'/2]).
-export([const/1]).
-export([id/0, id/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec fmap(fun((A) -> B), fun((R) -> A)) -> fun((R) -> B).
fmap(F, FA) ->
    '.'(F, FA).

-spec '<$'(B, fun((R) -> _A)) -> fun((R) -> B).
'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?MODULE).

-spec '<*>'(fun((R) -> fun((A) -> B)), fun((R) -> A)) -> fun((R) -> B).
'<*>'(FF, FA) ->
    fun(R) -> (FF(R))(FA(R)) end.

-spec pure(A) -> fun((_R) -> A).
pure(A) ->
    const(A).

-spec lift_a2(fun((A, B) -> C), fun((R) -> A), fun((R) -> B)) -> fun((R) -> C).
lift_a2(F, RTA, RTB) ->
    applicative:default_lift_a2(F, RTA, RTB, ?MODULE).

-spec '*>'(fun((R) -> _A), fun((R) -> B)) -> fun((R) -> B).
'*>'(RTA, RTB) ->
    applicative:'default_*>'(RTA, RTB, ?MODULE).

-spec '<*'(fun((R) -> A), fun((R) -> _B)) -> fun((R) -> A).
'<*'(RTA, RTB) ->
    applicative:'default_<*'(RTA, RTB, ?MODULE).
           
-spec '>>='(fun((R) -> A), fun((A) -> fun((R) -> B))) -> fun((R) -> B).  
'>>='(FA, KFB) ->
    fun(X) -> (KFB(FA(X)))(X) end.

-spec return(A) -> fun((_R) -> A).
return(A) ->
    pure(A).

ask() ->
    id().

reader(F) ->
    id(F).

local(F, FI) ->
    '.'(FI, F).

run_nargs() ->
    1.

run(F, [A]) ->
    F(A).

'.'(F, G) ->
    fun(X) -> F(G(X)) end.

const(A) -> fun(_R) -> A end.

id() ->
    fun(A) -> A end.

id(A) -> A.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

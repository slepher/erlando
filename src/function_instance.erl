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

%% API
% functor instance.
-export([fmap/2]).
% applicative instance.
-export(['<*>'/2, pure/1]).
% monad instance.
-export(['>>='/2, return/1]).
% monad reader instance.
-export([ask/0, reader/1, local/2]).

-export(['.'/2]).
-export([const/1]).
-export([id/0, id/1]).

%%%===================================================================
%%% API
%%%===================================================================
fmap(F, G) ->
    '.'(F, G).

'<*>'(F, G) ->
    fun(X) -> (F(X))(G(X)) end.

pure(A) ->
    const(A).
             
'>>='(F, K) ->
    fun(X) -> (K(F(X)))(X) end.

return(A) ->
    const(A).

ask() ->
    id().

reader(F) ->
    id(F).

local(F, FI) ->
    '.'(FI, F).

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
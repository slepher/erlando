%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_fail).

%% API
-export([fail/1, fail/2]).

-callback fail(any()) -> monad:monadic(M, _A) when M :: monad:monad().

%%%===================================================================
%%% API
%%%===================================================================

fail(E) ->
    undetermined:lift(fun(M) -> fail(E, M) end).

-spec fail(M, _E) -> monad:monadic(M, _A) when M :: monad:monad().
fail(E, {T, _IM} = M) ->
    T:fail(E, M);
fail(E, M) ->
    M:fail(E).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

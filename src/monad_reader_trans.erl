%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 31 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_reader_trans).

%% API
-export([]).

-callback ask(M) -> monad:monadic(M, _R) when M :: monad:monad().
-callback local(fun((R) -> R), monad:monadic(M, R), M) -> monad:monadic(M, R) when M :: monad:monad().
-callback reader(fun((_R) -> A), M) -> monad:monadic(M, A) when M :: monad:monad().

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

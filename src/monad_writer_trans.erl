%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_writer_trans).

-callback writer({A, [_W]}, M) -> monad:monadic(M, A).
-callback tell([_W], M) -> monad:monadic(M, _A).
-callback listen(monad:monadic(M, A), M) -> monad:monadic(M, {A, [_W]}).
-callback pass(monad:monadic(M, {A, fun(([W]) -> [W])}), M) -> monad:monadic(M, A). 

%% API
-export([]).

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

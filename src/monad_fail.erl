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
-export([fail/1]).
-export([fail/2]).

-callback fail(any()) -> monad:monadic(M, _A) when M :: monad:monad().

%%%===================================================================
%%% API
%%%===================================================================

fail(E) ->
    undetermined:new(fun(MonadFail) -> fail(E, MonadFail) end).

fail(E, MonadFail) ->
    monad_trans:apply_fun(fail, [E], MonadFail).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

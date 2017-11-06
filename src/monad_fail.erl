%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_fail).

-superclass([monad]).

-callback fail(any(), M) -> monad:monadic(M, _A) when M :: monad:monad().

%% API
-export([fail/1]).
-export([fail/2]).

%%%===================================================================
%%% API
%%%===================================================================

fail(E) ->
    undetermined:new(fun(MonadFail) -> fail(E, MonadFail) end).

fail(E, monad) ->
    fail(E);
fail(E, MonadFail) ->
    typeclass_trans:apply(fail, [E], MonadFail, ?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

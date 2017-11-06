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
-export([fail/2]).

-transform({?MODULE, [?MODULE], [fail/1]}).

%%%===================================================================
%%% API
%%%===================================================================
fail(E, UMonadFail) ->
    undetermined:new(
      fun(MonadFail) ->
              typeclass_trans:apply(fail, [E], MonadFail, ?MODULE)
      end, UMonadFail).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

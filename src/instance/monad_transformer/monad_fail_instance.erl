%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_fail_instance).

-include("erlando_instance.hrl").

-erlando_instance(
   #{types => [state_t, cont_t, reader_t, writer_t],
     capability => monad_fail,
     implementation => generic}).

%% API
-export([fail/2]).

%%%===================================================================
%%% API
%%%===================================================================
fail(E, MonadTrans) when is_atom(MonadTrans) ->
    fail(E, {MonadTrans, monad_fail});
fail(E, {MonadTrans, MonadFail}) ->
    monad_trans:lift(monad_fail:fail(E, MonadFail), {MonadTrans, MonadFail}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

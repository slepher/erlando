%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  3 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_state_instance).

-erlando_type([reader_t, writer_t, cont_t, maybe_t, error_t]).

-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-behaviour(monad_state).

%% API
-export([get/1, put/2, state/2]).

%%%===================================================================
%%% API
%%%===================================================================
get(MonadTrans) when is_atom(MonadTrans) ->
    get({MonadTrans, monad_state});
get({MonadTrans, MonadState}) ->
    monad_trans:lift(monad_state:get(MonadState), {MonadTrans, MonadState}).

put(S, MonadTrans) when is_atom(MonadTrans) ->
    put(S, {MonadTrans, monad_state});
put(S, {MonadTrans, MonadState}) ->
    monad_trans:lift(monad_state:put(S, MonadState), {MonadTrans, MonadState}).

state(F, MonadTrans) when is_atom(MonadTrans) ->
    state(F, {MonadTrans, monad_state});
state(F, {MonadTrans, MonadState}) ->
    monad_trans:lift(monad_state:state(F, MonadState), {MonadTrans, MonadState}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

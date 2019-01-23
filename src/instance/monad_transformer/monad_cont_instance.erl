%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_cont_instance).

-erlando_type([reader_t, writer_t, state_t, maybe_t, error_t]).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).

-behaviour(monad_cont).

%% API
-export([callCC/2]).

%%%===================================================================
%%% API
%%%===================================================================
callCC(F, {MonadTrans, MonadCont}) ->
    CallCC = monad_cont:callCC(_, MonadCont),
    lift_callCC(CallCC, F, {MonadTrans, MonadCont});
callCC(F, MonadTrans) ->
    callCC(F, {MonadTrans, monad_cont}).

lift_callCC(CallCC, F, {maybe_t, _MonadCont}) ->
    maybe_t:maybe_t(
      CallCC(
        fun(CC) ->
                maybe_t:run_maybe_t(F(fun(A) -> maybe_t:maybe_t(CC(monad:return(A, maybe))) end))
        end));
lift_callCC(CallCC, F, {error_t, _MonadCont}) ->
    error_t:error_t(
      CallCC(
        fun(CC) ->
                error_t:run_error_t(F(fun(A) -> error_t:error_t(CC(monad:return(A, error))) end))
        end));

lift_callCC(CallCC, F, {reader_t, _MonadCont}) ->
    reader_t:reader_t(
      fun(R) ->
              CallCC(
                fun(CC) ->
                        reader_t:run(reader_t:reader_t(fun(A) -> F(fun(_) -> CC(A) end) end), R)
                end)
      end);
lift_callCC(CallCC, F, {state_t, _MonadCont}) ->
    state_t:state_t(
      fun(S) ->
              CallCC(
                fun(CC) ->
                        state_t:run(F(fun(A) -> state_t:state_t(fun(_)  -> CC({A, S}) end) end), S)
                end)
      end);
lift_callCC(CallCC, F, MonadTrans) when is_atom(MonadTrans) ->
    lift_callCC(CallCC, F, {MonadTrans, monad_cont}).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

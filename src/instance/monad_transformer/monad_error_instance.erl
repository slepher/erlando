%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_error_instance).

-erlando_type([maybe_t, state_t, reader_t, writer_t]).

-behaviour(monad_error).

%% API
-export([throw_error/2, catch_error/3]).

%%%===================================================================
%%% API
%%%===================================================================
throw_error(E, MonadTrans) when is_atom(MonadTrans) ->
    throw_error(E, {MonadTrans, monad_error});
throw_error(E, {MonadTrans, MonadError}) ->
    monad_trans:lift(monad_error:throw_error(E, MonadError), {MonadTrans, MonadError}).

catch_error(MA, EMB, MonadTrans) when is_atom(MonadTrans) ->
    catch_error(MA, EMB, {MonadTrans, monad_error});
catch_error(MA, EMB, {MonadTrans, MonadError}) ->
    lift_catch(MA, EMB, {MonadTrans, MonadError}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
lift_catch(MA, EMB, {reader_t, _MonadError} = ReaderT) ->
    reader_t_lift_catch(MA, EMB, ReaderT);
lift_catch(MA, EMB, {state_t,  _MonadError} = StateT) ->
    state_t_lift_catch(MA, EMB, StateT);
lift_catch(MA, EMB, {writer_t, _MonadError} = WriterT) ->
    writer_t_lift_catch(MA, EMB, WriterT);
lift_catch(MA, EMB, {maybe_t,  _MonadError} = MaybeT) ->
    maybe_t_lift_catch(MA, EMB, MaybeT).

reader_t_lift_catch(MA, EMB, {reader_t, MonadError} = ReaderT) ->
    reader_t:reader_t(
      fun(R) ->
              MEA = reader_t:run(MA, R, ReaderT),
              EMEB = 
                  fun(E) ->
                          MB = EMB(E),
                          reader_t:run(MB, R, ReaderT)
                  end,
              monad_error:catch_error(MEA, EMEB, MonadError)
      end).

state_t_lift_catch(MA, EMB, {state_t, MonadError} = StateT) ->
   state_t:state_t(
     fun(S) ->
             MEA = state_t:run(MA, S, StateT),
             EMEB = fun(E) -> state_t:run(EMB(E), S, StateT) end,
             monad_error:catch_error(MEA, EMEB, MonadError)
     end).

writer_t_lift_catch(MA, EMB, {writer_t, MonadError} = WriterT) ->
    MEA = writer_t:run(MA, WriterT),
    EMEB = fun(E) -> writer_t:run(EMB(E), WriterT) end,
    writer_t:writer_t(monad_error:catch_error(MEA, EMEB, MonadError)).

maybe_t_lift_catch(MA, EMB, {maybe_t, MonadError} = MaybeT) ->
    MEA = maybe_t:run(MA, MaybeT),
    EMEB = fun(E) -> maybe_t:run(EMB(E), MaybeT) end,
    maybe_t:maybe_t(monad_error:catch_error(MEA, EMEB, MonadError)).

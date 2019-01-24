%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_writer_instance).

-erlando_type([state_t, reader_t, maybe_t, error_t]).

-include("do.hrl").

-compile({parse_transform, cut}).

-behaviour(monad_writer).

%% API
-export([writer/2, tell/2, listen/2, pass/2]).
-export([lift_listen/3, lift_pass/3]).

%%%===================================================================
%%% API
%%%===================================================================

writer({A, W}, {MonadTrans, MonadWriter}) ->
    monad_trans:lift(monad_writer:writer({A, W}, MonadWriter), {MonadTrans, MonadWriter});
writer({A, W}, MonadTrans) ->
    writer({A, W}, {MonadTrans, monad_writer}).

tell(W, {MonadTrans, MonadWriter}) ->
    monad_trans:lift(monad_writer:tell(W, MonadWriter), {MonadTrans, MonadWriter});
tell(W, MonadTrans) ->
    tell(W, {MonadTrans, monad_writer}).

listen(MWA, {MonadTrans, MonadWriter}) ->
    lift_listen(monad_writer:listen(_, MonadWriter), MWA, {MonadTrans, MonadWriter});
listen(MWA, MonadTrans) ->
    listen(MWA, {MonadTrans, monad_writer}).

pass(MWAF, {MonadTrans, MonadWriter}) ->
    Pass = monad_writer:pass(_, MonadWriter),
    lift_pass(Pass, MWAF, {MonadTrans, MonadWriter});
pass(MWAF, MonadTrans) ->
    pass(MWAF, {MonadTrans, monad_writer}).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
lift_listen(Listen, MTWMA, {maybe_t, MonadWriter}) ->
    maybe_t:map(
      fun(WMA) ->
              do([MonadWriter || 
                     {MA, W} <- Listen(WMA),
                     return(functor:fmap(fun(A) -> {A, W} end, MA, maybe))
                 ])
      end, MTWMA);
lift_listen(Listen, ETWMA, {error_t, MonadWriter}) ->
    error_t:map(
      fun(WMA) ->
              do([MonadWriter || 
                     {EA, W} <- Listen(WMA),
                     return(functor:fmap(fun(A) -> {A, W} end, EA, either))
                 ])
      end, ETWMA);
lift_listen(Listen, RTWMA, {reader_t, _MonadWriter}) ->
    reader_t:map(Listen, RTWMA);
lift_listen(Listen, STWMA, {state_t, MonadWriter}) ->
    state_t:map(
      fun(WMA) ->
              do([MonadWriter ||
                     {{A, S}, W} <- Listen(WMA),
                     return({{A, W}, S})
                 ])
      end, STWMA);
lift_listen(Listen, WMA, MonadTrans) when is_atom(MonadTrans) ->
    lift_listen(Listen, WMA, {MonadTrans, monad_writer}).

lift_pass(Pass, MTWMAF, {maybe_t, MonadWriter}) ->
    maybe_t:map(
      fun(WMAF) ->
              Pass(
                do([MonadWriter ||
                       MAF <- WMAF,
                       return(
                         case MAF of
                             {just, {A, F}} ->
                                 {{just, A}, F};
                             nothing ->
                                 {nothing, function_instance:id()}
                         end)
                   ])
               )
      end, MTWMAF);
lift_pass(Pass, ETWMAF, {error_t, MonadWriter}) ->
    error_t:map(
      fun(WMAF) ->
              Pass(
                do([MonadWriter ||
                       EAF <- WMAF,
                       return(
                         case EAF of
                             {right, {A, F}} ->
                                 {{right, A}, F};
                             {left, Reason} ->
                                 {{left, Reason}, function_instance:id()}
                         end)
                   ])
               )
      end, ETWMAF);
lift_pass(Pass, RTWMAF, {reader_t, _MonadWriter}) ->
    reader_t:map(Pass, RTWMAF);
lift_pass(Pass, STWMAF, {state_t, MonadWriter}) ->
    state_t:map(
      fun(WMAF) ->
              Pass(
                do([MonadWriter ||
                       {{A, F}, S} <- WMAF,
                       return({{A, S}, F})
                   ])
                )
      end, STWMAF);
lift_pass(Listen, WMAF, MonadTrans) when is_atom(MonadTrans) ->
    lift_pass(Listen, WMAF, {MonadTrans, monad_writer}).
              

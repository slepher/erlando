%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_runner).

-callback run_nargs() -> integer().
-callback run(monad:monadic(_M, A), [any()]) -> monad:monadic(_N, A) | A.

%% API
-export([run/2]).

%%%===================================================================
%%% API
%%%===================================================================
run(UA, Args) ->
    undetermined:map(
      fun(Module, MA) ->
              N = Module:run_nargs(),
              IsTransformer = lists:member({lift,1}, Module:module_info(exports)),
              case N =< length(Args) of
                  true ->
                      {H, T} = lists:split(N, Args),
                      Inner = Module:run(MA, H),
                      case IsTransformer of
                          true ->
                              run(Inner, T);
                          false ->
                              Inner
                      end;
                  false ->
                      case Args of
                          [] ->
                              MA;
                          _ ->
                              exit(invalid_run_nargs, {Module, N})
                      end
              end
      end, UA).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

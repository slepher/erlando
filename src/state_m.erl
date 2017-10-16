%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(state_m).

-compile({parse_transform, monad_m}).

-transformer({state_t, ['>>='/3, return/2, fail/2, 
                        mzero/1, mplus/3, get/1, put/2, state/2]}).

%% API
-export([eval_state/2, exec_state/2, run_state/2]).

%%%===================================================================
%%% API
%%%===================================================================

eval_state(SM, S) ->
    identity:run_identity(state_t:eval_state(SM, S, {state_t, identity})).

exec_state(SM, S) ->
    identity:run_identity(state_t:exec_state(SM, S, {state_t, identity})).

run_state(SM, S) ->
    identity:run_identity(state_t:run_state(SM, S, {state_t, identity})).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(state_m).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_state).

-compile({parse_transform, import_as}).

-import_as({state_t, [fmap/2, '<$'/2, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2, '>>='/2, '>>'/2]}).

-define(STATE, {state_t, identity}).

%% API
% impl of functor.
-export([fmap/2, '<$'/2]).
% impl of applcative.
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
% impl of monad.
-export(['>>='/2, '>>'/2, return/1]).
% impl of monad_state.
-export([get/0, put/1, state/1]).
-export([eval/2, exec/2, run/2]).

%%%===================================================================
%%% API
%%%===================================================================

pure(A) ->
    state_t:pure(A, ?STATE).

return(A) ->
    state_t:return(A, ?STATE).

get() ->
    state_t:get(?STATE).

put(S) ->
    state_t:put(S, ?STATE).

state(F) ->
    state_t:state(F, ?STATE).

eval(SM, S) ->
    identity:run(state_t:eval(SM, S)).

exec(SM, S) ->
    identity:run(state_t:exec(SM, S)).

run(SM, S) ->
    identity:run(state_t:run(SM, S)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(state_m).

-export_type([state_m/2]).

-type state_m(S, A) :: {state_t, fun((S) -> {identity, {S, A}})}.

-include("gen_fun.hrl").
-include("erlando_instance.hrl").
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-define(STATE, {state_t, identity}).

-erlando_instance(
   #{type => {?MODULE, []},
     capabilities =>
         [{functor, #{requires => identity, adapter => source, remote => state_t}},
          {applicative, #{requires => identity, adapter => source, remote => state_t}},
          {monad, #{requires => identity, adapter => source, remote => state_t}},
          {monad_state, #{requires => identity, adapter => source, remote => state_t}},
          {monad_fail, #{requires => identity, adapter => source,
                         remote => monad_fail_instance}}]}).

-gen_fun(#{remote => state_t, args => identity, extra_call => {identity, run}, 
           functions => [eval/2, exec/2, run/2]}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

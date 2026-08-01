%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cont_m).

-include("gen_fun.hrl").
-include("erlando_instance.hrl").

-erlando_instance(
   #{type => {?MODULE, []},
     adapters =>
         [#{mode => source, requires => identity, remote => cont_t,
            capabilities => [functor, applicative, monad, monad_cont]},
          #{mode => source, requires => identity,
            remote => monad_fail_instance,
            capabilities => [monad_fail]}]}).

-define(CONT, {cont_t, identity}).

-export([reset/1, shift/1]).
-export([eval/1, run/2]).

%%%===================================================================
%%% API
%%%===================================================================
reset(Cont) ->
    cont_t:reset(Cont, ?CONT).

shift(F) ->
    cont_t:shift(fun(CC) ->  F(fun(A) -> identity:run(CC(A)) end) end, ?CONT).

eval(Cont) ->
    identity:run(cont_t:eval(Cont)).

run(Cont, CC) ->
    identity:run(cont_t:run(Cont, fun(A) -> monad:return(CC(A), identity) end)).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

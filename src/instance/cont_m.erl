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
     capabilities =>
         [{functor, #{requires => identity, adapter => source, remote => cont_t}},
          {applicative, #{requires => identity, adapter => source, remote => cont_t}},
          {monad, #{requires => identity, adapter => source, remote => cont_t}},
          {monad_cont, #{requires => identity, adapter => source, remote => cont_t}},
          {monad_fail, #{requires => identity, adapter => source,
                         remote => monad_fail_instance}}]}).

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

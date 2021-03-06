%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cont_m).

-erlando_type({?MODULE, []}).

-include("gen_fun.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_cont).
-behaviour(monad_fail).

-define(CONT, {cont_t, identity}).

-export([reset/1, shift/1]).
-export([eval/1, run/2]).

-gen_fun(#{remote => cont_t, inner_type => identity, 
             behaviours => [functor, applicative, monad, monad_cont]}).

-gen_fun(#{remote => monad_fail_instance, inner_type => identity,
           behaviours => [monad_fail]}).

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

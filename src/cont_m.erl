%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cont_m).

-erlando_type_alias({cont_t, identity}).

-compile({parse_transform, monad_t_transform}).

-define(CONT, {cont_t, identity}).
-define(PG, [[], [?MODULE]]).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_cont).

-transform(#{remote => cont_t, patterns_group => ?PG, args => [?CONT],
             behaviours => [functor, applicative, monad, monad_cont]}).
-transform(#{remote => cont_t, extra_call => {identity, run}, functions => [eval/1, run/2]}).

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

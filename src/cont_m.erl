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

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_cont).

-transform({cont_t, [], identity_run, [run/2, eval/1]}).

-transform_behaviour({cont_t, [], [?CONT], [functor, applicative, monad, monad_cont]}).

-transform_behaviour({cont_t, [?MODULE], [?CONT], [functor, applicative, monad, monad_cont]}).

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

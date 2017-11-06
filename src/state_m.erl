%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(state_m).

-erlando_type(?MODULE).

-compile({parse_transform, monad_t_transform}).

-define(STATE, {state_t, identity}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_state).

-transform({state_t, [], identity_run, [eval/2, exec/2, run/2]}).

-transform_behaviour({state_t, [], [?STATE], functor}).
-transform_behaviour({state_t, [], [?STATE], applicative}).
-transform_behaviour({state_t, [], [?STATE], monad}).
-transform_behaviour({state_t, [], [?STATE], monad_state}).

-transform_behaviour({state_t, [?MODULE], [?STATE], functor}).
-transform_behaviour({state_t, [?MODULE], [?STATE], applicative}).
-transform_behaviour({state_t, [?MODULE], [?STATE], monad}).
-transform_behaviour({state_t, [?MODULE], [?STATE], monad_state}).

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

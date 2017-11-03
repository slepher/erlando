%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cont_m).

-erlando_type(?MODULE).

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_cont).

-transform({cont_t, identity, [fmap/2, '<$'/2]}).
-transform({cont_t, identity, [pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]}).
-transform({cont_t, identity, ['>>='/2, '>>'/2, return/1]}).
-transform({cont_t, identity, [callCC/1]}).
-transform({cont_t, [], identity_run, [run/2, eval/1]}).

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

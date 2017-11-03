%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(writer_m).

-erlando_type(?MODULE).

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_writer).

-transform({writer_t, identity, [fmap/2, '<$'/2]}).
-transform({writer_t, identity, [pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]}).
-transform({writer_t, identity, ['>>='/2, '>>'/2, return/1]}).
-transform({writer_t, identity, [writer/1, tell/1, listen/1, pass/1]}).
-transform({writer_t, [], identity_run, [eval/1, exec/1, run/1]}).

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

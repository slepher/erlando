%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(reader_m).

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_reader).

-transform({reader_t, false, [fmap/2, '<$'/2, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2, '>>='/2, '>>'/2, local/2]}).
-transform({reader_t, true,  [pure/1, return/1, ask/0, reader/1]}).
-transform({reader_t, false, true, [run/2]}).

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


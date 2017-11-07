%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(writer_m).

-erlando_type_alias({writer_t, identity}).

-compile({parse_transform, monad_t_transform}).

-define(WRITER, {writer_t, identity}).
-define(PG, [[], [?MODULE]]).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_writer).

-transform(#{remote => writer_t, patterns_group => ?PG, args => [?WRITER],
             behaviours => [functor, applicative, monad, monad_writer]}).
-transform(#{remote => writer_t, extra_call => {identity, run}, functions => [eval/1, exec/1, run/1]}).

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

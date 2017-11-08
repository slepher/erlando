%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(reader_m).

-erlando_type(?MODULE).

-compile({parse_transform, monad_t_transform}).

-define(READER, {reader_t, identity}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_reader).

-transform(#{remote => reader_t, inner_type => identity,
             behaviours => [functor, applicative, monad, monad_reader]}).
-transform(#{remote => reader_t, args => identity, extra_call => {identity, run}, 
             functions => [run/2]}).

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


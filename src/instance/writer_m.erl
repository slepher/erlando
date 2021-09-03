%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(writer_m).

-erlando_type({?MODULE, []}).

-include("gen_fun.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_writer).
-behaviour(monad_fail).

-gen_fun(#{remote => writer_t, inner_type => identity,
           behaviours => [functor, applicative, monad, monad_writer]}).

-gen_fun(#{remote => monad_fail_instance, inner_type => identity,
           behaviours => [monad_fail]}).

-gen_fun(#{remote => writer_t, args => identity, extra_call => {identity, run},
             functions => [eval/1, exec/1, run/1]}).

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

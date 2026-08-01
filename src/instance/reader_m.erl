%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(reader_m).

-include("gen_fun.hrl").
-include("erlando_instance.hrl").

-define(READER, {reader_t, identity}).

-erlando_instance(
   #{type => {?MODULE, []},
     adapters =>
         [#{mode => source,
            requires => identity,
            remote => reader_t,
            capabilities => [functor, applicative, monad, monad_reader]},
          #{mode => source,
            requires => identity,
            remote => monad_fail_instance,
            capabilities => [monad_fail]}]}).

-gen_fun(#{remote => reader_t, args => identity, extra_call => {identity, run}, 
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

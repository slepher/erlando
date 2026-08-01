%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(writer_m).

-include("gen_fun.hrl").
-include("erlando_instance.hrl").

-erlando_instance(
   #{type => {?MODULE, []},
     adapters =>
         [#{mode => source, requires => identity, remote => writer_t,
            capabilities => [functor, applicative, monad, monad_writer]},
          #{mode => source, requires => identity,
            remote => monad_fail_instance,
            capabilities => [monad_fail]}]}).

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

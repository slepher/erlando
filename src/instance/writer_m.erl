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
     capabilities =>
         [{functor, #{requires => identity, adapter => source, remote => writer_t}},
          {applicative, #{requires => identity, adapter => source, remote => writer_t}},
          {monad, #{requires => identity, adapter => source, remote => writer_t}},
          {monad_writer, #{requires => identity, adapter => source, remote => writer_t}},
          {monad_fail, #{requires => identity, adapter => source,
                         remote => monad_fail_instance}}]}).

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

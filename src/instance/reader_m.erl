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
     capabilities =>
         [{functor, #{requires => identity, adapter => source, remote => reader_t}},
          {applicative, #{requires => identity, adapter => source, remote => reader_t}},
          {monad, #{requires => identity, adapter => source, remote => reader_t}},
          {monad_reader, #{requires => identity, adapter => source, remote => reader_t}},
          {monad_fail, #{requires => identity, adapter => source,
                         remote => monad_fail_instance}}]}).

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

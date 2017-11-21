%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_fail).

-superclass([monad]).

-compile({parse_transform, monad_t_transform}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

-callback fail(any(), M) -> monad:m(M, _A) when M :: monad:class().

%% API
-export([fail/2]).

-transform(#{args => [?MODULE], functions => [fail/1]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec fail(any(), M) -> monad:m(M, _A) when M :: monad:class().
fail(E, UMonadFail) ->
    undetermined:new(
      fun(MonadFail) ->
              typeclass_trans:apply(fail, [E], MonadFail, ?MODULE)
      end, UMonadFail).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

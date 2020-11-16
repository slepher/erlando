%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_error).

-superclass([monad]).


-callback throw_error(any(), M) -> monad:m(M, _A) when M :: monad:class().
-callback catch_error(monad:m(M, A), fun((_E) -> monad:m(M,A)), M) -> monad:m(M, _A) when M :: monad:class().

-include("gen_fun.hrl").

%% API
-export([throw_error/2, catch_error/3]).
-export([lift_error/2, lift_either/2, trans_error/3, run_error/2]).

-gen_fun(#{args => [?MODULE], functions => [throw_error/1, catch_error/2]}).
-gen_fun(#{args => [?MODULE], functions => [lift_error/1, trans_error/2, run_error/1]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec throw_error(any(), M) -> monad:m(M, _A) when M :: monad:class().
throw_error(E, UMonadError) ->
    undetermined:new(
      fun(MonadError) ->
              typeclass_trans:apply(throw_error, [E], MonadError, ?MODULE)
      end, UMonadError).

-spec catch_error(monad:m(M, A), fun((_E) -> monad:m(M,A)), M) -> monad:m(M, _A) when M :: monad:class().
catch_error(UA, EUA, UMonadError) ->
    undetermined:map(
      fun(MonadError, MA) ->
              typeclass_trans:apply(catch_error, [MA, EUA], MonadError, ?MODULE)
      end, UA, UMonadError).

lift_error(Error, MonadError) ->
    case Error of
        ok ->
            monad:return(ok, MonadError);
        {ok, Val} ->
            monad:return(Val, MonadError);
        {error, Reason} ->
            throw_error(Reason, MonadError)
    end.

lift_either(Either, MonadError) ->
    case Either of
        {left, Val} ->
            monad:return(Val, MonadError);
        {right, Reason} ->
            throw_error(Reason, MonadError)
    end.

trans_error(MEA, KE, MonadError) ->
    catch_error(MEA, fun(E1) -> throw_error(KE(E1), MonadError) end, MonadError).

run_error(MEA, MonadError) ->
    catch_error(functor:fmap(fun(A) -> {right, A} end, MEA, MonadError),
                fun(E) -> monad:return({left, E}, MonadError) end, MonadError).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

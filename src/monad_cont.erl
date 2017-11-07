%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_cont).

-superclass([monad]).

-callback callCC(fun((fun( (A) -> monad:monadic(M, _B) )
                         ) -> monad:monadic(M, A)), M) -> monad:monadic(M, A) when M :: monad:monad().

-compile({parse_transform, monad_t_transform}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

%%%===================================================================        
%%% API
%%%===================================================================
-export([callCC/2]).

-transform(#{args => [?MODULE], functions => [callCC/1]}).

callCC(F, UMonadCont) ->
    undetermined:new(
      fun(MonadCont) ->
              NF = fun(CC) -> 
                           NCC = fun(A) -> undetermined:run(CC(A), MonadCont) end,
                           undetermined:run(F(NCC), MonadCont)
                   end,
              typeclass_trans:apply(callCC, [NF], MonadCont, ?MODULE)
      end, UMonadCont).
%%%===================================================================
%%% Internal functions
%%%===================================================================

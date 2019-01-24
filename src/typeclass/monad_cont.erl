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

-callback callCC(fun((fun((A) -> monad:m(M, _B))) -> monad:m(M, A)), M) -> monad:m(M, A) when M :: monad:class().

-include("gen_fun.hrl").

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

%%%===================================================================        
%%% API
%%%===================================================================
-export([callCC/2]).

-gen_fun(#{args => [?MODULE], functions => [callCC/1]}).

-spec callCC(fun((fun( (A) -> monad:m(M, _B) )) -> monad:m(M, A)), M) -> monad:m(M, A) when M :: monad:class().
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

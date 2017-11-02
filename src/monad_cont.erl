%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  2 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_cont).

-callback callCC(fun((fun( (A) -> monad:monadic(M, _B) ))-> monad:monadic(M, A))) -> monad:monadic(M, A) when M :: monad:monad().

-compile({parse_transform, monad_t_transform}).

-include("functor.hrl").
-include("applicative.hrl").
-include("monad.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-export([callCC/1]).
-export([callCC/2]).

callCC(F) ->
    undetermined:new(
      fun(MonadCont) -> 
              callCC(F, MonadCont)
      end).

callCC(F, MonadCont) ->
    NF = fun(CC) -> 
                 NCC = fun(A) -> undetermined:run(CC(A), MonadCont) end,
                 undetermined:run(F(NCC), MonadCont)
         end,
    monad_trans:apply_fun(callCC, [NF], MonadCont).
%%%===================================================================
%%% Internal functions
%%%===================================================================

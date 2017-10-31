%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cont_m).

-compile({parse_transform, import_as}).

-import_as({cont_t, [fmap/2, '<$'/2, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2,
                     '>>='/2, '>>'/2, callCC/1]}).

-define(CONT, {cont_t, identity}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_cont).

%% API
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([callCC/1]).
-export([run/2, eval/1]).
%%%===================================================================
%%% API
%%%===================================================================
pure(A) ->
    cont_t:pure(A, ?CONT).

return(A) ->
    cont_t:return(A, ?CONT).

run(CA, CC) ->
    identity:run(cont_t:run(CA, CC)).

eval(CA) ->
    identity:run(cont_t:eval(CA)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

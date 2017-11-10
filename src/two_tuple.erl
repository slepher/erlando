%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(two_tuple).

-include("erlando.hrl").

%% API
-export([fmap/3]).
-export([fmap_trans/3]).
-export([ap/3, pure/2]).
-export([ap_trans/3, pure_trans/2]).
-export([bind/3, return/2]).
-export([bind_trans/3]).
-export([run/2]).
%%%===================================================================
%%% API
%%%===================================================================
fmap(T, F, {T, A}) ->
    {T, F(A)}.

fmap_trans(T, F, {T, FA}) ->
    {T, functor:fmap(F, FA)}.

ap(T, {T, F}, {T, A}) ->
    {T, F(A)}.


ap_trans(T, {T, AF}, {T, AA}) ->
    {T, applicative:ap(AF, AA)}.

pure(T, A) ->
    {T, A}.

pure_trans(T, A) ->
    {T, applicative:pure(A)}.

bind(T, {T, A}, K) ->
    {T, B} = K(A),
    {T, B}.

bind_trans(T, {T, MA}, K) ->
    pure(T, monad:bind(MA, fun(A) -> run(T, K(A)) end)).

return(T, A) ->
    {T, A}.

run(T, #undetermined{} = A) ->
    run(T, undetermined:run(A, T));
run(T, {T, A}) ->
    A.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

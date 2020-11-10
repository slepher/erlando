%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2020, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Nov 2020 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(applicative_lift).

-erlando_type({lift, [lift/2]}).

-export_type([lift/1, pure/1, other/1]).

-define(TYPE, lift).

%% This is really instance (Error e) => Monad (Either e) with 'error'
%% for Left and 'ok' for Right.
-type lift(A) :: pure(A) | other(A).

-type pure(A) :: {pure, A}.
-type other(_A) :: {other, term()}.

-include("gen_fun.hrl").
-include("do.hrl").

-behaviour(functor).
-behaviour(applicative).

-gen_fun(#{patterns => [lift], tbehaviours => [functor, applicative]}).

%% API
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export([run_errors/1, hoist_errors/1, failure/1]).
-export([sequence_either/1, sequence_monad_error/1, sequence_monad_error/2]).
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
fmap(F, LA) ->
    case LA of
        {pure, A} ->
            {pure, F(A)};
        {other, E} ->
            {other, functor:fmap(F, E)}
    end.

-spec '<$'(B, lift(_A)) -> lift(B).
'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?TYPE).

pure(A) ->
    {pure, A}.

-spec '<*>'(lift(fun((A) -> B)), lift(A)) -> lift(B).
'<*>'({pure, F}, {pure, A}) ->
    {pure, F(A)};
'<*>'({pure, F}, {other, E}) ->
    {other, functor:fmap(F, E)};
'<*>'({other, EF}, {pure, A}) ->
    {other, functor:fmap(fun(F) -> F(A) end, EF)};
'<*>'({other, EF}, {other, E}) ->
    {other, applicative:'<*>'(EF, E)}.

-spec lift_a2(fun((A, B) -> C), lift(A), lift(B)) -> lift(C).
lift_a2(F, EA, EB) ->
    applicative:default_lift_a2(F, EA, EB, ?TYPE).

-spec '*>'(lift(_A), lift(B)) -> lift(B).
'*>'(EA, EB) ->
    applicative:'default_*>'(EA, EB, ?TYPE).

-spec '<*'(lift(A), lift(_B)) -> lift(A).
'<*'(EA, EB) ->
    applicative:'default_<*'(EA, EB, ?TYPE).

run_errors({other, {const, E}}) ->
    {left, E};
run_errors({pure, A}) ->
    {right, A}.

hoist_errors({left, E}) ->
    {other, {const, E}};
hoist_errors({right, A}) ->
    {pure, A}.

failure(E) ->
    {other, {const, E}}.

sequence_either(FEA) ->
    run_errors(undetermined:run(traversable:traverse(fun hoist_errors/1, FEA), lift)).

sequence_monad_error(FME) ->
    sequence_monad_error(FME, monad).

sequence_monad_error(FME, Monad) ->
    do([Monad ||
           Es <- traversable:traverse(fun(MEA) -> monad_error:run_error(MEA, Monad) end, FME),
           case sequence_either(Es) of
               {left, Es1} ->
                   monad_fail:fail(Es1, Monad);
               {right, A} ->
                   monad:return(A, Monad)
           end
       ]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

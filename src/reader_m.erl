%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 16 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(reader_m).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_reader).

-compile({parse_transform, import_as}).

-import_as({reader_t, [fmap/2, '<$'/2, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2,
                       '>>='/2, '>>'/2, local/2]}).

-define(READER, {reader_t, identity}).

-export([fmap/2, '<$'/2]).
% impl of applicative
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
% impl of monad
-export(['>>='/2, '>>'/2, return/1]).
% impl of monad reader
-export([ask/0, reader/1, local/2]).
-export([run/2]).
%%%===================================================================
%%% API
%%%===================================================================
pure(A) ->
    reader_t:pure(A, ?READER).

return(A) ->
    reader_t:return(A, ?READER).

ask() ->
    reader_t:ask(?READER).

reader(R) ->
    reader_t:reader(R, ?READER).

run(RTA, R) ->
    identity:run(reader_t:run(RTA, R)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

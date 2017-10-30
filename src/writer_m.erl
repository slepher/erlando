%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 29 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(writer_m).

-compile({parse_transform, import_as}).

-import_as({writer_t, [fmap/2, '<$'/2, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2,
                       '>>='/2, '>>'/2, listen/1, pass/1]}).
                       
-define(WRITER, {writer_t, identity}).

%% API
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([writer/1, tell/1, listen/1, pass/1]).
-export([run/1]).

%%%===================================================================
%%% API
%%%===================================================================
pure(A) ->
    writer_t:pure(A, identity).

return(A) ->
    writer_t:return(A, identity).

writer({A, Ws}) ->
    writer_t:writer({A, Ws}, identity).

tell(Ws) ->
    writer_t:tell(Ws, identity).

run(WA) ->
    identity:run(writer_t:run(WA)).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

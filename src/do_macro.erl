%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 21 Jan 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(do_macro).

-include_lib("astranaut/include/quote.hrl").

%% API
-export([do/1]).
-export([format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
do(Ast) ->
    astranaut_do:do(Ast, #{monad => monad, monad_fail => monad_fail}).

format_error(Reason) ->
    astranaut_do:format_error(Reason).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

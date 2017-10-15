%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(alternative).

%% API
-export([empty/0, '<|>'/2]).

-callback empty() -> applicative:applicative(_F, _A).
-callback '<|>'(applicative:applicative(F, A), 
                applicative:applicative(F, A)) -> applicative:applicative(F, A).

%%%===================================================================
%%% API
%%%===================================================================

empty() ->
    undetermined:empty().

'<|>'(AA, AB) ->
    undetermined:unwrap(undetermined:'<|>'(undetermined:wrap(AA), undetermined:wrap(AB))).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

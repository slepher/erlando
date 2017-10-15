
%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 10 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monoid).
-export_type([monoid/1]).

-type monoid(_M) :: any().

-callback mempty() -> monoid(_M).
-callback mappend(monoid(M), monoid(M)) -> monoid(M).

%% API
-export([mempty/0, mappend/2]).

%%%===================================================================
%%% API
%%%===================================================================

mempty() ->
    undetermined:mempty().

mappend(MA, MB) ->
    undetermined:unwrap(undetermined:mappend(undetermined:wrap(MA), undetermined:wrap(MB))).
    
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

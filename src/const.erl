%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(const).

-behaviour(functor).
-behaviour(applicative).

%% API
-export([const/1]).
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export([run_const/1]).

%%%===================================================================
%%% API
%%%===================================================================

const(R) ->
    {?MODULE, R}.

fmap(_F, CA) ->
    CA. 

'<$'(_B, CA) ->
    CA.

pure(_A) ->
    {const, monoid:mempty()}.

'<*>'({?MODULE, MA}, {?MODULE, MB}) ->
    const(monoid:mappend(MA, MB)).

lift_a2(F, CA, CB) ->
    applicative:default_lift_a2(F, CA, CB, ?MODULE).

'*>'(CA, CB) ->
    applicative:'default_*>'(CA, CB, ?MODULE).

'<*'(CA, CB) ->
    applicative:'default_<*'(CA, CB, ?MODULE).

run_const({undetermined, _} = UA) ->
    run_const(undetermined:run(UA, ?MODULE));
run_const({?MODULE, CA}) ->
    CA.
    

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

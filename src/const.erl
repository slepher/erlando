%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(const).

-erlando_type(?MODULE).

-compile({parse_transform, monad_t_transform}).

-behaviour(functor).
-behaviour(applicative).

-include("erlando.hrl").

%% API
-export([const/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export([run_const/1]).

-transform(#{args => [?MODULE], behaviours => [functor, applicative]}).

%%%===================================================================
%%% API
%%%===================================================================
const(R) ->
    {?MODULE, R}.

fmap(_F, CA, ?MODULE) ->
    CA. 

'<$'(_B, CA, ?MODULE) ->
    CA.

pure(_A, ?MODULE) ->
    {const, monoid:mempty()}.

'<*>'({?MODULE, MA}, {?MODULE, MB}, ?MODULE) ->
    const(monoid:mappend(MA, MB)).

lift_a2(F, CA, CB, ?MODULE) ->
    applicative:default_lift_a2(F, CA, CB, ?MODULE).

'*>'(CA, CB, ?MODULE) ->
    applicative:'default_*>'(CA, CB, ?MODULE).

'<*'(CA, CB, ?MODULE) ->
    applicative:'default_<*'(CA, CB, ?MODULE).

run_const(#undetermined{} = UA) ->
    run_const(undetermined:run(UA, ?MODULE));
run_const({?MODULE, CA}) ->
    CA.
%%%===================================================================
%%% Internal functions
%%%===================================================================

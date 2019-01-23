%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 15 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(alternative).

-superclass([applicative]).

-callback empty(F) -> applicative:applicative(F, _A).
-callback '<|>'(applicative:applicative(F, A), 
                applicative:applicative(F, A), F) -> applicative:applicative(F, A).

-compile({parse_transform, cut}).
-compile({parse_transform, function_generator}).

%% API
-export([empty/1, '<|>'/3]).

-gen_fun(#{args => [?MODULE], functions => [empty/0, '<|>'/2]}).

%%%===================================================================
%%% API
%%%===================================================================
empty(UAlternative) ->
    undetermined:new(
      fun(Alternative) -> 
              typeclass_trans:apply(empty, [], Alternative, ?MODULE)
      end, UAlternative).

'<|>'(UA, UB, UAlternative) ->
    undetermined:map_pair(
      fun(Alternative, FA, FB) ->
              typeclass_trans:apply('<|>', [FA, FB], Alternative, ?MODULE)
      end, UA, UB, UAlternative).
%%%===================================================================
%%% Internal functions
%%%===================================================================

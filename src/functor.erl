%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(functor).

-superclass([]).

-export_type([class/0, f/2]).
-export_type([functor/2]).

-type functor(F, A) :: f(F, A).

-type class() :: {module(), class()} | module().
-type f(_F, _A) :: any().

-callback fmap(fun((A) -> B), functor:f(F, A), F) -> functor:f(F, B) when F :: functor:class().
-callback '<$'(B, functor:f(F, _A), F) -> functor:f(F, B) when F :: functor:class().

-compile({parse_transform, monad_t_transform}).

-include("op.hrl").

%% API
-export([fmap/3, '<$'/3]).
-export(['<$>'/3]).
-export(['default_<$'/3]).

-transform(#{args => [?MODULE], functions => [fmap/2, '<$'/2]}).
-transform(#{args => [?MODULE], functions => ['<$>'/2]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec fmap(fun((A) -> B), functor:f(F, A), F) -> functor:f(F, B) when F :: functor:class().
fmap(F, UA, UFunctor) ->
    undetermined:map(
      fun(Functor, FA) ->
              do_fmap(F, FA, Functor)
      end, UA, UFunctor).

-spec '<$'(B, functor:f(F, _A), F) -> functor:f(F, B) when F :: functor:class().
'<$'(UB, UA, UFunctor) ->
    undetermined:map_pair(
      fun(Functor, FB, FA) ->
              typeclass_trans:apply('<$', [FB, FA], Functor, ?MODULE)
      end, UB, UA, UFunctor).

-spec '<$>'(fun((A) -> B), functor:f(F, A), F) -> functor:f(F, B) when F :: functor:class().
'<$>'(F, FA, Functor) ->
    fmap(F, FA, Functor).

-spec 'default_<$'(B, functor:f(F, _A), F) -> functor:f(F, B) when F :: functor:class().
'default_<$'(B, FA, Functor) ->
    do_fmap(function_instance:const(B), FA, Functor).
%%%===================================================================
%%% Internal functions
%%%===================================================================
do_fmap(F, FA, Functor) ->
    typeclass_trans:apply(fmap, [F, FA], Functor, ?MODULE).

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

-export_type([functor/2]).

-type functor_module() :: module() | {module(), functor_module()}.
-type functor(_F, _A) :: any().

-callback fmap(fun((A) -> B), functor(F, A)) -> functor(F, B).
-callback '<$'(B, functor(F, _A)) -> functor(F, B).

-compile({parse_transform, monad_t_transform}).

-include("op.hrl").

%% API
-export([fmap/3, '<$'/3]).
-export(['<$>'/3]).
-export(['default_<$'/3]).

-transform({?MODULE, [functor], [fmap/2, '<$'/2]}).
-transform({?MODULE, [functor], ['<$>'/2]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec fmap(fun((A) -> B), functor(F, A)) -> functor(F, B).
fmap(F, UA, UFunctor) ->
    undetermined:map(
      fun(Functor, FA) ->
              do_fmap(F, FA, Functor)
      end, UA, UFunctor).

-spec '<$'(B, functor(F, _A)) -> functor(F, B).
'<$'(UB, UA, UFunctor) ->
    undetermined:map_pair(
      fun(Functor, FB, FA) ->
              typeclass_trans:apply('<$', [FB, FA], Functor)
      end, UB, UA, UFunctor).

-spec '<$>'(fun((A) -> B), functor(F, A), F) -> functor(F, B).
'<$>'(F, FA, Functor) ->
    fmap(F, FA, Functor).

'default_<$'(B, FA, Functor) ->
    do_fmap(function_instance:const(B), FA, Functor).
%%%===================================================================
%%% Internal functions
%%%===================================================================
do_fmap(F, FA, Functor) ->
    typeclass_trans:apply(fmap, [F, FA], Functor).

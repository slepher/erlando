%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(functor).

-compile({parse_transform, monad_t_transform}).

-include("op.hrl").

-export_type([functor/2]).
%% API
-export([fmap/1]).
-export([fmap/2, '<$'/2]).
-export([fmap/3, '<$'/3]).
-export(['<$>'/3]).
-export(['default_<$'/3]).

-transform({?MODULE, [functor], ['<$>'/2]}).

-type functor(_F, _A) :: any().

-callback fmap(fun((A) -> B), functor(F, A)) -> functor(F, B).
-callback '<$'(B, functor(F, _A)) -> functor(F, B).
%%%===================================================================
%%% API
%%%===================================================================
-spec fmap(fun((A) -> B)) -> fun((functor(F, A)) -> functor(F, B)).
fmap(F) ->
    fun(UA) ->
            fmap(F, UA)
    end.

-spec fmap(fun((A) -> B), functor(F, A)) -> functor(F, B).
fmap(F, UA) ->
    undetermined:map(
      fun(Functor, FA) ->
              do_fmap(F, FA, Functor)
      end, UA, ?MODULE).

-spec '<$'(B, functor(F, _A)) -> functor(F, B).
'<$'(UB, UA) ->
    undetermined:map_pair(
      fun(Functor, FB, FA) ->
              typeclass_trans:apply('<$', [FB, FA], Functor)
      end, UB, UA, ?MODULE).

fmap(F, UA, Functor) ->
    undetermined:run(fmap(F, UA), Functor).

'<$'(UB, UA, Functor) ->
    undetermined:run('<$'(UB, UA), Functor).

-spec '<$>'(fun((A) -> B), functor(F, A), F) -> functor(F, B).
'<$>'(F, FA, Functor) ->
    fmap(F, FA, Functor).

'default_<$'(B, FA, Functor) ->
    do_fmap(function_instance:const(B), FA, Functor).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_fmap(F, FA, Functor) ->
    typeclass_trans:apply(fmap, [F, FA], Functor).

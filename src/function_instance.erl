%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  9 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(function_instance).

-erlando_type({function, []}).

-compile({parse_transform, monad_t_transform}).

-include("op.hrl").

-define(TYPE, function).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_reader).
-behaviour(monad_runner).

%% API
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
% monad reader instance.
-export([ask/1, local/3, reader/2]).
% monad runner instance.
-export([run_nargs/0, run_m/2]).

-export(['.'/2]).
-export([const/1]).
-export([id/0, id/1]).

-transform(#{args => [?TYPE], behaviours => [functor, applicative, monad, monad_reader]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec fmap(fun((A) -> B), fun((R) -> A)) -> fun((R) -> B).
fmap(F, FA, ?TYPE) ->
    '.'(F, FA).

-spec '<$'(B, fun((R) -> _A)) -> fun((R) -> B).
'<$'(B, FA, ?TYPE) ->
    functor:'default_<$'(B, FA, ?TYPE).

-spec '<*>'(fun((R) -> fun((A) -> B)), fun((R) -> A)) -> fun((R) -> B).
'<*>'(FF, FA, ?TYPE) ->
    fun(R) -> (FF(R))(FA(R)) end.

-spec pure(A) -> fun((_R) -> A).
pure(A, ?TYPE) ->
    const(A).

-spec lift_a2(fun((A, B) -> C), fun((R) -> A), fun((R) -> B)) -> fun((R) -> C).
lift_a2(F, RTA, RTB, ?TYPE) ->
    applicative:default_lift_a2(F, RTA, RTB, ?MODULE).

-spec '*>'(fun((R) -> _A), fun((R) -> B)) -> fun((R) -> B).
'*>'(RTA, RTB, ?TYPE) ->
    applicative:'default_*>'(RTA, RTB, ?TYPE).

-spec '<*'(fun((R) -> A), fun((R) -> _B)) -> fun((R) -> A).
'<*'(RTA, RTB, ?TYPE) ->
    applicative:'default_<*'(RTA, RTB, ?TYPE).
           
-spec '>>='(fun((R) -> A), fun((A) -> fun((R) -> B))) -> fun((R) -> B).  
'>>='(FA, KFB, ?TYPE) ->
    fun(X) -> (KFB(FA(X)))(X) end.

-spec '>>'(fun((R) -> _A), fun((R) -> B)) -> fun((R) -> B).
'>>'(FA, FB, ?TYPE) ->
    monad:'default_>>'(FA, FB, ?TYPE).

-spec return(A) -> fun((_R) -> A).
return(A, ?TYPE) ->
    monad:default_return(A, ?TYPE).

ask(?TYPE) ->
    id().

local(F, FI, ?TYPE) ->
    '.'(FI, F).

reader(F, ?TYPE) ->
    id(F).

run_nargs() ->
    1.

run_m(F, [A]) ->
    F(A).

'.'(F, G) ->
    fun(X) -> F(G(X)) end.

const(A) -> fun(_R) -> A end.

id() ->
    fun(A) -> A end.

id(A) -> A.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

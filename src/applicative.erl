%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(applicative).

-export_type([applicative_module/0, f/1, applicative/2]).

%% API
-export(['<*>'/2, ap/2, pure/1]).
-export([as/2]).

-type applicative_module() :: module().
-type f(_A) :: any().
-type applicative(_F, _A) :: any().

-callback '<*>'(applicative(F, fun((A) -> B)), applicative(F, A)) -> applicative(F, B).
-callback pure(A) -> f(A).

%%%===================================================================
%%% API
%%%===================================================================

'<*>'(AF, AA) ->
    undetermined:unwrap(undetermined:'<*>'(undetermined:wrap(AF), undetermined:wrap(AA))).

ap(AF, A) ->
    '<*>'(AF, A).

pure(A) ->
    undetermined:undetermined(fun(M) -> M:pure(A) end).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
as({undetermined, _} = A, Module) ->
    undetermined:run(A, Module);
as(A, _Module) ->
    A.

    

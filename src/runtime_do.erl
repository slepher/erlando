%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(runtime_do).

%% API
-export(['>>='/3, return/2, fail/2]).

%%%===================================================================
%%% API
%%%===================================================================
'>>='(X, K, monad) ->
    monad:'>>='(X, K);
'>>='({undetermined, _} = UX, K, Monad) ->
    '>>='(undetermined:run(UX, Monad), fun(A) -> undetermined:run(K(A), Monad) end, Monad);
'>>='(X, K, {T, M}) ->
    T:'>>='(X, K, {T, M});
'>>='(X, K, M) ->
    M:'>>='(X, K).

return(A, {T, M}) ->
    T:return(A, {T, M});
return(A, M) ->
    M:return(A).

fail(E, {T, M}) ->
    T:fail(E, {T, M});
fail(E, monad) ->
    monad_fail:fail(E);
fail(E, M) ->
    M:fail(E).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

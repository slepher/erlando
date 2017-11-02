%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  1 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(applicative_trans).

%% API
-export([]).

-callback pure(A, F) -> applicative:applicative(F, A).
-callback '<*>'(applicative:applicative(F, fun((A) -> B)), applicative:applicative(F, A), F) -> applicative:applicative(F, B).
-callback lift_a2(fun((A, B) -> C), 
                     applicative:applicative(F, A), 
                     applicative:applicative(F, B), F) -> applicative:applicative(F, C).
-callback '*>'(applicative:applicative(F, _A), applicative:applicative(F, B), F) -> applicative:applicative(F, B).
-callback '<*'(applicative:applicative(F, A), applicative:applicative(F, _B), F) -> applicative:applicative(F, A).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

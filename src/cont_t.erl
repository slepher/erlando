%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 June 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cont_t).

-compile({parse_transform, cut}).
-compile({parse_transform, do}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("op.hrl").

-define(CONT_T_MONAD, {?MODULE, monad}).

-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_trans).
-behaviour(monad_state).
-behaviour(monad_reader).
-behaviour(monad_runner).

-export_type([cont_t/3]).

-export([type/0]).
-export([new/1, cont_t/1, run_cont_t/1]).
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export([pure/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([return/2]).
-export([fail/1, fail/2]).
-export([lift/1]).
-export([callCC/1]).
-export([shift/1, reset/1]).
-export([get/0, put/1, state/1]).
-export([get/1, put/2, state/2]).
-export([ask/0, reader/1, local/2]).
-export([ask/1, reader/2]).
-export([run_nargs/0, run_m/2]).
-export([run/2, eval/1, map/2, with/2]).
-export([lift_local/4]).

-opaque cont_t(R, M, A) :: {cont_t, inner_t(R, M, A)}.
-type inner_t(R, M, A) :: fun((fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R)).
-type t(M) :: {cont_t, M}.

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(IM) ->
    {?MODULE, IM}.

-spec cont_t(inner_t(R, M, A)) -> cont_t(R, M, A).
cont_t(Inner) ->
    {?MODULE, Inner}.

-spec run_cont_t(cont_t(R, M, A)) -> inner_t(R, M, A).
run_cont_t({?MODULE, Inner}) ->
    Inner;
run_cont_t({undetermined, _} = U) ->
    run_cont_t(undetermined:run(U, cont_t));
run_cont_t(Other) ->
    exit({invalid_monad, Other}).

type() ->
    type:default_type(?MODULE).

-spec fmap(fun((A) -> B), cont_t(R, M, A)) -> cont_t(R, M, B).
fmap(F, CTA) ->
    cont_t(
      fun(CC) ->
              run(CTA, fun(A) -> CC(F(A)) end)
      end).

-spec '<$'(B, cont_t(R, M, _A)) -> cont_t(R, M, B).
'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?MODULE).

-spec pure(A) -> cont_t(_R, _M, A).
pure(A) ->
   pure(A).

pure(A, {?MODULE, _IM}) ->
    return(A).

-spec '<*>'(cont_t(R, M, fun((A) -> B)), cont_t(R, M, A)) -> cont_t(R, M, B).
'<*>'(CTF, CTA) ->
    cont_t(
      fun(CC) ->
              run(CTF, fun(F) -> run(CTA, fun(A) -> CC(F(A)) end) end)
      end).

-spec lift_a2(fun((A, B) -> C), cont_t(R, M, A), cont_t(R, M, B)) -> cont_t(R, M, C).
lift_a2(F, CTA, CTB) ->
    applicative:default_lift_a2(F, CTA, CTB, ?MODULE).

-spec '*>'(cont_t(R, M, _A), cont_t(R, M, B)) -> cont_t(R, M, B).
'*>'(CTA, CTB) ->
    applicative:'default_*>'(CTA, CTB, ?MODULE).

-spec '<*'(cont_t(R, M, A), cont_t(R, M, _B)) -> cont_t(R, M, A).
'<*'(CTA, CTB) ->
    applicative:'default_<*'(CTA, CTB, ?MODULE).

-spec '>>='(cont_t(R, M, A), fun((A) -> cont_t(R, M, B))) -> cont_t(R, M, B).
'>>='(CTA, KCTB) ->
    cont_t(
      fun (K) -> 
              run(CTA, 
                       fun (A) -> 
                               run(KCTB(A), K) 
                       end) 
      end).

-spec '>>'(cont_t(R, M, _A), cont_t(R, M, B)) -> cont_t(R, M, B).
'>>'(CTA, CTB) ->
    monad:'default_>>'(CTA, CTB, ?MODULE).

-spec return(A) -> cont_t(_R, _M, A).
return(A) ->
    cont_t(fun (K) -> K(A) end).

-spec return(A, t(M)) -> cont_t(_R, M, A).
return(A, {?MODULE, _IM}) ->
    return(A).

fail(E) ->
    fail(E, {?MODULE, monad_fail}).

-spec fail(any(), t(M)) -> cont_t(_R, M, _A).
fail(E, {?MODULE, IM}) ->
    cont_t(fun (_) -> monad:fail(E, IM) end).

lift(X) ->
    cont_t(fun (F) -> monad:'>>='(X, F) end).

-spec callCC(fun((fun( (A) -> cont_t(R, M, _B) ))-> cont_t(R, M, A))) -> cont_t(R, M, A).
callCC(F) ->
    cont_t(fun (CC) -> run(F(fun(A) -> cont_t(fun(_) -> CC(A) end) end), CC) end).

-spec reset(cont_t(R, M, R)) -> cont_t(_NR, M, R).
reset(X) ->
    lift(eval(X)).

-spec shift(fun((fun((A) -> monad:monadic(M, R))) -> cont_t(R, M, R))) -> cont_t(R, M, A).
shift(F) ->
    cont_t(fun (CC) -> eval(F(CC)) end).

ask() ->
    ask({?MODULE, monad_reader}).

local(F, X) ->
    lift_local(fun() -> monad_reader:ask() end, monad_reader:local(_, _), F, X).

reader(F) ->
    reader(F, {?MODULE, monad_reader}).

ask({?MODULE, IM}) ->
    lift(monad_reader:ask(IM)).

reader(F, {?MODULE, IM}) ->
    lift(monad_reader:reader(F, IM)).

get() ->
    get({?MODULE, monad_state}).

put(S) ->
    put(S, {?MODULE, monad_state}).

state(F) ->
    state(F, {?MODULE, monad_state}).

get({?MODULE, IM}) ->
    lift(monad_state:get(IM)).

put(S, {?MODULE, IM}) ->
    lift(monad_state:put(S, IM)).

state(F, {?MODULE, IM}) ->
    lift(monad_state:state(F, IM)).

-spec run(cont_t(R, M, A), fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R).
run(X, CC) ->
    (run_cont_t(X))(CC).

-spec eval(cont_t(R, M, R)) -> monad:monadic(M, R).
eval(X) ->
    run(X, fun (A) -> monad:return(A) end).

-spec map(fun((monad:monadic(M, R)) -> monad:monadic(M, R)), cont_t(R, M, A)) -> cont_t(R, M, A).
map(F, X) ->
    cont_t(fun(CC) -> F(run(X, CC)) end).

-spec with(fun((fun((B) -> monad:monadic(M, R))) -> fun((A) -> monad:monadic(M, R))), 
                cont_t(R, M, A)) -> cont_t(R, M, B).
with(F, X) ->
    cont_t(fun (CC) -> run(X, F(CC)) end).

run_nargs() ->
    1.

run_m(CTA, [CC]) ->
    run(CTA, CC).

lift_local(Ask, Local, F, X) ->
    cont_t(fun (CC) ->
                   do([monad || 
                          R <- Ask(),
                          Local(F, run(X, fun(A) -> Local(fun(_) -> R end, CC(A)) end))
                      ])
           end).

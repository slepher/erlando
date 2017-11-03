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
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("op.hrl").

-define(CONT_T_MONAD, {?MODULE, monad}).

-behaviour(type).
-behaviour(functor).
-behaviour(functor_trans).
-behaviour(applicative).
-behaviour(applicative_trans).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_cont).
-behaviour(monad_cont_trans).
-behaviour(monad_fail).
-behaviour(monad_fail_trans).
-behaviour(monad_reader).
-behaviour(monad_reader_trans).
-behaviour(monad_state).
-behaviour(monad_state_trans).
-behaviour(monad_runner).

-export_type([cont_t/3]).

-export([type/0]).
-export([new/1, cont_t/1, run_cont_t/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([lift/2]).
-export([callCC/2]).
-export([fail/2]).
-export([shift/2, reset/2]).
-export([ask/1, reader/2, local/3]).
-export([get/1, put/2, state/2]).
-export([run_nargs/0, run_m/2]).
-export([run/2, eval/2, map/2, with/2]).
-export([lift_local/5]).

-transform({?MODULE, any, [fmap/2, '<$'/2]}).
-transform({?MODULE, any, [pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]}).
-transform({?MODULE, any, ['>>='/2, '>>'/2, return/1]}).
-transform({?MODULE, monad, [lift/1]}).
-transform({?MODULE, monad_cont, [callCC/1]}).
-transform({?MODULE, monad_fail, [fail/1]}).
-transform({?MODULE, monad_reader, [ask/0, reader/1, local/2]}).
-transform({?MODULE, monad_state, [get/0, put/1, state/1]}).
-transform({?MODULE, monad, [shift/1, reset/1]}).
-transform({?MODULE, monad, [eval/1, lift_local/4]}).

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
fmap(F, CTA, {?MODULE, _IM}) ->
    cont_t(
      fun(CC) ->
              run(CTA, fun(A) -> CC(F(A)) end)
      end).

-spec '<$'(B, cont_t(R, M, _A)) -> cont_t(R, M, B).
'<$'(B, FA, {?MODULE, _IM} = CT) ->
    functor:'default_<$'(B, FA, CT).

pure(A, {?MODULE, _IM} = CT) ->
    return(A, CT).

-spec '<*>'(cont_t(R, M, fun((A) -> B)), cont_t(R, M, A)) -> cont_t(R, M, B).
'<*>'(CTF, CTA, {?MODULE, _IM}) ->
    cont_t(
      fun(CC) ->
              run(CTF, fun(F) -> run(CTA, fun(A) -> CC(F(A)) end) end)
      end).

-spec lift_a2(fun((A, B) -> C), cont_t(R, M, A), cont_t(R, M, B)) -> cont_t(R, M, C).
lift_a2(F, CTA, CTB, {?MODULE, _IM} = CT) ->
    applicative:default_lift_a2(F, CTA, CTB, CT).

-spec '*>'(cont_t(R, M, _A), cont_t(R, M, B)) -> cont_t(R, M, B).
'*>'(CTA, CTB, {?MODULE, _IM} = CT) ->
    applicative:'default_*>'(CTA, CTB, CT).

-spec '<*'(cont_t(R, M, A), cont_t(R, M, _B)) -> cont_t(R, M, A).
'<*'(CTA, CTB, {?MODULE, _IM} = CT) ->
    applicative:'default_<*'(CTA, CTB, CT).

-spec '>>='(cont_t(R, M, A), fun((A) -> cont_t(R, M, B))) -> cont_t(R, M, B).
'>>='(CTA, KCTB, {?MODULE, _IM}) ->
    cont_t(
      fun (K) -> 
              run(CTA, 
                       fun (A) -> 
                               run(KCTB(A), K) 
                       end) 
      end).

-spec '>>'(cont_t(R, M, _A), cont_t(R, M, B)) -> cont_t(R, M, B).
'>>'(CTA, CTB, {?MODULE, _IM} = CT) ->
    monad:'default_>>'(CTA, CTB, CT).

-spec return(A, t(M)) -> cont_t(_R, M, A).
return(A, {?MODULE, _IM}) ->
    cont_t(fun (K) -> K(A) end).

-spec fail(any(), t(M)) -> cont_t(_R, M, _A).
fail(E, {?MODULE, IM}) ->
    cont_t(fun (_) -> monad_fail:fail(E, IM) end).

lift(X, {?MODULE, IM}) ->
    cont_t(fun (F) -> monad:'>>='(X, F, IM) end).

-spec callCC(fun((fun( (A) -> cont_t(R, M, _B) ))-> cont_t(R, M, A))) -> cont_t(R, M, A).
callCC(F, {?MODULE, _IM}) ->
    cont_t(fun (CC) -> run(F(fun(A) -> cont_t(fun(_) -> CC(A) end) end), CC) end).

-spec reset(cont_t(R, M, R), t(M)) -> cont_t(_NR, M, R).
reset(X, {?MODULE, IM}) ->
    lift(eval(X, {?MODULE, IM})).

-spec shift(fun((fun((A) -> monad:monadic(M, R))) -> cont_t(R, M, R)), t(M)) -> cont_t(R, M, A).
shift(F, {?MODULE, IM}) ->
    cont_t(fun (CC) -> eval(F(CC), {?MODULE, IM}) end).

ask({?MODULE, IM}) ->
    lift(monad_reader:ask(IM)).

local(F, X, {?MODULE, IM}) ->
    lift_local(fun() -> monad_reader:ask(IM) end, monad_reader:local(_, _, IM), F, X, {?MODULE, IM}).

reader(F, {?MODULE, IM}) ->
    lift(monad_reader:reader(F, IM)).

get({?MODULE, IM}) ->
    lift(monad_state:get(IM)).

put(S, {?MODULE, IM}) ->
    lift(monad_state:put(S, IM)).

state(F, {?MODULE, IM}) ->
    lift(monad_state:state(F, IM)).

-spec run(cont_t(R, M, A), fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R).
run(X, CC) ->
    (run_cont_t(X))(CC).

-spec eval(cont_t(R, M, R), t(M)) -> monad:monadic(M, R).
eval(X, {?MODULE, IM}) ->
    run(X, fun (A) -> monad:return(A, IM) end).

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

lift_local(Ask, Local, F, X, {?MODULE, IM}) ->
    cont_t(fun (CC) ->
                   do([IM || 
                          R <- Ask(),
                          Local(F, run(X, fun(A) -> Local(fun(_) -> R end, CC(A)) end))
                      ])
           end).

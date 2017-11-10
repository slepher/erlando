%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 June 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(cont_t).

-erlando_type(?MODULE).

-export_type([cont_t/3]).

-opaque cont_t(R, M, A) :: {cont_t, inner_t(R, M, A)}.
-type inner_t(R, M, A) :: fun((fun((A) -> monad:m(M, R))) -> monad:m(M, R)).
-type t(M) :: {cont_t, M}.

-compile({parse_transform, cut}).
-compile({parse_transform, do}).
-compile({parse_transform, monad_t_transform}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).

-include("op.hrl").
-include("erlando.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_cont).
-behaviour(monad_fail).
-behaviour(monad_runner).

-export([new/1, cont_t/1, run_cont_t/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([lift/2]).
-export([callCC/2]).
-export([fail/2]).
-export([run_nargs/0, run_m/2]).
-export([reset/2, shift/2]).
-export([map/3, with/3]).
-export([run/3, eval/2]).

-transform(#{inner_type => any,   behaviours => [functor, applicative, monad]}).
-transform(#{inner_type => monad, behaviours => [monad_trans, monad_cont]}).
-transform(#{args => monad,       functions => [shift/1, reset/1]}).
-transform(#{args => monad,       functions => [map/2, with/2]}).
-transform(#{args => monad,       functions => [run/2, eval/1]}).

-spec new(M) -> TM when TM :: monad:class(), M :: monad:class().
new(IM) ->
    {?MODULE, IM}.

-spec cont_t(inner_t(R, M, A)) -> cont_t(R, M, A).
cont_t(Inner) ->
    {?MODULE, Inner}.

-spec run_cont_t(cont_t(R, M, A)) -> inner_t(R, M, A).
run_cont_t({?MODULE, Inner}) ->
    Inner;
run_cont_t(#undetermined{} = U) ->
    run_cont_t(undetermined:run(U, cont_t));
run_cont_t(Other) ->
    exit({invalid_monad, Other}).

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

-spec shift(fun((fun((A) -> monad:m(M, R))) -> cont_t(R, M, R)), t(M)) -> cont_t(R, M, A).
shift(F, {?MODULE, IM}) ->
    cont_t(fun (CC) -> eval(F(CC), {?MODULE, IM}) end).

-spec map(fun((monad:m(M, R)) -> monad:m(M, R)), cont_t(R, M, A)) -> cont_t(R, M, A).
map(F, X, {?MODULE, _IM}) ->
    cont_t(fun(CC) -> F(run(X, CC)) end).

-spec with(fun((fun((B) -> monad:m(M, R))) -> fun((A) -> monad:m(M, R))), 
                cont_t(R, M, A)) -> cont_t(R, M, B).
with(F, X, {?MODULE, _IM}) ->
    cont_t(fun (CC) -> run(X, F(CC)) end).

-spec run(cont_t(R, M, A), fun((A) -> monad:m(M, R))) -> monad:m(M, R).
run(X, CC, {?MODULE, _IM}) ->
    (run_cont_t(X))(CC).

-spec eval(cont_t(R, M, R), t(M)) -> monad:m(M, R).
eval(X, {?MODULE, IM}) ->
    run(X, fun (A) -> monad:return(A, IM) end).

run_nargs() ->
    1.

run_m(CTA, [CC]) ->
    run(CTA, CC).

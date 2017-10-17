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

-include("op.hrl").

-define(CONT_T_MONAD, {?MODULE, monad}).

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_fail).
-behaviour(monad_trans).
-behaviour(monad_cont).
-behaviour(monad_state).
-behaviour(monad_reader).
-behaviour(monad_runner).

-export_type([cont_t/3]).

-export([cont_t/1, run_cont_t/1]).
-export([fmap/2]).
-export([ap/2, pure/1]).
-export(['>>='/2, return/1]).
-export([fail/1]).
-export([lift/1]).
-export([callCC/1]).
-export([shift/1, reset/1]).
-export([get/0, put/1, state/1]).
-export([ask/0, reader/1, local/2]).
-export([run_nargs/0, run/2]).

-export([run_cont/2, eval_cont/1, map_cont/2, with_cont/2]).
-export([lift_local/4]).

-export([new/1]).
% impl of functor 
-export([fmap/3]).
% impl of monad
-export(['>>='/3, return/2, fail/2]).
% impl of monad transformer
-export([lift/2]).
% impl of monad cont
-export([callCC/2, reset/2, shift/2]).
% cont functions
-export([run_cont/3, eval_cont/2, map_cont/3, with_cont/3]).
% lift other monad functions
-export([lift_local/5]).

-opaque cont_t(R, M, A) :: {cont_t, inner_cont_t(R, M, A)}.
-type inner_cont_t(R, M, A) :: fun((fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R)).
-type t(M) :: {cont_t, M}.

-spec cont_t(inner_cont_t(R, M, A)) -> cont_t(R, M, A).
cont_t(Inner) ->
    {?MODULE, Inner}.

-spec run_cont_t(cont_t(R, M, A)) -> inner_cont_t(R, M, A).
run_cont_t({?MODULE, Inner}) ->
    Inner;
run_cont_t({undetermined, _} = U) ->
    run_cont_t(undetermined:run(U, cont_t));
run_cont_t(Other) ->
    exit({invalid_monad, Other}).

-spec fmap(fun((A) -> B), cont_t(R, M, A)) -> cont_t(R, M, B).
fmap(F, CTA) ->
    cont_t(
      fun(CC) ->
              run_cont(CTA, fun(A) -> CC(F(A)) end)
      end).

ap(CTF, CTA) ->
    cont_t(
      fun(CC) ->
              run_cont(CTF, fun(F) -> run_cont(CTA, fun(A) -> CC(F(A)) end) end)
      end).

pure(A) ->
    return(A).

'>>='(CTA, Fun) ->
    cont_t(
      fun (K) -> 
              run_cont(CTA, 
                       fun (A) -> 
                               run_cont(Fun(A), K) 
                       end) 
      end).

return(A) ->
    cont_t(fun (K) -> K(A) end).

fail(E) ->
    fail(E, ?CONT_T_MONAD).

lift(X) ->
    cont_t(fun (F) -> monad:'>>='(X, F) end).

-spec callCC(fun((fun( (A) -> cont_t(R, M, _B) ))-> cont_t(R, M, A))) -> cont_t(R, M, A).
callCC(F) ->
    cont_t(fun (CC) -> run_cont(F(fun(A) -> cont_t(fun(_) -> CC(A) end) end), CC) end).

-spec reset(cont_t(R, M, R)) -> cont_t(_NR, M, R).
reset(X) ->
    lift(eval_cont(X)).

-spec shift(fun((fun((A) -> monad:monadic(M, R))) -> cont_t(R, M, R))) -> cont_t(R, M, A).
shift(F) ->
    cont_t(fun (CC) -> eval_cont(F(CC)) end).

get() ->
    lift(monad_state:get()).

put(S) ->
    lift(monad_state:put(S)).

state(F) ->
    lift(monad_state:state(F)).

-spec run_cont(cont_t(R, M, A), fun((A) -> monad:monadic(M, R))) -> monad:monadic(M, R).
run_cont(X, CC) ->
    (run_cont_t(X))(CC).

-spec eval_cont(cont_t(R, M, R)) -> monad:monadic(M, R).
eval_cont(X) ->
    run_cont(X, fun (A) -> monad:return(A) end).

-spec map_cont(fun((monad:monadic(M, R)) -> monad:monadic(M, R)), cont_t(R, M, A)) -> cont_t(R, M, A).
map_cont(F, X) ->
    cont_t(fun(CC) -> F(run_cont(X, CC)) end).

-spec with_cont(fun((fun((B) -> monad:monadic(M, R))) -> fun((A) -> monad:monadic(M, R))), 
                cont_t(R, M, A)) -> cont_t(R, M, B).
with_cont(F, X) ->
    cont_t(fun (CC) -> run_cont(X, F(CC)) end).

ask() ->
    lift(monad_reader:ask()).

reader(F) ->
    lift(monad_reader:reader(F)).

local(F, X) ->
    lift_local(fun() -> monad_reader:ask() end, monad_reader:local(_, _), F, X).

run_nargs() ->
    1.

run(CTA, [CC]) ->
    run_cont(CTA, CC).

lift_local(Ask, Local, F, X) ->
    cont_t(fun (CC) ->
                   do([monad || 
                          R <- Ask(),
                          Local(F, run_cont(X, fun(A) -> Local(fun(_) -> R end, CC(A)) end))
                      ])
           end).

%%----------------------------------------------------------------------------------------
%%
%% old style monad transformer functions below
%%
%%----------------------------------------------------------------------------------------

-spec new(M) -> TM when TM :: monad:monad(), M :: monad:monad().
new(IM) ->
    {?MODULE, IM}.

-spec fmap(fun((A) -> B), cont_t(R, M, A), t(M)) -> cont_t(R, M, B).
fmap(F, CTA, {?MODULE, _IM}) ->
    fmap(F, CTA).

-spec '>>='(cont_t(R, M, A), fun((A) -> cont_t(R, M, B)), t(M)) -> cont_t(R, M, B).
'>>='(CTA, Fun, {?MODULE, _IM}) ->
    '>>='(CTA, Fun).

-spec return(A, t(M)) -> cont_t(_R, M, A).
return(A, {?MODULE, _IM}) ->
    return(A).

-spec fail(any(), t(M)) -> cont_t(_R, M, _A).
fail(E, {?MODULE, IM}) ->
    cont_t(fun (_) -> monad:fail(E, IM) end).

-spec lift(monad:monadic(M, A), t(M)) -> cont_t(_R, M, A).
lift(X, {?MODULE, IM}) ->
    cont_t(fun (F) -> monad:'>>='(X, F, IM) end).

-spec callCC(fun((fun( (A) -> cont_t(R, M, _B) ))-> cont_t(R, M, A)), t(M)) -> cont_t(R, M, A).
callCC(F, {?MODULE, _IM}) ->
    callCC(F).

-spec reset(cont_t(R, M, R), t(M)) -> cont_t(_NR, M, R).
reset(X, {?MODULE, _IM} = CT) ->
    lift(eval_cont(X, CT), CT).

-spec shift(fun((fun((A) -> monad:monadic(M, R))) -> cont_t(R, M, R)), t(M)) -> cont_t(R, M, A).
shift(F, {?MODULE, _IM} = CT) ->
    cont_t(fun (CC) -> eval_cont(F(CC), CT) end).

-spec run_cont(cont_t(R, M, A), fun((A) -> monad:monadic(M, R)), t(M)) -> monad:monadic(M, R).
run_cont(X, CC, {?MODULE, _IM}) ->
    run_cont(X, CC).

-spec eval_cont(cont_t(R, M, R), t(M)) -> monad:monadic(M, R).
eval_cont(X, {?MODULE, IM}) ->
    run_cont(X, fun (A) -> monad:return(A, IM) end).

-spec map_cont(fun((monad:monadic(M, R)) -> monad:monadic(M, R)), cont_t(R, M, A), t(M)) -> cont_t(R, M, A).
map_cont(F, CTA, {?MODULE, _IM}) ->
    map_cont(F, CTA).

-spec with_cont(fun((fun((B) -> monad:monadic(M, R))) -> fun((A) -> monad:monadic(M, R))),
                cont_t(R, M, A), t(M)) -> cont_t(R, M, B).
with_cont(F, CTA, {?MODULE, _IM}) ->
    with_cont(F, CTA).

-spec lift_local(fun(() -> monad:monadic(M, R)),
                 fun((fun((R) -> R), monad:monadic(M, R)) -> monad:monadic(M, R)),
                 fun((R) -> R),
                 cont_t(R, M, A), t(M)) -> cont_t(R, M, A).
lift_local(Ask, Local, F, X, {?MODULE, IM}) ->
    cont_t(fun (CC) ->
                   do([IM ||
                          R <- Ask(),
                          Local(F, run_cont(X, fun(A) -> Local(fun(_) -> R end, CC(A)) end))
                      ])
           end).

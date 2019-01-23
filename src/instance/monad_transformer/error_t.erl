%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @Doc
%%%
%%% @end
%%% Created : 11 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(error_t).

-erlando_type(?MODULE).

-export_type([error_t/3]).

-opaque error_t(E, M, A) :: {error_t, inner_t(E, M, A)}.
-type inner_t(E, M, A) :: monad:m(M, either:either(E, A)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

-compile({parse_transform, do}).
-compile({parse_transform, cut}).
-compile({parse_transform, function_generator}).
-compile({no_auto_import, [get/0, get/1, put/1, put/2]}).


-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-include("op.hrl").
-include("erlando.hrl").

-export([new/1, error_t/1, run_error_t/1]).
% impl of functor.
-export([fmap/3, '<$'/3]).
% impl of applicative.
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
% impl of monad.
-export(['>>='/3, '>>'/3, return/2]).
% impl of monad_trans.
-export([lift/2]).
% impl of monad_fail.
-export([fail/2]).
-export([empty/1, '<|>'/3]).
-export([mzero/1, mplus/3]).
% impl of monad_runner.
-export([run_nargs/0, run_m/2]).
-export([map/3, with/3]).
-export([run/2]).

-gen_fun(#{inner_type => functior,   behaviours => [functor]}).
-gen_fun(#{inner_type => monad,      behaviours => [applicative]}).
-gen_fun(#{inner_type => monad,      behaviours => [monad, monad_trans, monad_fail]}).
-gen_fun(#{inner_type => monad_plus, behaviours => [alternative, monad_plus]}).
-gen_fun(#{args => monad,            functions => [map/2, with/2]}).
-gen_fun(#{args => monad,            functions => [run/1]}).

-spec new(M) -> t(M) when M :: monad:class().
new(M) ->
    {?MODULE, M}.

-spec error_t(inner_t(E, M, A)) -> error_t(E, M, A).
error_t(Inner) ->
    {?MODULE, Inner}.

-spec run_error_t(error_t(E, M, A)) -> inner_t(E, M, A).
run_error_t({?MODULE, Inner}) ->
    Inner;
run_error_t(#undetermined{} = UT) ->
    run_error_t(undetermined:run(UT, ?MODULE));
run_error_t(Other) ->
    exit({invalid_t, Other}).

-spec fmap(fun((A) -> B), error_t(E, M, A)) -> error_t(E, M, B).
fmap(F, ETA, {?MODULE, IM}) ->
    map(
      fun(FA) ->
              functor:fmap(either:fmap(F, _), FA, IM)
      end, ETA).

-spec '<$'(B, error_t(E, M, _A)) -> error_t(E, M, B).
'<$'(B, ETA, {?MODULE, _IM} = ET) ->
    functor:'default_<$'(B, ETA, ET).

-spec '<*>'(error_t(E, M, fun((A) -> B)), error_t(E, M, A)) -> error_t(E, M, B).
'<*>'(ETF, ETA, {?MODULE, IM}) ->
    error_t(
      do([IM || 
             EF <- run_error_t(ETF),
             case EF of
                 {left, _} ->
                     return(EF);
                 {right, F} ->
                     do([IM ||
                            EA <- ETA,
                            case EA of
                                {left, _} ->
                                    return(EA);
                                {right, A} ->
                                    return({right, F(A)})
                            end
                        ])
             end
         ])
     ).

-spec lift_a2(fun((A, B) -> C), error_t(E, M, A), error_t(E, M, B)) -> error_t(E, M, C).
lift_a2(F, ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:default_lift_a2(F, ETA, ETB, ET).

-spec '*>'(error_t(E, M, _A), error_t(E, M, B)) -> error_t(E, M, B).
'*>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:'default_*>'(ETA, ETB, ET).

-spec '<*'(error_t(E, M, A), error_t(E, M, _B)) -> error_t(E, M, A).
'<*'(ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:'default_<*'(ETA, ETB, ET).

pure(A, {?MODULE, _IM} = ET) ->
    return(A, ET).

-spec '>>='(error_t(E, M, A), fun( (A) -> error_t(E, M, B) )) -> error_t(E, M, B).
'>>='(ETA, KETB, {?MODULE, IM}) ->
    error_t(
      do([IM || EA <- run_error_t(ETA),
              case EA of
                  {left, _Err}    -> return(EA);
                  {right,  A}         -> run_error_t(KETB(A))
              end
       ])).

-spec '>>'(error_t(E, M, _A), error_t(E, M, B)) -> error_t(E, M, B).
'>>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    monad:'default_>>'(ETA, ETB, ET).

return(A, {?MODULE, IM}) ->
    error_t(monad:return(either:pure(A), IM)).

-spec lift(monad:m(M, A)) -> error_t(_E, M, A).
lift(MA, {?MODULE, IM}) ->
    error_t(functor:fmap(either:return(_), MA, IM)).

fail(E, {?MODULE, IM}) ->
    error_t(monad:return(either:fail(E), IM)).

empty({?MODULE, _IM} = ET) ->
    mzero(ET).

'<|>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    mplus(ETA, ETB, ET).

mplus(ETA, ETB, {?MODULE, IM}) ->
    error_t(
      do([IM ||
             EA <- run_error_t(ETA),
             case EA of
                 {left, _} ->
                     run_error_t(ETB);
                 _ ->
                     return(EA)
             end
         ])).

mzero({?MODULE, IM}) ->
    error_t(monad:return({left, mzero}, IM)).

run_nargs() ->
    0.

run_m(EM, []) ->
    run(EM).

-spec run(error_t(E, M, A)) -> monad:m(M, either:either(E, A)).
run(EM, {?MODULE, _IM}) -> 
    run_error_t(EM).

-spec map(fun((monad:m(M, either:either(EA, A))) -> monad:m(N, either:either(EB, B))),
                error_t(EA, M, A)) -> error_t(EB, N, B).
map(F, X, {?MODULE, _IM}) ->
    error_t(F(run_error_t(X))).

-spec with(fun((EA) -> EB), error_t(EA, M, A)) -> error_t(EB, M, A).
with(F, X, {?MODULE, _IM}) ->
    map(
      fun(MA) ->
              fun({left, R}) -> {left, F(R)}; (Val) -> Val end /'<$>'/ MA
      end, X).

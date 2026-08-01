%%%-------------------------------------------------------------------
%%% @doc Modern exception transformer. ErrorT is retained separately
%%%      for compatibility; conversions between the two are explicit.
%%% @end
%%%-------------------------------------------------------------------
-module(except_t).

-erlando_type(?MODULE).

-export_type([except_t/3]).

-opaque except_t(E, M, A) :: {except_t, inner_t(E, M, A)}.
-type inner_t(E, M, A) :: monad:m(M, either:either(E, A)).
-type t(M) :: monad_trans:monad_trans(?MODULE, M).

-include("do.hrl").
-compile({parse_transform, cut}).
-include("gen_fun.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(monad_error).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-include("erlando.hrl").

-export([new/1, except_t/1, run_except_t/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, '>>'/3, return/2]).
-export([lift/2]).
-export([fail/2, throw_error/2, catch_error/3]).
-export([empty/1, '<|>'/3, mzero/1, mplus/3]).
-export([run_nargs/0, run_m/2]).
-export([map/3, with/3, run/2]).
-export([map_except_t/2, with_except_t/2]).
-export([from_error_t/1, to_error_t/1]).

-gen_fun(#{inner_type => functor,    behaviours => [functor]}).
-gen_fun(#{inner_type => monad,      behaviours => [applicative]}).
-gen_fun(#{inner_type => monad,      behaviours => [monad, monad_trans, monad_fail, monad_error]}).
-gen_fun(#{inner_type => monad,      behaviours => [alternative, monad_plus]}).
-gen_fun(#{args => monad,            functions => [map/2, with/2]}).
-gen_fun(#{args => monad,            functions => [run/1]}).

-spec new(M) -> t(M) when M :: monad:class().
new(M) ->
    {?MODULE, M}.

-spec except_t(inner_t(E, M, A)) -> except_t(E, M, A).
except_t(Inner) ->
    {?MODULE, Inner}.

-spec run_except_t(except_t(E, M, A)) -> inner_t(E, M, A).
run_except_t({?MODULE, Inner}) ->
    Inner;
run_except_t(#undetermined{} = UT) ->
    run_except_t(undetermined:run(UT, ?MODULE));
run_except_t(Other) ->
    exit({invalid_t, Other}).

fmap(F, ETA, {?MODULE, IM}) ->
    map(fun(MEA) -> functor:fmap(either:fmap(F, _), MEA, IM) end, ETA).

'<$'(B, ETA, {?MODULE, _IM} = ET) ->
    functor:'default_<$'(B, ETA, ET).

'<*>'(ETF, ETA, {?MODULE, Applicative}) ->
    Apply = fun(EF, EA) -> either:'<*>'(EF, EA) end,
    except_t(applicative:lift_a2(Apply, run_except_t(ETF), run_except_t(ETA), Applicative)).

lift_a2(F, ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:default_lift_a2(F, ETA, ETB, ET).

'*>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:'default_*>'(ETA, ETB, ET).

'<*'(ETA, ETB, {?MODULE, _IM} = ET) ->
    applicative:'default_<*'(ETA, ETB, ET).

pure(A, {?MODULE, _IM} = ET) ->
    return(A, ET).

'>>='(ETA, KETB, {?MODULE, IM}) ->
    except_t(
      do([IM ||
             EA <- run_except_t(ETA),
             case EA of
                 {left, _Reason} -> return(EA);
                 {right, A} -> run_except_t(KETB(A))
             end
         ])).

'>>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    monad:'default_>>'(ETA, ETB, ET).

return(A, {?MODULE, IM}) ->
    except_t(monad:return({right, A}, IM)).

lift(MA, {?MODULE, IM}) ->
    except_t(functor:fmap(fun(A) -> {right, A} end, MA, IM)).

fail(E, {?MODULE, IM}) ->
    except_t(monad_fail:fail(E, IM)).

throw_error(E, {?MODULE, IM}) ->
    except_t(monad:return({left, E}, IM)).

catch_error(ETA, EMB, {?MODULE, IM}) ->
    except_t(
      do([IM ||
             EA <- run_except_t(ETA),
             case EA of
                 {left, Reason} -> run_except_t(EMB(Reason));
                 {right, _A} -> return(EA)
             end
         ])).

empty({?MODULE, _IM} = ET) ->
    mzero(ET).

'<|>'(ETA, ETB, {?MODULE, _IM} = ET) ->
    mplus(ETA, ETB, ET).

mplus(ETA, ETB, {?MODULE, IM}) ->
    except_t(
      do([IM ||
             EA <- run_except_t(ETA),
             case EA of
                 {left, ReasonA} ->
                     functor:fmap(
                       fun({left, ReasonB}) ->
                               {left, monoid:mappend(ReasonA, ReasonB)};
                          ({right, _B} = Success) ->
                               Success
                       end, run_except_t(ETB), IM);
                 {right, _A} -> return(EA)
             end
         ])).

mzero({?MODULE, IM}) ->
    except_t(monad:return({left, monoid:mempty()}, IM)).

run_nargs() ->
    0.

run_m(ETA, []) ->
    run_except_t(ETA).

run(ETA, {?MODULE, _IM}) ->
    run_except_t(ETA).

map(F, ETA, {?MODULE, _IM}) ->
    except_t(F(run_except_t(ETA))).

map_except_t(F, ETA) ->
    except_t(F(run_except_t(ETA))).

with(F, ETA, {?MODULE, IM} = ET) ->
    map(
      fun(MEA) ->
              functor:fmap(
                fun({left, Reason}) -> {left, F(Reason)};
                   ({right, _A} = Success) -> Success
                end, MEA, IM)
      end, ETA, ET).

with_except_t(F, ETA) ->
    except_t(
      functor:fmap(
        fun({left, Reason}) -> {left, F(Reason)};
           ({right, _A} = Success) -> Success
        end, run_except_t(ETA))).

from_error_t(ErrorT) ->
    except_t(error_t:run_error_t(ErrorT)).

to_error_t(ExceptT) ->
    error_t:error_t(run_except_t(ExceptT)).

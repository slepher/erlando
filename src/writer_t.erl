%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(writer_t).

-erlando_type({?MODULE, [writer_t/3]}).

-export_type([writer_t/3]).

-type writer_t(W, M, A) :: {writer_t, inner_writer_t(W, M, A)}.
-type inner_writer_t(W, M, A) :: monad:m(M, {A, monoid:m(W)}).
-type t(M) :: {writer_t, M}.

-compile({parse_transform, do}).
-compile({parse_transform, function_generator}).

-include("op.hrl").
-include("erlando.hrl").

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_writer).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-export([new/1, writer_t/1, run_writer_t/1]).
-export([fmap/3, '<$'/3]).
% impl of applicative
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
% impl of monad
-export(['>>='/3, '>>'/3, return/2]).
% impl of monad_trans
-export([lift/2]).
% impl of monad_writer
-export([writer/2, tell/2, listen/2, pass/2]).
% impl of alternative
-export([empty/1, '<|>'/3]).
% impl of monad_plus
-export([mzero/1, mplus/3]).
-export([run_nargs/0, run_m/2]).
-export([map/3]).
-export([exec/2, eval/2, run/2]).

-gen_fun(#{inner_type => functor,     behaviours => [functor]}).
-gen_fun(#{inner_type => applicative, behaviours => [applicative]}).
-gen_fun(#{inner_type => monad,       behaviours => [monad, monad_trans, monad_writer]}).
-gen_fun(#{inner_type => alternative, behaviours => [alternative]}).
-gen_fun(#{inner_type => monad_plus,  behaviours => [monad_plus]}).
-gen_fun(#{args => monad,             functions => [map/2]}).
-gen_fun(#{args => monad,             functions => [exec/1, eval/1, run/1]}).

-spec new(M) -> t(M) when M :: monad:class().
new(M) ->
    {?MODULE, M}.

-spec writer_t(inner_writer_t(W, M, A)) -> writer_t(W, M, A).
writer_t(Inner) ->
    {?MODULE, Inner}.

-spec run_writer_t(writer_t(W, M, A)) -> inner_writer_t(W, M, A).
run_writer_t({?MODULE, Inner}) ->
    Inner;
run_writer_t(#undetermined{} = U) ->
    run_writer_t(undetermined:run(U, ?MODULE));
run_writer_t(Other) ->
    exit({invalid_t, Other}).

-spec fmap(fun((A) -> B), writer_t(W, M, A)) -> writer_t(W, M, B).
fmap(F, WTA, {?MODULE, IM}) ->
    map(
      fun(FA) ->
              functor:fmap(fun({A, Ws}) ->  {F(A), Ws} end, FA, IM)
      end, WTA).

'<$'(B, FA, {?MODULE, _IM} = WT) ->
    functor:'default_<$'(B, FA, WT).

-spec '<*>'(writer_t(W, M, fun((A) -> B)), writer_t(W, M, A)) -> writer_t(W, M, B).
'<*>'(WTF, WTA, {?MODULE, IM}) ->
    AF = 
        fun({F, W1}) ->
                fun({A, W2}) ->
                        {F(A), monoid:mappend(W1, W2)}
                end
        end,
    writer_t(applicative:lift_a2(AF, run_writer_t(WTF), run_writer_t(WTA), IM)).

-spec lift_a2(fun((A, B) -> C), writer_t(W, M, A), writer_t(W, M, B)) -> writer_t(W, M, C).
lift_a2(F, WTA, WTB, {?MODULE, _IM} = WT) ->
    applicative:default_lift_a2(F, WTA, WTB, WT).

-spec '*>'(writer_t(W, M, _A), writer_t(W, M, B)) -> writer_t(W, M, B).
'*>'(WTA, WTB, {?MODULE, _IM} = WT) ->
    applicative:'default_*>'(WTA, WTB, WT).

-spec '<*'(writer_t(W, M, A), writer_t(W, M, _B)) -> writer_t(W, M, A).
'<*'(WTA, WTB, {?MODULE, _IM} = WT) ->
    applicative:'default_<*'(WTA, WTB, WT).

pure(A, {?MODULE, IM}) ->
    writer_t(applicative:pure({A, monoid:mempty()}, IM)).

-spec '>>='(writer_t(W, M, A), fun((A) -> writer_t(W, M, B) )) -> writer_t(W, M, B).
'>>='(WTA, KWTB, {?MODULE, IM}) ->
    writer_t(
      do([IM || {A, LogsA} <- run_writer_t(WTA),
                {B, LogsB} <- run_writer_t(KWTB(A)),
                return({B, monoid:mappend(LogsA, LogsB)})
       ])).

-spec '>>'(writer_t(W, M, _A), writer_t(W, M, B)) -> writer_t(W, M, B).
'>>'(WTA, WTB, {?MODULE, _IM} = WT) ->
    monad:'default_>>'(WTA, WTB, WT).

return(A, {?MODULE, IM}) ->
    writer_t(monad:return({A, monoid:mempty()}, IM)).

-spec lift(monad:m(M, A)) -> writer_t(_W, M, A).
lift(MA, {?MODULE, IM}) ->
    writer_t(monad:lift_m(fun(A) -> {A, monoid:mempty()} end, MA, IM)).

writer({A, Ws}, {?MODULE, IM}) ->
    writer_t(monad:return({A, Ws}, IM)).

tell(Ws, {?MODULE, IM}) ->
    writer_t(monad:return({ok, Ws}, IM)).

-spec listen(writer_t(W, M, A)) -> writer_t(W, M, {A, monoid:m(W)}).
listen(WTA, {?MODULE, IM}) ->
    map(
      fun(MA) ->
              monad:lift_m(fun({A, Ws}) -> {{A, Ws}, Ws} end, MA, IM)
      end, WTA).

-spec pass(writer_t(W, M, {A, fun((monoid:m(W)) -> monoid:m(W))})) -> writer_t(W, M, A).
pass(WTAK, {?MODULE, IM}) ->
    map(
      fun(MAK) ->
              monad:lift_m(fun({{A, F}, Ws}) -> {A, F(Ws)} end, MAK, IM)
      end, WTAK).

empty({?MODULE, IM}) ->
    writer_t(alternative:empty(IM)).

'<|>'(WTA, WTB, {?MODULE, IM}) ->
    writer_t(
      alternative:'<|>'(run_writer_t(WTA), run_writer_t(WTB), IM)
     ).

mzero({?MODULE, IM}) ->
    writer_t(monad_plus:mzero(IM)).

-spec mplus(writer_t(W, M, A), writer_t(W, M, A)) -> writer_t(W, M, A).
mplus(WTA, WTB, {?MODULE, IM}) ->
    writer_t(
      monad_plus:mplus(run_writer_t(WTA), run_writer_t(WTB), IM)
     ).

run_nargs() ->
    0.

run_m(WTA, []) ->
    run_writer_t(WTA).

-spec exec(writer_t(W, M, _A)) -> monad:m(M, monoid:m(W)).
exec(WTA, {?MODULE, IM}) ->
    do([IM || 
           {_A, Ws} <- run_writer_t(WTA),
           return(Ws)
       ]).

-spec eval(writer_t(_W, M, A)) -> monad:m(M, A).
eval(WTA, {?MODULE, IM}) ->
    do([IM || 
           {A, _Ws} <- run_writer_t(WTA),
           return(A)
       ]).

run(WTA, {?MODULE, _IM}) ->
    run_writer_t(WTA).

-spec map(fun((monad:m(M, {A, monoid:m(WA)})) -> monad:m(N, {B, monoid:m(WB)})),
                   writer_t(WA, M, A)) -> writer_t(WB, N, B).
map(F, X, {?MODULE, _IM}) ->
    writer_t(F(run_writer_t(X))).

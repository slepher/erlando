%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 11 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(writer_t).
-compile({parse_transform, do}).

-include("op.hrl").

-export_type([writer_t/3]).

-type writer_t(W, M, A) :: {writer_t, inner_writer_t(W, M, A)}.
-type inner_writer_t(W, M, A) :: monad:monadic(M, {A, [W]}).
-type t(M) :: {writer_t, M}.

-behaviour(type).
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_runner).

-export([new/1, writer_t/1, run_writer_t/1]).
-export([type/0]).
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export([pure/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([return/2, lift/1]).
-export([fail/1]).
-export([writer/1, tell/1, listen/1, listens/2, pass/1, censor/2]).
-export([writer/2, tell/2]).
-export([empty/0, '<|>'/2]).
-export([mzero/0, mplus/2]).
-export([run_nargs/0, run_m/2]).
-export([exec/1, eval/1, run/1, map/2]).


-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec writer_t(inner_writer_t(W, M, A)) -> writer_t(W, M, A).
writer_t(Inner) ->
    {?MODULE, Inner}.

-spec run_writer_t(writer_t(W, M, A)) -> inner_writer_t(W, M, A).
run_writer_t({?MODULE, Inner}) ->
    Inner;
run_writer_t({undetermined, _} = U) ->
    run_writer_t(undetermined:run(U, ?MODULE));
run_writer_t(Other) ->
    exit({invalid_t, Other}).

type() ->
    type:default_type(?MODULE).

-spec fmap(fun((A) -> B), writer_t(W, M, A)) -> writer_t(W, M, B).
fmap(F, WTA) ->
    map(
      fun(FA) ->
              fun({A, Ws}) ->  {F(A), Ws} end /'<$>'/ FA
      end, WTA).

'<$'(B, FA) ->
    functor:'default_<$'(B, FA, ?MODULE).

-spec pure(A) -> writer_t(_W, _M, A).
pure(A) ->
    pure(A, {?MODULE, applicative}).

-spec '<*>'(writer_t(W, M, fun((A) -> B)), writer_t(W, M, A)) -> writer_t(W, M, B).
'<*>'(WTF, WTA) ->
    AF = 
        fun({F, W1}) ->
                fun({A, W2}) ->
                        {F(A), W1 ++ W2}
                end
        end,
    writer_t(applicative:lift_a2(AF, run_writer_t(WTF), run_writer_t(WTA))).

-spec lift_a2(fun((A, B) -> C), writer_t(W, M, A), writer_t(W, M, B)) -> writer_t(W, M, C).
lift_a2(F, WTA, WTB) ->
    applicative:default_lift_a2(F, WTA, WTB, ?MODULE).

-spec '*>'(writer_t(W, M, _A), writer_t(W, M, B)) -> writer_t(W, M, B).
'*>'(WTA, WTB) ->
    applicative:'default_*>'(WTA, WTB, ?MODULE).

-spec '<*'(writer_t(W, M, A), writer_t(W, M, _B)) -> writer_t(W, M, A).
'<*'(WTA, WTB) ->
    applicative:'default_<*'(WTA, WTB, ?MODULE).

pure(A, {?MODULE, IM}) ->
    writer_t(applicative:pure({A, []}, IM)).

-spec '>>='(writer_t(W, M, A), fun((A) -> writer_t(W, M, B) )) -> writer_t(W, M, B).
'>>='(WTA, KWTB) -> 
    writer_t(
      do([monad || {A, LogsA} <- run_writer_t(WTA),
                   {B, LogsB} <- run_writer_t(KWTB(A)),
                   return({B, LogsA ++ LogsB})
       ])).

-spec '>>'(writer_t(W, M, _A), writer_t(W, M, B)) -> writer_t(W, M, B).
'>>'(WTA, WTB) ->
    monad:'default_>>'(WTA, WTB, ?MODULE).

-spec return(A) -> writer_t(_W, _M, A).
return(A) -> 
    return(A, {?MODULE, monad}).

return(A, {?MODULE, IM}) ->
    writer_t(monad:return({A, []}, IM)).

-spec lift(monad:monadic(M, A)) -> writer_t(_W, M, A).
lift(MA) ->
    writer_t(fun(A) -> {A, []} end /'<$>'/ MA).

-spec fail(any()) -> writer_t(_W, _M, _A).
fail(E) ->
    writer_t(monad_fail:fail(E)).

writer({A, Ws}) ->
    writer({A, Ws}, {?MODULE, monad}).

-spec tell([W]) -> writer_t(W, _M, ok).
tell(Ws) ->
    tell(Ws, {?MODULE, monad}).

-spec listen(writer_t(W, M, A)) -> writer_t(W, M, {A, [W]}).
listen(WTA) ->
    listens(fun(Ws) -> Ws end, WTA).

-spec listens(fun(([W]) -> B), writer_t(W, M, A)) -> writer_t(W, M, {A, B}).
listens(F, WTA) ->
    map(
      fun(MA) ->
              fun({A, Ws}) -> {{A, F(Ws)}, Ws} end /'<$>'/ MA
      end, WTA).

-spec pass(writer_t(W, M, {A, fun(([W]) -> [W])})) -> writer_t(W, M, A).
pass(WTAK) ->
    map(
      fun(MAK) ->
              fun({{A, F}, Ws}) -> {A, F(Ws)} end /'<$>'/ MAK
      end, WTAK).

-spec censor(fun(([W]) -> [W]), writer_t(W, M, A)) -> writer_t(W, M, A).
censor(F, WTA) ->
    map(
      fun(MA) ->
              fun({A, Ws}) -> {A, F(Ws)} end /'<$>'/ MA
      end, WTA).

writer({A, Ws}, {?MODULE, IM}) ->
    writer_t(monad:return({A, Ws}, IM)).

tell(Ws, {?MODULE, IM}) ->
    writer_t(monad:return({ok, Ws}, IM)).

empty() ->
    empty({?MODULE, alternative}).

'<|>'(WTA, WTB) ->
    writer_t(
      alternative:'<|>'(run_writer_t(WTA), run_writer_t(WTB))
     ).

empty({?MODULE, IM}) ->
    writer_t(alternative:empty(IM)).

-spec mzero() -> writer_t(_W, _M, _A).
mzero() ->
    mzero({?MODULE, monad_plus}).

-spec mplus(writer_t(W, M, A), writer_t(W, M, A)) -> writer_t(W, M, A).
mplus(WTA, WTB) ->
    writer_t(
      monad_plus:mplus(run_writer_t(WTA), run_writer_t(WTB))
     ).

mzero({?MODULE, IM}) ->
    writer_t(monad_plus:mzero(IM)).

run_nargs() ->
    0.

run_m(WTA, []) ->
    run_writer_t(WTA).

-spec exec(writer_t(W, M, _A)) -> monad:monadic(M, [W]).
exec(WTA) ->
    do([monad || 
           {_A, Ws} <- run_writer_t(WTA),
           return(Ws)
       ]).

-spec map(fun((monad:monadic(M, {A, [WA]})) -> monad:monadic(N, {B, [WB]})),
                   writer_t(WA, M, A)) -> writer_t(WB, N, B).
map(F, X) ->
    writer_t(F(run_writer_t(X))).

-spec eval(writer_t(_W, M, A)) -> monad:monadic(M, A).
eval(WTA) ->
    do([monad || 
           {A, _Ws} <- run_writer_t(WTA),
           return(A)
       ]).

run(WTA) ->
    run_writer_t(WTA).

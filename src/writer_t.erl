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

-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_fail).
-behaviour(alternative).
-behaviour(monad_plus).

-export([writer_t/1, run_writer_t/1]).

-export([fmap/2, '<$'/2]).
-export([ap/2, pure/1]).
-export(['>>='/2, return/1]).
-export([lift/1]).
-export([fail/1]).
-export([tell/1, listen/1, listens/2, pass/1, censor/2]).
-export([empty/0, '<|>'/2]).
-export([mzero/0, mplus/2]).
-export([exec_writer/1, map_writer/2]).
-export([run_writer/1]).

-export([new/1]).
-export([fmap/3]).
-export([mzero/1, mplus/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([tell/2, listen/2, listens/3, pass/2, censor/3]).
-export([exec_writer/2, map_writer/3]).
-export([run_writer/2]).

-spec writer_t(inner_writer_t(W, M, A)) -> writer_t(W, M, A).
writer_t(Inner) ->
    {?MODULE, Inner}.

-spec run_writer_t(writer_t(W, M, A)) -> inner_writer_t(W, M, A).
run_writer_t({?MODULE, Inner}) ->
    Inner;
run_writer_t({undetermined, _} = U) ->
    run_writer_t(undetermined:run(U, ?MODULE));
run_writer_t(Other) ->
    exit({invalid_writer_t, Other}).

-spec fmap(fun((A) -> B), writer_t(W, M, A)) -> writer_t(W, M, B).
fmap(F, WTA) ->
    map_writer(
      fun(FA) ->
              fun({A, Ws}) ->  {F(A), Ws} end /'<$>'/ FA
      end, WTA).

'<$'(B, FA) ->
    functor:'default_<$'(B, FA).

-spec ap(writer_t(W, M, fun((A) -> B)), writer_t(W, M, A)) -> writer_t(W, M, B).
ap(WTF, WTA) ->
    AF = 
        fun({F, W1}) ->
                fun({A, W2}) ->
                        {F(A), W1 ++ W2}
                end
        end,
    writer_t((AF /'<$>'/ run_writer(WTF)) /'<*>'/ run_writer_t(WTA)).

-spec pure(A) -> writer_t(_W, _M, A).
pure(A) ->
    return(A).

-spec '>>='(writer_t(W, M, A), fun((A) -> writer_t(W, M, B) )) -> writer_t(W, M, B).
'>>='(WTA, KWTB) -> 
    writer_t(
      do([monad || {A, LogsA} <- run_writer_t(WTA),
                   {B, LogsB} <- run_writer_t(KWTB(A)),
                   return({B, LogsA ++ LogsB})
       ])).

-spec return(A) -> writer_t(_W, _M, A).
return(A) -> 
    writer_t(monad:return({A, []})).

-spec lift(monad:monadic(M, A)) -> writer_t(_W, M, A).
lift(MA) ->
    writer_t(fun(A) -> {A, []} end /'<$>'/ MA).

-spec fail(any()) -> writer_t(_W, _M, _A).
fail(E) ->
    writer_t(monad_fail:fail(E)).

-spec tell([W]) -> writer_t(W, _M, ok).
tell(Ws) ->
    writer_t(monad:return({ok, Ws})).

-spec listen(writer_t(W, M, A)) -> writer_t(W, M, {A, [W]}).
listen(WTA) ->
    listens(fun(Ws) -> Ws end, WTA).

-spec listens(fun(([W]) -> B), writer_t(W, M, A)) -> writer_t(W, M, {A, B}).
listens(F, WTA) ->
    map_writer(
      fun(MA) ->
              fun({A, Ws}) -> {{A, F(Ws)}, Ws} end /'<$>'/ MA
      end, WTA).

-spec pass(writer_t(W, M, {A, fun(([W]) -> [W])})) -> writer_t(W, M, A).
pass(WTAK) ->
    map_writer(
      fun(MAK) ->
              fun({{A, F}, Ws}) -> {A, F(Ws)} end /'<$>'/ MAK
      end, WTAK).

-spec censor(fun(([W]) -> [W]), writer_t(W, M, A)) -> writer_t(W, M, A).
censor(F, WTA) ->
    map_writer(
      fun(MA) ->
              fun({A, Ws}) -> {A, F(Ws)} end /'<$>'/ MA
      end, WTA).

empty() ->
    mzero().

'<|>'(WTA, WTB) ->
    mplus(WTA, WTB).

-spec mzero() -> writer_t(_W, _M, _A).
mzero() ->
    writer_t(monad_plus:mzero()).

-spec mplus(writer_t(W, M, A), writer_t(W, M, A)) -> writer_t(W, M, A).
mplus(WTA, WTB) ->
    writer_t(
      monad_plus:mplus(run_writer_t(WTA), run_writer_t(WTB))
     ).

-spec exec_writer(writer_t(W, M, _A)) -> monad:monadic(M, [W]).
exec_writer(WTA) ->
    do([monad || 
           {_A, Ws} <- run_writer_t(WTA),
           return(Ws)
       ]).

-spec map_writer(fun((monad:monadic(M, {A, [WA]})) -> monad:monadic(N, {B, [WB]})),
                   writer_t(WA, M, A)) -> writer_t(WB, N, B).
map_writer(F, X) ->
    writer_t(F(run_writer_t(X))).

-spec run_writer(writer_t(_W, M, A)) -> monad:monadic(M, A).
run_writer(WTA) ->
    do([monad || 
           {A, _Ws} <- run_writer_t(WTA),
           return(A)
       ]).

%%----------------------------------------------------------------------------------------
%%
%% old monad trans functions below
%%
%%----------------------------------------------------------------------------------------
-spec new(M) -> t(M) when M :: monad:monad().
new(M) ->
    {?MODULE, M}.

-spec fmap(fun((A) -> B), writer_t(W, M, A), t(M)) -> writer_t(W, M, B).
fmap(F, WTA, {?MODULE, IM}) ->
    map_writer(
      fun(MA) ->
              do([IM ||
                     {A, Ws} <- MA,
                     return({F(A), Ws})
                 ])
      end, WTA).

-spec '>>='(writer_t(W, M, A), fun((A) -> writer_t(W, M, B) ), M) -> writer_t(W, M, B).
'>>='(WTA, KWTB, {?MODULE, IM}) ->
    writer_t(
      do([IM || {A, LogsA} <- run_writer_t(WTA),
                {B, LogsB} <- run_writer_t(KWTB(A)),
                return({B, LogsA ++ LogsB})
       ])).

-spec return(A, M) -> writer_t(_W, M, A).
return(A, {?MODULE, IM}) ->
    writer_t(monad:return({A, []}, IM)).

-spec fail(any(), M) -> writer_t(_W, M, _A).
fail(E, {?MODULE, IM}) ->
    writer_t(monad:fail(E, IM)).

-spec lift(monad:monadic(M, A), t(M)) -> writer_t(_W, M, A).
lift(MA, {?MODULE, IM}) ->
    do([IM ||
           A <- MA,
           return({A, []})
       ]).

-spec mzero(t(M)) -> writer_t(_W, M, _A).
mzero({?MODULE, IM}) ->
    writer_t(monad_plus:mzero(IM)).

-spec mplus(writer_t(W, M, A), writer_t(W, M, A), t(M)) -> writer_t(W, M, A).
mplus(WTA, WTB, {?MODULE, IM}) ->
    writer_t(
      monad_plus:mplus(IM, run_writer_t(WTA), run_writer_t(WTB))
     ).

-spec tell([W], t(M)) -> writer_t(W, M, ok).
tell(Ws, {?MODULE, IM}) ->
    writer_t(monad:return({ok, Ws}, IM)).

-spec listen(writer_t(W, M, A), t(M)) -> writer_t(W, M, {A, [W]}).
listen(WTA, {?MODULE, _IM} = WT) ->
    listens(fun(Ws) -> Ws end, WTA, WT).

-spec listens(fun(([W]) -> B), writer_t(W, M, A), t(M)) -> writer_t(W, M, {A, B}).
listens(F, WTA, {?MODULE, IM}) ->
    map_writer(
      fun(WIM) ->
              do([IM ||
                     {A, Ws} <- WIM,
                     return({{A, F(Ws)}, Ws})
                 ])
      end, WTA).

-spec pass(writer_t(W, M, {A, fun(([W]) -> [W])}), t(M)) -> writer_t(W, M, A).
pass(WTAK, {?MODULE, IM}) ->
    map_writer(
      fun(MAK) ->
              do([IM ||
                     {{A, F}, Ws} <- MAK,
                     return({A, F(Ws)})
                 ])
      end, WTAK).

-spec censor(fun(([W]) -> [W]), writer_t(W, M, A), t(M)) -> writer_t(W, M, A).
censor(F, WTA, {?MODULE, IM}) ->
    map_writer(
      fun(MA) ->
              do([IM ||
                     {A, Ws} <- MA,
                     return({A, F(Ws)})
                 ])
      end, WTA).

-spec exec_writer(writer_t(W, M, _A), t(M)) -> monad:monadic(M, [W]).
exec_writer(WTA, {?MODULE, IM}) ->
    do([IM ||
           {_, Ws} <- run_writer_t(WTA),
           return(Ws)
       ]).

-spec map_writer(fun((monad:monadic(M, {A, [WA]})) -> monad:monadic(N, {B, [WB]})),
                   writer_t(WA, M, A), t(M)) -> writer_t(WB, N, B).
map_writer(F, WTA, {?MODULE, _IM}) ->
    map_writer(F, WTA).

-spec run_writer(writer_t(_W, M, A), t(M)) -> monad:monadic(M, A).
run_writer(WTA, {?MODULE, IM}) ->
    do([IM || 
           {A, _Ws} <- run_writer_t(WTA),
           return(A)
       ]).

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

-export_type([writer_t/3]).

-type writer_t(W, M, A) :: {writer_t, inner_writer_t(W, M, A)}.
-type inner_writer_t(W, M, A) :: monad:monadic(M, {A, [W]}).
-type t(M) :: {writer_t, M}.


-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_plus_trans).
-export([new/1, writer_t/1, run_writer_t/1]).

-export([fmap/2]).
-export(['<*>'/2, pure/1]).
-export(['>>='/2, return/1]).
-export([lift/1]).
-export([fail/1]).
-export([tell/1, listen/1, listens/2, pass/1, censor/2]).
-export([exec_writer/1, map_writer/2]).

-export([mzero/1, mplus/3]).
-export(['>>='/3, return/2, fail/2, lift/2]).
-export([tell/2, listen/2, listens/3, pass/2, censor/3]).
-export([exec_writer/2, map_writer/3]).

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
    exit({invalid_writer_t, Other}).

-spec fmap(fun((A) -> B), writer_t(W, M, A)) -> writer_t(W, M, B).
fmap(F, X) ->
    map_writer(
      fun(WIM) ->
              functor:fmap(fun({A, Ws}) ->  {F(A), Ws} end, WIM)
      end, X).

'<*>'(AF, AA) ->
    do([monad ||
           {F, W1} <- AF,
           {A, W2} <- AA,
           {F(A), W1 ++ W2}
       ]).

pure(A) ->
    writer_t(applicative:pure({A, []})).

-spec '>>='(writer_t(W, M, A), fun((A) -> writer_t(W, M, B) )) -> writer_t(W, M, B).
'>>='(X, Fun) -> 
    writer_t(
      do([monad || {A, LogsA} <- run_writer_t(X),
                   {B, LogsB} <- run_writer_t(Fun(A)),
                   return({B, LogsA ++ LogsB})
       ])).

-spec return(A) -> writer_t(_W, _M, A).
return(A) -> 
    pure(A).

-spec lift(monad:monadic(M, A)) -> writer_t(_W, M, A).
lift(X) ->
    writer_t(functor:fmap(fun(A) -> {A, []} end, X)).

-spec fail(any()) -> writer_t(_W, _M, _A).
fail(E) ->
    writer_t(monad_fail:fail(E)).

-spec tell([W]) -> writer_t(W, _M, ok).
tell(X) ->
    writer_t(monad:return({ok, X})).

-spec listen(writer_t(W, M, A)) -> writer_t(W, M, {A, [W]}).
listen(X) ->
    listens(fun(Ws) -> Ws end, X).

-spec listens(fun(([W]) -> B), writer_t(W, M, A)) -> writer_t(W, M, {A, B}).
listens(F, X) ->
    map_writer(
      fun(WIM) ->
              functor:fmap(fun({A, Ws}) -> {{A, F(Ws)}, Ws} end, WIM)
      end, X).

-spec pass(writer_t(W, M, {A, fun(([W]) -> [W])})) -> writer_t(W, M, A).
pass(X) ->
    map_writer(
      fun(WIM) ->
              functor:fmap(fun({{A, F}, Ws}) -> {A, F(Ws)} end, WIM)
      end, X).

-spec censor(fun(([W]) -> [W]), writer_t(W, M, A)) -> writer_t(W, M, A).
censor(F, X) ->
    map_writer(
      fun(WIM) ->
              functor:fmap(fun({A, Ws}) -> A, F(Ws) end, WIM)
      end, X).

-spec exec_writer(writer_t(W, M, _A)) -> monad:monadic(M, [W]).
exec_writer(X) ->
    do([monad || 
           {_A, Ws} <- run_writer_t(X),
           return(Ws)
       ]).

-spec map_writer(fun((monad:monadic(M, {A, [WA]})) -> monad:monadic(N, {B, [WB]})),
                   writer_t(WA, M, A)) -> writer_t(WB, N, B).
map_writer(F, X) ->
    writer_t(F(run_writer_t(X))).

-spec lift(monad:monadic(M, A), t(M)) -> writer_t(_W, M, A).
lift(X, {?MODULE, IM}) ->
    do([IM || 
           A <- X,
           return({A, []})
       ]).

-spec mzero(t(M)) -> writer_t(_W, M, _A).
mzero({?MODULE, IM}) ->
    writer_t(monad_plus:mzero(IM)).

-spec mplus(writer_t(W, M, A), writer_t(W, M, A), t(M)) -> writer_t(W, M, A).
mplus(WA, WB, {?MODULE, IM}) ->
    writer_t(
      monad_plus:mplus(IM, run_writer_t(WA), run_writer_t(WB))
     ).

-spec '>>='(writer_t(W, M, A), fun((A) -> writer_t(W, M, B) ), t(M)) -> writer_t(W, M, B).
'>>='(X, Fun, {?MODULE, IM}) -> 
    writer_t(
      do([IM || {A, LogsA} <- run_writer_t(X),
                   {B, LogsB} <- run_writer_t(Fun(A)),
                   return({B, LogsA ++ LogsB})
       ])).

-spec return(A, t(M)) -> writer_t(_W, M, A).
return(A, {?MODULE, IM}) -> 
    writer_t(IM:return({A, []})).

-spec fail(any(), t(M)) -> writer_t(_W, M, _A).
fail(E, {?MODULE, IM}) ->
    writer_t(IM:fail(E)).
      
-spec tell([W], t(M)) -> writer_t(W, M, ok).
tell(X, {?MODULE, M}) ->
    writer_t(M:return({ok, X})).

-spec listen(writer_t(W, M, A), t(M)) -> writer_t(W, M, {A, [W]}).
listen(X, {?MODULE, _IM} = WT) ->
    listens(fun(Ws) -> Ws end, X, WT).

-spec listens(fun(([W]) -> B), writer_t(W, M, A), t(M)) -> writer_t(W, M, {A, B}).
listens(F, X, {?MODULE, IM} = WT) ->
    map_writer(
      fun(WIM) ->
              do([IM || 
                     {A, Ws} <- WIM,
                     return({{A, F(Ws)}, Ws})
                 ])
      end, X, WT).

-spec pass(writer_t(W, M, {A, fun(([W]) -> [W])}), t(M)) -> writer_t(W, M, A).
pass(X, {?MODULE, IM} = WT) ->
    map_writer(
      fun(WIM) ->
              do([IM ||
                     {{A, F}, Ws} <- WIM,
                     return({A, F(Ws)})
                 ])
      end, X, WT).

-spec censor(fun(([W]) -> [W]), writer_t(W, M, A), t(M)) -> writer_t(W, M, A).
censor(F, X, {?MODULE, IM} = WT) ->
    map_writer(
      fun(WIM) ->
              do([IM ||
                     {A, Ws} <- WIM,
                     return({A, F(Ws)})
                 ])
      end, X, WT).

-spec exec_writer(writer_t(W, M, _A), t(M)) -> monad:monadic(M, [W]).
exec_writer(X, {?MODULE, IM}) ->
    do([IM || 
           {_A, Ws} <- run_writer_t(X),
           return(Ws)
       ]).

-spec map_writer(fun((monad:monadic(M, {A, [WA]})) -> monad:monadic(N, {B, [WB]})),
                   writer_t(WA, M, A), t(M)) -> writer_t(WB, N, B).
map_writer(F, X, {?MODULE, _IM}) ->
    writer_t(F(run_writer_t(X))).

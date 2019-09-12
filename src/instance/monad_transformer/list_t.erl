%%%-------------------------------------------------------------------
%%% @Authorx Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(list_t).

-erlando_type({?MODULE, [list_t/2]}).

-export_type([list_t/2]).

-type list_t(M, A) :: {list_t, mlist(M, A)}.
-type mlist(M, A) :: monad:m(M, list(M, A)).
-type list(M, A) :: {cons, A, mlist(M, A)} | nil.

-include("do.hrl").
-include("gen_fun.hrl").
-include("erlando.hrl").
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(alternative).
-behaviour(monad_plus).
-behaviour(monad_fail).
-behaviour(monad_error).

%% API
-export([new/1]).
-export([list_t/1, run_list_t/1]).
-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, return/2]).
-export([fail/2]).
-export([lift/2]).
-export([mzero/1, mplus/3]).
-export([throw_error/2, catch_error/3]).
-export([map/3, from_list/2, lift_list/2, run/2]).

-gen_fun(#{inner_type => functor,      behaviours => [functor]}).
-gen_fun(#{inner_type => applicative,  behaviours => [applicative, alternative]}).
-gen_fun(#{inner_type => monad,        behaviours => [monad, monad_trans, monad_plus]}).
-gen_fun(#{inner_type => monad_fail,   behaviours => [monad_fail]}).
-gen_fun(#{inner_type => monad_error,  behaviours => [monad_error]}).
-gen_fun(#{args => monad,              functions  => [map/2]}).
-gen_fun(#{args => applicative,        functions  => [from_list/1, lift_list/1]}).
-gen_fun(#{args => monad,              functions  => [run/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(M) -> TM when TM :: monad:class(), M :: monad:class().
new(Inner) ->
    {?MODULE, Inner}.

-spec list_t(mlist(M, A)) -> list_t(M, A).
list_t(Inner) ->
    {?MODULE, Inner}.

-spec run_list_t(list_t(M, A)) -> mlist(M, A).
run_list_t({?MODULE, MListTA}) -> 
    MListTA;
run_list_t(#undetermined{} = U) ->
    run_list_t(undetermined:run(U, list_t));
run_list_t(Other) ->
    exit({invalid_monad, Other}).

-spec fmap(fun((A) -> B), list_t(M, A), functor:class()) -> list_t(M, B).
fmap(F, ListTA, {?MODULE, _Functor} = ListT) ->
    map(fun(MListA) -> fmap_flist(F, MListA, ListT) end, ListTA, ListT).

-spec '<$'(B, list_t(M, _A)) -> list_t(M, B).
'<$'(B, ListTA, {?MODULE, _Functor} = ListT) ->
    functor:'default_<$'(B, ListTA, ListT).

-spec pure(A, applicative:class()) -> list_t(_M, A).
pure(A, {?MODULE, Applicative}) ->
    list_t(applicative:pure(cons(A, applicative:pure(nil, Applicative)), Applicative)).

-spec '<*>'(list_t(M, fun((A) -> B)), list_t(M, A), applicative:class()) -> list_t(M, B).
'<*>'(ListTF, ListTA, {?MODULE, _Monad} = ListT) ->
    list_t(ap_flist(run_list_t(ListTF), run_list_t(ListTA), ListT)).

-spec lift_a2(fun((A, B) -> C), list_t(M, A), list_t(M, B), applicative:class()) -> list_t(M, C).
lift_a2(F, ListTA, ListTB, {?MODULE, _Applicative} = ListT) ->
    applicative:default_lift_a2(F, ListTA, ListTB, ListT).

-spec '*>'(list_t(M, _A), list_t(M, B), applicative:class()) -> list_t(M, B).
'*>'(LTA, LTB, {?MODULE, _Applicative} = ListT) ->
    applicative:'default_*>'(LTA, LTB, ListT).

-spec '<*'(list_t(M, A), list_t(M, _B), applicative:class()) -> list_t(M, A).
'<*'(LTA, LTB, {?MODULE, _Applicative} = ListT) ->
    applicative:'default_<*'(LTA, LTB, ListT).

-spec '>>='(list_t(M, A), fun((A) -> list_t(M, B)), monad:class()) -> list_t(M, B).
'>>='(ListTA, KListTB, {?MODULE, _Monad} = ListT) ->
    join(fmap(KListTB, ListTA, ListT), ListT).

-spec return(A, monad:class()) -> list_t(_M, A).
return(A, {?MODULE, _Monad} = ListT) ->
    monad:default_return(A, ListT).

-spec fail(_E, monad:class()) -> list_t(_M, _A).
fail(E, {?MODULE, MonadFail} = ListT) ->
    lift(monad_fail:fail(E, MonadFail), ListT).

-spec lift(monad:m(M, A), monad:class()) -> list_t(M, A).
lift(MA, {?MODULE, Monad}) ->
    list_t(functor:fmap(fun(A) -> cons(A, monad:return(nil(), Monad)) end, MA, Monad)).

empty({?MODULE, _Applicative} = ListT) ->
    from_list([], ListT).

-spec '<|>'(list_t(M, A), list_t(M, A), monad:class()) -> list_t(M, A).
'<|>'(ListTA, ListTB, {?MODULE, _Applicative} = ListT) ->
    list_t(append_flist(run_list_t(ListTA), run_list_t(ListTB), ListT)).

-spec mzero(monad:class()) -> list_t(_M, _A).
mzero({?MODULE, _Monad} = ListT) ->
    empty(ListT).

-spec mplus(list_t(M, A), list_t(M, A), monad:class()) -> list_t(M, A).
mplus(ListTA, ListTB, {?MODULE, _Monad} = ListT) ->
    '<|>'(ListTA, ListTB, ListT).

-spec throw_error(_E, monad:class()) -> list_t(_M, _A).
throw_error(E, {?MODULE, MonadError}) ->
    lift(monad_error:throw_error(E, MonadError), {?MODULE, MonadError}).

-spec catch_error(list_t(M, A), fun((_E) -> list_t(M, A)), monad:class()) -> list_t(M, A).
catch_error(ListTA, EListTB, {?MODULE, _MonadError} = ListT) ->
    MListA = run_list_t(ListTA),
    EMListB = fun(E) -> run_list_t(EListTB(E)) end,
    list_t(catch_error_mlist(MListA, EMListB, ListT)).

-spec map(fun((mlist(M, A)) -> mlist(M, B)), list_t(M, A), monad:class()) -> list_t(M, B).
map(F, LTA, {?MODULE, _Any}) ->
    list_t(F(run_list_t(LTA))).

-spec join(list_t(M, list_t(M, A)), monad:class()) -> list_t(M, A).
join(ListTListTA, {?MODULE, _Monad} = ListT) ->
    map(
      fun(MListListTA) -> 
              MListMListA = fmap_flist(fun run_list_t/1, MListListTA, ListT),
              join_mlist(MListMListA, ListT)
      end, ListTListTA).

-spec from_list([A], applicative:class()) -> list_t(_M, A).
from_list(List, ListT) ->
    list_t(from_list_1(List, ListT)).

from_list_1([], {?MODULE, Applicative}) ->
    applicative:pure(nil(), Applicative);
from_list_1([H|T], {?MODULE, Applicative} = ListT) ->
    ListA = cons(H, from_list_1(T, ListT)),
    applicative:pure(ListA, Applicative).

-spec lift_list([monad:m(M, A)], applicative:class()) -> list_t(M, A).
lift_list(List, ListT) ->
    list_t:list_t(lift_list_1(List, ListT)).

lift_list_1([], {?MODULE, Applicative}) ->
    applicative:pure(nil(), Applicative);
lift_list_1([FH|T], {?MODULE, Applicative} = ListT) ->
    functor:fmap(
      fun(H) ->
              cons(H, lift_list_1(T, ListT))
      end, FH, Applicative).

-spec run(list_t(M, A), functor:class()) -> monad:m(M, [A]).
run(ListTA, {?MODULE, _Monad} = ListT) ->
    run_mlist(run_list_t(ListTA), ListT).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec fmap_flist(fun((A) -> B), mlist(M, A), functor:class()) -> mlist(M, B).
fmap_flist(F, FListA, {?MODULE, Functor} = ListT) ->
    functor:fmap(fun(ListA) -> fmap_list(F, ListA, ListT) end, FListA, Functor).

-spec fmap_list(fun((A) -> B), list(M, A), functor:class()) -> mlist(M, B).
fmap_list(F, {cons, A, MListA}, {?MODULE, _Functor} = ListT) ->
    cons(F(A), fmap_flist(F, MListA, ListT));
fmap_list(_F, nil, {?MODULE, _Functor}) ->
    nil().

-spec ap_flist(mlist(F, fun((A) -> B)), mlist(F, A), applicative:class()) -> mlist(F, B).
ap_flist(FListF, FListA, {?MODULE, Applicative} = ListT) ->
    FAB = fun(ListF, ListA) -> ap_list(ListF, ListA, ListT) end,
    applicative:lift_a2(FAB, FListF, FListA, Applicative).

-spec ap_list(list(F, fun((A) -> B)), list(F, A), applicative:class()) -> list(F, B).
ap_list(_ListA, nil, {?MODULE, _Applicative}) ->
    nil();
ap_list(nil, _ListA, {?MODULE, _Applicative}) ->
    nil();
ap_list({cons, F, FListF}, {cons, A, FListA} = ListA, {?MODULE, Applicative} = ListT) ->
    FListFA = fmap_flist(F, FListA, ListT),
    FListFB = ap_flist(FListF, applicative:pure(ListA, Applicative), ListT),
    cons(F(A), append_flist(FListFA, FListFB, ListT)).

-spec join_mlist(mlist(M, mlist(M, A)), monad:class()) -> mlist(M, A).
join_mlist(MListMListA, {?MODULE, Monad} = ListT) ->
    monad:'>>='(MListMListA, fun(ListMListA) -> join_list(ListMListA, ListT) end, Monad).

-spec join_list(list(M, mlist(M, A)), monad:class()) -> mlist(M, A).
join_list({cons, MListA, MListMListA}, {?MODULE, _Monad} = ListT) ->
    append_flist(MListA, join_mlist(MListMListA, ListT), ListT);
join_list(nil, {?MODULE, Monad}) ->
    monad:return(nil, Monad).

-spec append_flist(mlist(M, A), mlist(M, A), monad:class()) -> mlist(M, A).
append_flist(FListA, FListB, {?MODULE, Applicative} = ListT) ->
    FAB = fun(ListA, ListB) -> append_list(ListA, ListB, ListT) end,
    applicative:lift_a2(FAB, FListA, FListB, Applicative).

-spec append_list(list(M, A), list(M, A), monad:class()) -> list(M, A).
append_list({cons, A, FListA}, ListB, {?MODULE, Applicative} = ListT) ->
    cons(A, append_flist(FListA, applicative:pure(ListB, Applicative), ListT));
append_list(nil, ListB, {?MODULE, _Monad}) ->
    ListB.

-spec catch_error_mlist(mlist(M, A), fun((_E) -> mlist(M, A)), monad:class()) -> mlist(M, A).
catch_error_mlist(MListA, EMListB, {?MODULE, MonadError} = ListT) ->
   MListB = monad_error:catch_error(MListA, EMListB, MonadError),
   functor:fmap(fun(ListB) -> catch_error_list(ListB, EMListB, ListT) end, MListB, MonadError).

-spec catch_error_list(list(M, A), fun((_E) -> mlist(M, A)), monad:class()) -> list(M, A).
catch_error_list({cons, A, MListA}, EMListB, {?MODULE, _MonadError} = ListT) ->
   cons(A, catch_error_mlist(MListA, EMListB, ListT));
catch_error_list(nil, _EMConsB, {?MODULE, _MonadError}) ->
   nil().

run_mlist(MListA, {?MODULE, Monad} = ListT) ->
    monad:'>>='(MListA, fun(ListA) -> run_list(ListA, ListT) end, Monad).

run_list({cons, A, MListA}, {?MODULE, Monad} = ListT) ->
    functor:fmap(fun(T) -> [A|T] end, run_mlist(MListA, ListT), Monad);
run_list(nil, {?MODULE, Monad}) ->
    monad:return([], Monad).

-spec cons(A, mlist(M, A)) -> list(M, A).
cons(A, MListA) ->
    {cons, A, MListA}.

-spec nil() -> list(_M, _A).
nil() ->
    nil.

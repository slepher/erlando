%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(list_t).

-erlando_type({?MODULE, [list_t/3]}).

-type list_t(S, M, A) :: {list_t, monad:m(M, inner_t(S, M, A))}.
-type inner_t(S, M, A) :: {cons, A, monad:m(list_t(S, M, A))} | nil.

-compile({no_auto_import, [get/1, put/2]}).

-include("do.hrl").
-include("gen_fun.hrl").
-include("erlando.hrl").
-behaviour(functor).
-behaviour(applicative).
-behaviour(monad).
-behaviour(monad_trans).
-behaviour(monad_plus).
-behaviour(monad_fail).
-behaviour(monad_error).

%% API
-export([new/1]).

-export([fmap/3, '<$'/3]).
-export([pure/2, '<*>'/3, lift_a2/4, '*>'/3, '<*'/3]).
-export(['>>='/3, return/2]).
-export([fail/2]).
-export([lift/2]).
-export([mzero/1, mplus/3]).
-export([throw_error/2, catch_error/3]).
-export([map/3, lift_list/2]).

-gen_fun(#{inner_type => functor,      behaviours => [functor]}).
-gen_fun(#{inner_type => applicative,  behaviours => [applicative]}).
-gen_fun(#{inner_type => monad,        behaviours => [monad, monad_trans, monad_plus]}).
-gen_fun(#{inner_type => monad_fail,   behaviours => [monad_fail]}).
-gen_fun(#{inner_type => monad_error,  behaviours => [monad_error]}).
-gen_fun(#{args => monad,              functions  => [map/2]}).
-gen_fun(#{args => monad,              functions  => [lift_list/1]}).

%%%===================================================================
%%% API
%%%===================================================================

-spec new(M) -> TM when TM :: monad:class(), M :: monad:class().
new(Inner) ->
    {?MODULE, Inner}.

list_t(Inner) ->
    {?MODULE, Inner}.

run_list_t({?MODULE, MListTA}) -> 
    MListTA;
run_list_t(#undetermined{} = U) ->
    run_list_t(undetermined:run(U, list_t));
run_list_t(Other) ->
    exit({invalid_monad, Other}).

fmap(F, LTA, {?MODULE, IM}) ->
    MConsA = run_list_t(LTA),
    list_t(
      functor:fmap(
        fun(ConsA) ->
                fmap_cons(F, ConsA, {?MODULE, IM})
        end, MConsA, IM)).

'<$'(B, FA, {?MODULE, _IM} = MT) ->
    functor:'default_<$'(B, FA, MT).

pure(A, {?MODULE, IM}) ->
    list_t(applicative:pure({cons, A, applicative:pure(nil, IM)}, IM)).

'<*>'(LTF, LTA, {?MODULE, IM}) ->
    list_t(ap_mcons(run_list_t(LTF), run_list_t(LTA), {?MODULE, IM})).

lift_a2(F, LTA, LTB, {?MODULE, IM}) ->
    applicative:default_lift_a2(F, LTA, LTB, {?MODULE, IM}).

'*>'(LTA, LTB, {?MODULE, IM}) ->
    applicative:'default_*>'(LTA, LTB, {?MODULE, IM}).

'<*'(LTA, LTB, {?MODULE, IM}) ->
    applicative:'default_<*'(LTA, LTB, {?MODULE, IM}).

'>>='(LTA, KLTB, {?MODULE, IM}) ->
    join(fmap(KLTB, LTA, {?MODULE, IM}), {?MODULE, IM}).

return(A, {?MODULE, IM}) ->
    monad:default_return(A, {?MODULE, IM}).

fail(E, {?MODULE, MonadFail}) ->
    lift(monad_fail:fail(E, MonadFail), {?MODULE, MonadFail}).

lift(MA, {?MODULE, IM}) ->
    list_t(functor:fmap(fun(A) -> {cons, A, monad:return(nil, IM)} end, MA, IM)).

mzero({?MODULE, IM}) ->
    lift_list([], {?MODULE, IM}).

mplus(ListTA, ListTB, {?MODULE, IM}) ->
    list_t(mappend_mcons(run_list_t(ListTA), run_list_t(ListTB), {?MODULE, IM})).

throw_error(E, {?MODULE, MonadError}) ->
    lift(monad_error:throw_error(E, MonadError), {?MODULE, MonadError}).

catch_error(LTA, ELTB, {?MODULE, MonadError}) ->
    EMConsB = fun(E) -> run_list_t(ELTB(E)) end,
    list_t(catch_error_mcons(run_list_t(LTA), EMConsB, {?MODULE, MonadError})).

map(F, LTA, {?MODULE, _IM}) ->
    list_t(F(run_list_t(LTA))).

join(ListTListTA, {?MODULE, IM}) ->
    MConsListTA = run_list_t(ListTListTA),
    MConsMCons = 
        functor:fmap(
          fun(ConsListTA) -> 
                  fmap_cons(fun run_list_t/1, ConsListTA, {?MODULE, IM})
          end, MConsListTA),
    list_t(join_mcons(MConsMCons, {?MODULE, IM})).

lift_list([], {?MODULE, IM}) ->
    list_t(monad:return(nil, IM));
lift_list([H|T], {?MODULE, IM}) ->
    list_t(monad:return({cons, H, run_list_t(lift_list(T, {?MODULE, IM}))}, IM)).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
fmap_mcons(F, MConsA, {?MODULE, Functor}) ->
    functor:fmap(fun(ConsA) -> fmap_cons(F, ConsA, {?MODULE, Functor}) end, MConsA, Functor).

fmap_cons(F, {cons, A, MConsA}, {?MODULE, Functor}) ->
    {cons, F(A), fmap_mcons(F, MConsA, {?MODULE, Functor})};
fmap_cons(_F, nil, {?MODULE, _Functor}) ->
    nil.

ap_mcons(MConsF, MConsA, {?MODULE, IM}) ->
    functor:fmap(fun(ConsF) -> ap_cons(ConsF, MConsA, {?MODULE, IM}) end, MConsF, {?MODULE, IM}).

ap_cons({cons, F, MConsFA}, MConsA, {?MODULE, Applicative}) ->
    mappend_mcons(fmap_mcons(F, MConsA, {?MODULE, Applicative}), 
                  ap_mcons(MConsFA, MConsA, {?MODULE, Applicative}), {?MODULE, Applicative});
ap_cons(nil, _MConsA, {?MODULE, Applicative}) ->
    applicative:pure(nil, Applicative).

join_mcons(MConsMConsA, {?MODULE, IM}) ->
    monad:'>>='(MConsMConsA, fun(MConsA) -> join_cons(MConsA, {?MODULE, IM}) end, IM).

join_cons({cons, MConsA, MConsMConsA}, {?MODULE, IM}) ->
    mappend_mcons(MConsA, join_mcons(MConsMConsA, {?MODULE, IM}), {?MODULE, IM});
join_cons(nil, {?MODULE, IM}) ->
    return(nil, {?MODULE, IM}).

mappend_mcons(MConsA, MConsB, {?MODULE, IM}) ->
    monad:'>>='(MConsA, fun(ConsA) -> mappend_cons(ConsA, MConsB, {?MODULE, IM}) end, IM).

mappend_cons({cons, A, MConsA}, MConsB, {?MODULE, IM}) ->    
    monad:return({cons, A, mappend_mcons(MConsA, MConsB, {?MODULE, IM})}, IM);
mappend_cons(nil, MConsB, {?MODULE, _IM}) -> 
    MConsB.

catch_error_mcons(MConsA, EMConsB, {?MODULE, MonadError}) ->
   functor:fmap(fun(ConsA) -> catch_error_cons(ConsA, EMConsB, {?MODULE, MonadError}) end, MConsA, MonadError).

catch_error_cons({cons, A, MConsA}, EMConsB, {?MODULE, MonadError}) ->
   {cons, A, catch_error_mcons(MConsA, EMConsB, {?MODULE, MonadError})};
catch_error_cons(nil, _EMConsB, {?MODULE, _MonadError}) ->
   nil.

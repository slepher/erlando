%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(applicative).

-superclass([functor]).

-export_type([applicative_module/0, f/1, applicative/2]).

-type applicative_module() :: {module(), applicative_module()} | module().
-type f(_A) :: any().
-type applicative(_F, _A) :: any().

-callback pure(A, _F) -> f(A).
-callback '<*>'(applicative(F, fun((A) -> B)), applicative(F, A), F) -> applicative(F, B).
-callback lift_a2(fun((A, B) -> C), applicative(F, A), applicative(F, B), F) -> applicative(F, C).
-callback '*>'(applicative(F, _A), applicative(F, B), F) -> applicative(F, B).
-callback '<*'(applicative(F, A), applicative(F, _B), F) -> applicative(F, A).

-compile({parse_transform, monad_t_transform}).

-include("op.hrl").
-include("functor.hrl").

%% API
-export([pure/2, '<*>'/3, lift_a2/4, '<*'/3, '*>'/3]).
-export(['default_<*>'/3, default_lift_a2/4, 'default_<*'/3, 'default_*>'/3]).
-export([ap/3, '<**>'/3, lift_a3/5]).

-transform(#{args => [?MODULE], functions => [pure/1, '<*>'/2, lift_a2/3, '<*'/2, '*>'/2]}).
-transform(#{args => [?MODULE], functions => [ap/2, '<**>'/2, lift_a3/4]}).

%%%===================================================================
%%% API
%%%===================================================================
pure(A, UApplicative) ->
    undetermined:new(
      fun(Applicative) ->
              typeclass_trans:apply(pure, [A], Applicative, ?MODULE)
      end, UApplicative).

-spec '<*>'(applicative(F, fun((A) -> B)), applicative(F, A), F) -> applicative(F, B).
'<*>'(UF, UA, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AF, AA) ->
              'do_<*>'(AF, AA, Applicative)
      end, UF, UA, UApplicative).

lift_a2(F, UA, UB, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AA, AB) ->
              do_lift_a2(F, AA, AB, Applicative)
      end, UA, UB, UApplicative).

-spec '*>'(applicative(F, _A), applicative(F, B)) -> applicative(F, B).
'*>'(UA, UB, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AA, AB) ->
              typeclass_trans:apply('*>', [AA, AB], Applicative, ?MODULE)
      end, UA, UB, UApplicative).

-spec '<*'(applicative(F, A), applicative(F, _B)) -> applicative(F, A).
'<*'(UA, UB, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AA, AB) ->
              typeclass_trans:apply('<*', [AB, AA], Applicative, ?MODULE)
      end, UA, UB, UApplicative).

'default_<*>'(AF, AA, Applicative) ->
    FA = 
        fun(F, A) ->
                F(A)
        end,
    lift_a2(FA, AF, AA, Applicative).

-spec default_lift_a2(fun((A, B) -> C), applicative(F, A), applicative(F, B), module()) -> applicative(F, C).
default_lift_a2(F, AA, AB, Applicative) ->
    NF = 
        fun(A) ->
                fun(B) ->
                        F(A, B)
                end
        end,
    AF = applicative:pure(NF, Applicative),
    'do_<*>'('do_<*>'(AF, AA, Applicative), AB, Applicative).

-spec 'default_*>'(applicative(F, _A), applicative(F, B), module()) -> applicative(F, B).
'default_*>'(AA, AB, Applicative) ->
    ConstId = 
        fun(_A) ->
                fun(B) -> B end
        end,
    do_lift_a2(ConstId, AA, AB, Applicative).

-spec 'default_<*'(applicative(F, A), applicative(F, _B), module()) -> applicative(F, A).
'default_<*'(AA, AB, Applicative) ->
    Const = 
        fun(A) -> 
                fun(_B) -> A end
        end,
    do_lift_a2(Const, AA, AB, Applicative).

-spec 'ap'(applicative(F, fun((A) -> B)), applicative(F, A), F) -> applicative(F, B).
ap(AF, A, Applicative) ->
    '<*>'(AF, A, Applicative).

-spec '<**>'(applicative(F, A), applicative(F, fun((A) -> B)), F) -> applicative(F, B).
'<**>'(AA, AF, Applicative) ->
    '<*>'(AF, AA, Applicative).

-spec lift_a3(fun((A, B, C) -> D), applicative(F, A), applicative(F, B), applicative(F, C), F) -> applicative(F, D).
lift_a3(F, AA, AB, AC, Applicative) ->
    NF = 
        fun(A) ->
                fun(B) ->
                        fun(C) -> F(A, B, C) end
                end
        end,
    '<*>'(lift_a2(NF, AA, AB, Applicative), AC, Applicative).

%%%===================================================================
%%% Internal functions
%%%===================================================================
'do_<*>'(AF, AA, Applicative) ->
    typeclass_trans:apply('<*>', [AF, AA], Applicative, ?MODULE).

do_lift_a2(F, AA, AB, Applicative) ->
    typeclass_trans:apply(lift_a2, [F, AA, AB], Applicative, ?MODULE).

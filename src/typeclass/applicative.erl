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

-export_type([class/0, f/2]).
-export_type([applicative_module/0, applicative/2]).

-type applicative_module() :: class().
-type applicative(F, A) :: f(F, A).

-type class() :: {module(), class()} | module().
-type f(_F, _A) :: any().

-callback pure(A, F) -> applicative:f(F, A) when F :: applicative:class(). 
-callback '<*>'(applicative:f(F, fun((A) -> B)), applicative:f(F, A), F) -> applicative:f(F, B) when F :: applicative:class(). 
-callback lift_a2(fun((A, B) -> C), applicative:f(F, A), applicative:f(F, B), F) -> applicative:f(F, C) when F :: applicative:class(). 
-callback '*>'(applicative:f(F, _A), applicative:f(F, B), F) -> applicative:f(F, B) when F :: applicative:class(). 
-callback '<*'(applicative:f(F, A), applicative:f(F, _B), F) -> applicative:f(F, A) when F :: applicative:class(). 

-include("gen_fun.hrl").

-include("op.hrl").
-include("functor.hrl").

%% API
-export([pure/2, '<*>'/3, lift_a2/4, '<*'/3, '*>'/3]).
-export(['default_<*>'/3, default_lift_a2/4, 'default_<*'/3, 'default_*>'/3]).
-export([ap/3, '<**>'/3, lift_a3/5]).

-gen_fun(#{args => [?MODULE], functions => [pure/1, '<*>'/2, lift_a2/3, '<*'/2, '*>'/2]}).
-gen_fun(#{args => [?MODULE], functions => [ap/2, '<**>'/2, lift_a3/4]}).

%%%===================================================================
%%% API
%%%===================================================================
-spec pure(A, F) -> applicative:f(F, A) when F :: applicative:class(). 
pure(A, UApplicative) ->
    undetermined:new(
      fun(Applicative) ->
              typeclass_trans:apply(pure, [A], Applicative, ?MODULE)
      end, UApplicative).

-spec '<*>'(applicative:f(F, fun((A) -> B)), applicative:f(F, A), F) -> applicative:f(F, B) when F :: applicative:class(). 
'<*>'(UF, UA, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AF, AA) ->
              'do_<*>'(AF, AA, Applicative)
      end, UF, UA, UApplicative).

-spec lift_a2(fun((A, B) -> C), applicative:f(F, A), applicative:f(F, B), F) -> 
                     applicative:f(F, C) when F :: applicative:class(). 
lift_a2(F, UA, UB, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AA, AB) ->
              do_lift_a2(F, AA, AB, Applicative)
      end, UA, UB, UApplicative).

-spec '*>'(applicative:f(F, _A), applicative:f(F, B), F) -> applicative:f(F, B) when F :: applicative:class(). 
'*>'(UA, UB, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AA, AB) ->
              typeclass_trans:apply('*>', [AA, AB], Applicative, ?MODULE)
      end, UA, UB, UApplicative).

-spec '<*'(applicative:f(F, A), applicative:f(F, _B), F) -> applicative:f(F, A) when F :: applicative:class(). 
'<*'(UA, UB, UApplicative) ->
    undetermined:map_pair(
      fun(Applicative, AA, AB) ->
              typeclass_trans:apply('<*', [AA, AB], Applicative, ?MODULE)
      end, UA, UB, UApplicative).

'default_<*>'(AF, AA, Applicative) ->
    FA = 
        fun(F, A) ->
                F(A)
        end,
    lift_a2(FA, AF, AA, Applicative).

-spec default_lift_a2(fun((A, B) -> C), applicative:f(F, A), applicative:f(F, B), F) -> 
                             applicative:f(F, C) when F :: applicative:class(). 
default_lift_a2(F, AA, AB, Applicative) ->
    NF = 
        fun(A) ->
                fun(B) ->
                        F(A, B)
                end
        end,
    AF = applicative:pure(NF, Applicative),
    'do_<*>'('do_<*>'(AF, AA, Applicative), AB, Applicative).

-spec 'default_*>'(applicative:f(F, _A), applicative:f(F, B), F) -> applicative:f(F, B) when F :: applicative:class(). 
'default_*>'(AA, AB, Applicative) ->
    ConstId = 
        fun(_A) ->
                fun(B) -> B end
        end,
    do_lift_a2(ConstId, AA, AB, Applicative).

-spec 'default_<*'(applicative:f(F, A), applicative:f(F, _B), F) -> applicative:f(F, A) when F :: applicative:class(). 
'default_<*'(AA, AB, Applicative) ->
    Const = 
        fun(A) -> 
                fun(_B) -> A end
        end,
    do_lift_a2(Const, AA, AB, Applicative).

-spec 'ap'(applicative:f(F, fun((A) -> B)), applicative:f(F, A), F) -> applicative:f(F, B) when F :: applicative:class(). 
ap(AF, A, Applicative) ->
    '<*>'(AF, A, Applicative).

-spec '<**>'(applicative:f(F, A), applicative:f(F, fun((A) -> B)), F) -> applicative:f(F, B) when F :: applicative:class(). 
'<**>'(AA, AF, Applicative) ->
    '<*>'(AF, AA, Applicative).

-spec lift_a3(fun((A, B, C) -> D), applicative:f(F, A), applicative:f(F, B), applicative:f(F, C), F) -> 
                     applicative:f(F, D) when F :: applicative:class(). 
lift_a3(F, AA, AB, AC, Applicative) ->
    NF = 
        fun(A, B) ->
                fun(C) -> F(A, B, C) end
        end,
    '<*>'(lift_a2(NF, AA, AB, Applicative), AC, Applicative).

%%%===================================================================
%%% Internal functions
%%%===================================================================
'do_<*>'(AF, AA, Applicative) ->
    typeclass_trans:apply('<*>', [AF, AA], Applicative, ?MODULE).

do_lift_a2(F, AA, AB, Applicative) ->
    typeclass_trans:apply(lift_a2, [F, AA, AB], Applicative, ?MODULE).

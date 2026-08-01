%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(either).

-export_type([either/2]).

-type either(L, R) :: {left, L} | {right, R}.

-compile({parse_transform, cut}).
-include("erlando_instance.hrl").

-define(TYPE, ?MODULE).

-erlando_instance(
   #{type => {?MODULE, [either/2]},
     adapters =>
         [#{mode => target,
            patterns => [?TYPE],
            capabilities =>
                [functor, applicative, monad, monad_fail, monad_error]}],
     manual => [monad_runner]}).

-include("erlando.hrl").

%% API
-export([fmap/2, '<$'/2]).
-export([pure/1, '<*>'/2, lift_a2/3, '*>'/2, '<*'/2]).
-export(['>>='/2, '>>'/2, return/1]).
-export([fail/1]).
-export([throw_error/1, catch_error/2]).
-export([run_nargs/0, run_m/2]).
-export([run/1]).
-export([left/1, right/1]).
-export([either/2, swap/0]).

%%%===================================================================
%%% API
%%%===================================================================
fmap(_, {left, L}) ->
    {left, L};
fmap(F, {right, R}) ->
    {right, F(R)}.

'<$'(FB, FA) ->
    functor:'default_<$'(FB, FA, ?TYPE).

pure(A) ->
    {right, A}.

'<*>'({left, L}, _) ->
    {left, L};
'<*>'(_, {left, L}) ->
    {left, L};
'<*>'({right, F}, {right, A}) ->
    {right, F(A)}.

lift_a2(FAB, AA, AB) ->
    applicative:default_lift_a2(FAB, AA, AB, ?TYPE).

'*>'(AA, AB) ->
    applicative:'default_*>'(AA, AB, ?TYPE).

'<*'(AA, AB) ->
    applicative:'default_<*'(AA, AB, ?TYPE).

'>>='({left, L}, _) ->
    {left, L};
'>>='({right, R}, KAMB) ->
    KAMB(R).

'>>'(MA, MB) ->
    monad:'default_>>'(MA, MB, ?TYPE).

return(A) ->
    monad:default_return(A, ?TYPE).

fail(E) ->
    {left, E}.

throw_error(E) ->
    fail(E).

catch_error({left, L}, EMB) ->
    undetermined:run(EMB(L), either);
catch_error({right, R}, _EMB) ->
    {right, R}.

run_nargs() ->
    0.

run_m(EA, []) ->
    run(EA).

run(#undetermined{} = UEA) ->
    undetermined:run(UEA, either);
run(Either) ->
    Either.

left(A) ->
    {left, A}.

right(B) ->
    {right, B}.

either(FAC, FBC) ->
    fun(EAB) ->
            case EAB of
                {left, A} ->
                    FAC(A);
                {right, B} ->
                    FBC(B)
            end
    end.

swap() ->
    either(right(_), left(_)).


%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================

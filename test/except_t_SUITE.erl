-module(except_t_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [test_return_and_bind,
     test_error_short_circuits_bind,
     test_lift,
     test_map_except_t,
     test_with_except_t,
     test_mplus_combines_errors,
     test_mplus_prefers_success].

test_return_and_bind(_Config) ->
    ExceptT = except_t:new(identity),
    M0 = except_t:return(2, ExceptT),
    M1 = except_t:'>>='(M0, fun(A) -> except_t:return(A + 3, ExceptT) end, ExceptT),
    ?assertEqual({right, 5}, run_identity(M1, ExceptT)).

test_error_short_circuits_bind(_Config) ->
    ExceptT = except_t:new(identity),
    M0 = except_t:throw_error(reason, ExceptT),
    M1 = except_t:'>>='(M0, fun(_) -> erlang:error(bind_was_called) end, ExceptT),
    ?assertEqual({left, reason}, run_identity(M1, ExceptT)).

test_lift(_Config) ->
    ExceptT = except_t:new(identity),
    M = except_t:lift(identity:return(value), ExceptT),
    ?assertEqual({right, value}, run_identity(M, ExceptT)).

test_map_except_t(_Config) ->
    ExceptT = except_t:new(identity),
    M0 = except_t:return(2, ExceptT),
    M1 = except_t:map_except_t(
           fun(Inner) -> identity:fmap(fun({right, A}) -> {right, A + 3} end, Inner) end,
           M0),
    ?assertEqual({right, 5}, run_identity(M1, ExceptT)).

test_with_except_t(_Config) ->
    ExceptT = except_t:new(identity),
    M0 = except_t:throw_error([reason], ExceptT),
    M1 = except_t:with_except_t(fun(Reason) -> [mapped | Reason] end, M0),
    ?assertEqual({left, [mapped, reason]}, run_identity(M1, ExceptT)).

test_mplus_combines_errors(_Config) ->
    ExceptT = except_t:new(identity),
    M0 = except_t:throw_error([first], ExceptT),
    M1 = except_t:throw_error([second], ExceptT),
    M2 = except_t:mplus(M0, M1, ExceptT),
    ?assertEqual({left, [first, second]}, run_identity(M2, ExceptT)).

test_mplus_prefers_success(_Config) ->
    ExceptT = except_t:new(identity),
    Failure = except_t:throw_error([reason], ExceptT),
    Success = except_t:return(value, ExceptT),
    ?assertEqual({right, value},
                 run_identity(except_t:mplus(Failure, Success, ExceptT), ExceptT)),
    ?assertEqual({right, value},
                 run_identity(except_t:mplus(Success, Failure, ExceptT), ExceptT)).

run_identity(M, ExceptT) ->
    identity:run(except_t:run(M, ExceptT)).

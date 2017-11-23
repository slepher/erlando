%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 17 Oct 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(state_t_SUITE).

-compile(export_all).

-compile({parse_transform, do}).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    application:ensure_all_started(erlando),
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() -> 
    [test_get, test_put, test_state, test_modify, test_gets,
     test_left_identity, test_right_identity, test_associativity,
     test_functor, test_applicative].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_get(_Config) -> 
    M = monad_state:get(),
    ?assertEqual({ok, 5}, error_m:run(reader_t:run(state_t:eval(M, 5), 5))).

test_put(_Config) ->
    M = monad_state:put(10),
    ?assertEqual({ok, 10}, error_m:run(reader_t:run(state_t:exec(M, 5), 5))).

test_state(_Config) ->
    M = monad_state:state(fun(S) -> {S + 1, S} end), 
    ?assertEqual({6, 5}, identity:run(state_t:run(M, 5))).
    
test_modify(_Config) ->
    M = monad_state:modify(fun(S) -> S + 1 end),
    ?assertEqual({ok, 6}, identity:run(state_t:run(M, 5))).

test_gets(_Config) ->
    M = monad_state:gets(fun(A) -> 2 * A end),
    ?assertEqual({10, 5}, identity:run(state_t:run(M, 5))).

test_left_identity(_Config) ->
    F = fun(A) -> state_m:put(A) end,
    MA = F(10),
    MB = ml_test_util:left_identity(10, F),
    ?assertEqual({ok, 10}, state_m:run(MA, undefined)),
    ?assertEqual({ok, 10}, state_m:run(MB, undefined)).
    
test_right_identity(_Config) ->
    MA = state_m:get(),
    MB = ml_test_util:right_identity(MA),
    ?assertEqual({10, 10}, state_m:run(MA, 10)),
    ?assertEqual({10, 10}, state_m:run(MB, 10)).

test_associativity(_Config) ->
    M = state_m:get(),
    F = fun(A) -> do([state_m ||
                         monad_state:modify(fun(S) -> S + A end, state_m),
                         state_m:get()
                     ])
        end,
    G = fun(A) ->
                state_m:put(A + 10)
        end,
    MA = ml_test_util:associativity1(M, F, G),
    MB = ml_test_util:associativity2(M, F, G),
    MC = ml_test_util:associativity3(M, F, G),
    
    ?assertEqual(50, state_m:exec(MA, 20)),
    ?assertEqual(50, state_m:exec(MB, 20)),
    ?assertEqual(50, state_m:exec(MC, 20)).
    
test_functor(_Config) ->
    MA = functor:fmap(fun(A) -> A + 3 end, state_m:get()),
    ?assertEqual({6, 3}, state_m:run(MA, 3)).

test_applicative(_Config) ->
    MA = state_m:get(),
    MB = functor:fmap(fun(A) -> A + 2 end, MA),
    MC = applicative:lift_a2(fun(A, B) -> (A + B) div 2 end, MA, MB),
    ?assertEqual({5, 4}, state_m:run(MC, 4)).

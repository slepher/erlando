%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_reader_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%%--------------------------------------------------------------------
%% TEST SERVER CALLBACK FUNCTIONS
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%%
%% @doc
%% Initialization before the suite.
%%
%% Config0 = Config1 = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the suite.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_suite(Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    erlando:start(),
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after the suite.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_suite(Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Initialization before each test case
%%
%% TestCase - atom()
%%   Name of the test case that is about to be run.
%% Config - [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% Note: This function is free to add any key/value pairs to the Config
%% variable, but should NOT alter/remove any existing entries.
%%
%% @spec init_per_testcase(TestCase, Config) -> Config
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @doc
%% Cleanup after each test case
%%
%% TestCase = atom()
%%   Name of the test case that is finished.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%%
%% @spec end_per_testcase(TestCase, Config) -> _
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Returns a description of the test suite when
%% Clause == doc, and a test specification (list
%% of the conf and test cases in the suite) when
%% Clause == suite.
%% Returns a list of all test cases in this test suite
%%
%% Clause = doc | suite
%%   Indicates expected return value.
%% Descr = [string()] | []
%%   String that describes the test suite.
%% Spec = [TestCase]
%%   A test specification.
%% TestCase = ConfCase | atom()
%%   Configuration case, or the name of a test case function.
%% ConfCase = {conf,Init,Spec,End} |
%%            {conf,Properties,Init,Spec,End}
%% Init = End = {Mod,Func} | Func
%%   Initialization and cleanup function.
%% Mod = Func = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%%   Execution properties of the test cases (may be combined).
%% Shuffle = shuffle | {shuffle,Seed}
%%   To get cases executed in random order.
%% Seed = {integer(),integer(),integer()}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%%   To get execution of cases repeated.
%% N = integer() | forever
%% Reason = term()
%%   The reason for skipping the test suite.
%%
%% @spec all(Clause) -> TestCases
%% @end
%%--------------------------------------------------------------------
all() -> 
    [test_ask_error_t, test_ask_maybe_t, test_ask_reader_t, test_ask_state_t, test_ask_cont_t,
     test_reader_error_t, test_reader_maybe_t, test_reader_reader_t, test_reader_state_t, test_reader_cont_t,
     test_local_error_t, test_local_maybe_t, test_local_reader_t, test_local_state_t, test_local_cont_t
    ].


%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%%  Test case function. Returns a description of the test
%%  case (doc), then returns a test specification (suite),
%%  or performs the actual test (Config).
%%
%% Arg = doc | suite | Config
%%   Indicates expected behaviour and return value.
%% Config = [tuple()]
%%   A list of key/value pairs, holding the test case configuration.
%% Descr = [string()] | []
%%   String that describes the test case.
%% Spec = [tuple()] | []
%%   A test specification, see all/1.
%% Reason = term()
%%   The reason for skipping the test case.
%%
%% @spec TestCase(Arg) -> Descr | Spec | ok | exit() | {skip,Reason}
%% @end
%%--------------------------------------------------------------------
test_ask_error_t(_Config) ->
    ask(fun(M) -> functor:fmap(fun({right, V}) -> V end, error_t:run(M)) end).

test_ask_maybe_t(_Config) ->
    ask(fun(T) -> functor:fmap(fun({just, V}) -> V end,maybe_t:run(T)) end).

test_ask_reader_t(_Config) ->
    ask(fun(T) -> T end).

test_ask_state_t(_Config) ->
    ask(fun(T) -> state_t:eval(T, undefined) end).

test_ask_cont_t(_Config) ->
    ask(fun(T) -> cont_t:eval(T) end).

test_reader_error_t(_Config) ->
    reader(fun(M) -> functor:fmap(fun({right, V}) -> V end, error_t:run(M)) end).

test_reader_maybe_t(_Config) ->
    reader(fun(T) -> functor:fmap(fun({just, V}) -> V end,maybe_t:run(T)) end).

test_reader_reader_t(_Config) ->
    reader(fun(T) -> T end).

test_reader_state_t(_Config) ->
    reader(fun(T) -> state_t:eval(T, undefined) end).

test_reader_cont_t(_Config) ->
    reader(fun(T) -> cont_t:eval(T) end).

test_local_error_t(_Config) ->
    local(fun(M) -> functor:fmap(fun({right, V}) -> V end, error_t:run(M)) end).

test_local_maybe_t(_Config) ->
    local(fun(T) -> functor:fmap(fun({just, V}) -> V end,maybe_t:run(T)) end).

test_local_reader_t(_Config) ->
    local(fun(T) -> T end).

test_local_state_t(_Config) ->
    local(fun(T) -> state_t:eval(T, undefined) end).

test_local_cont_t(_Config) ->
    local(fun(T) -> cont_t:eval(T) end).

ask(F) ->
    M = monad_reader:ask(),
    M1 = F(M),
    Result = 10,
    ?assertEqual(Result, reader_m:run(M1, 10)).

reader(F) ->
    M = monad_reader:reader(fun(A) -> A * 2 end),
    M1 = F(M),
    Result = 20,
    ?assertEqual(Result, reader_m:run(M1, 10)).

local(F) ->
    M = monad_reader:local(fun(A) -> A * 3 end, monad_reader:ask()),
    M1 = F(M),
    Result = 30,
    ?assertEqual(Result, reader_m:run(M1, 10)).


%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 22 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_error_SUITE).

%% Note: This directive should only be used in test suites.

-suite_defaults([{timetrap, {seconds, 5}}]).

-compile(export_all).
-compile(nowarn_export_all).

-compile({parse_transform, cut}).
-include("do.hrl").

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
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_throw_error |
%%              repeat_until_any_ok | repeat_until_any_throw_error
%%   To get execution of cases repeated.
%% N = integer() | forever
%% Reason = term()
%%   The reason for skipping the test suite.
%%
%% @spec all(Clause) -> TestCases
%% @end
%%--------------------------------------------------------------------
all() -> 
    [test_error_m_throw_error, test_error_m_catch_error,
     test_either_throw_error, test_either_catch_error,
     test_error_t_throw_error, test_error_t_catch_error,
     test_reader_t_throw_error, test_reader_t_catch_error,
     test_reader_t_trans_error, test_reader_t_lift_error,
     test_writer_t_throw_error, test_writer_t_catch_error, test_writer_t_catch_error_2,
     test_state_t_throw_error,  test_state_t_catch_error
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
test_error_m_throw_error(_Config) ->
    F = fun(M) -> {error, E} = error_m:run(M), E end,
    throw_error(F).

test_error_m_catch_error(_Config) ->
    F = fun(M) -> {error, E} = error_m:run(M), E end,
    test_catch_error(F).

test_either_throw_error(_Config) ->
    F = fun(M) -> {left, E} = either:run(M), E end,
    throw_error(F).

test_either_catch_error(_Config) ->
    F = fun(M) -> {left, E} = either:run(M), E end,
    test_catch_error(F).

test_error_t_throw_error(_Config) ->
    F = fun(E) -> {left, L} = identity:run(error_t:run(E)), L end,
    throw_error(F).

test_error_t_catch_error(_Config) ->
    F = fun(E) -> {left, L} = identity:run(error_t:run(E)), L end,
    test_catch_error(F).

test_reader_t_throw_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(reader_t:run(E, undefined)), L end,
    throw_error(F).

test_reader_t_catch_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(reader_t:run(E, undefined)), L end,
    test_catch_error(F).

test_reader_t_trans_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(reader_t:run(E, undefined)), L end,
    test_trans_error(F).

test_reader_t_lift_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(reader_t:run(E, undefined)), L end,
    test_lift_error(F).

test_writer_t_throw_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(writer_t:eval(E)), L end,
    throw_error(F).

test_writer_t_catch_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(writer_t:eval(E)), L end,
    test_catch_error(F).

test_writer_t_catch_error_2(_Config) ->
    M0 = 
        monad_error:catch_error(
          do([monad ||
                 monad_writer:tell([a]),
                 monad_error:catch_error(monad_error:throw_error(error), fun(error) -> monad_writer:tell([b]) end),
                 monad_error:throw_error(error2)
          ]), fun(error2) -> monad_writer:tell([c]) end),
   Result1 = writer_m:run(error_t:run(M0, error_t:new(writer_m))),
   Result2 = error_m:run(writer_t:run(M0, writer_t:new(error_m))),
   ?assertEqual({{right, ok}, [a, b, c]}, Result1),
   ?assertEqual({ok, {ok, [c]}}, Result2).

test_state_t_throw_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(state_t:eval(E, undefined)), L end,
    throw_error(F).

test_state_t_catch_error(_Config) ->
    F = fun(E) -> {left, L} = either:run(state_t:eval(E, undefined)), L end,
    test_catch_error(F).

test_trans_error(F) ->
    M = monad_error:throw_error(error),
    M1 = monad_error:trans_error(M, fun(error) -> error1 end),
    M2 = monad_error:trans_error(M, fun(error1) -> error2 end),
    Result1 = error1,
    ?assertEqual(Result1, F(M1)),
    Result2 = error,
    ?assertEqual(Result2, F(M2)).

test_lift_error(F) ->
    M1 = monad_error:lift_error({error, 30}),
    M2 = do([monad ||
                Val <- monad_error:catch_error(M1, fun(E) -> monad:return(E + 20) end),
                monad_error:lift_error({error, Val * 2})
            ]),
    Result = 100,
    ?assertEqual(Result, F(M2)).

throw_error(F) ->
    M = monad_error:throw_error(error),
    Result = error,
    ?assertEqual(Result, F(M)).

test_catch_error(F) ->
    M = monad_error:throw_error(error),
    M1 = monad_error:catch_error(M, fun(error) -> monad_error:throw_error(error1) end),
    M2 = monad_error:catch_error(M, fun(error1) -> monad_error:throw_error(error2) end),
    Result1 = error1,
    ?assertEqual(Result1, F(M1)),
    Result2 = error,
    ?assertEqual(Result2, F(M2)).

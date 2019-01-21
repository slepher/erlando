%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(monad_writer_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
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
all(doc) ->
    ["Describe the main purpose of this suite"].

all() -> 
    [test_writer_error_t, test_writer_maybe_t, 
     test_writer_reader_t, test_writer_state_t,
     test_writer_writer_t,

     test_tell_error_t, test_tell_maybe_t, 
     test_tell_reader_t, test_tell_state_t,
     test_tell_writer_t,

     test_listen_error_t, test_listen_maybe_t, 
     test_listen_reader_t, test_listen_state_t,
     test_listen_writer_t,

     test_pass_error_t, test_pass_maybe_t, 
     test_pass_reader_t, test_pass_state_t,
     test_pass_writer_t
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

test_writer_error_t(_Config) ->
    writer(fun(M) -> functor:fmap(fun({right, V}) -> V end, error_t:run(M)) end).

test_writer_maybe_t(_Config) ->
    writer(fun(T) -> functor:fmap(fun({just, V}) -> V end,maybe_t:run(T)) end).

test_writer_reader_t(_Config) ->
    writer(fun(T) -> reader_t:run(T, undefined) end).

test_writer_state_t(_Config) ->
    writer(fun(T) -> state_t:eval(T, undefined) end).

test_writer_writer_t(_Config) ->
    writer(fun(T) -> T end).

test_tell_error_t(_Config) ->
    tell(fun(M) -> functor:fmap(fun({right, V}) -> V end, error_t:run(M)) end).

test_tell_maybe_t(_Config) ->
    tell(fun(T) -> functor:fmap(fun({just, V}) -> V end,maybe_t:run(T)) end).

test_tell_reader_t(_Config) ->
    tell(fun(T) -> reader_t:run(T, undefined) end).

test_tell_state_t(_Config) ->
    tell(fun(T) -> state_t:eval(T, undefined) end).

test_tell_writer_t(_Config) ->
    tell(fun(T) -> T end).

test_listen_error_t(_Config) ->
    listen(fun(M) -> functor:fmap(fun({right, V}) -> V end, error_t:run(M)) end).

test_listen_maybe_t(_Config) ->
    listen(fun(T) -> functor:fmap(fun({just, V}) -> V end,maybe_t:run(T)) end).

test_listen_reader_t(_Config) ->
    listen(fun(T) -> reader_t:run(T, undefined) end).

test_listen_state_t(_Config) ->
    listen(fun(T) -> state_t:eval(T, undefined) end).

test_listen_writer_t(_Config) ->
    listen(fun(T) -> T end).

test_pass_error_t(_Config) ->
    pass(fun(M) -> functor:fmap(fun({right, V}) -> V end, error_t:run(M)) end).

test_pass_maybe_t(_Config) ->
    pass(fun(T) -> functor:fmap(fun({just, V}) -> V end,maybe_t:run(T)) end).

test_pass_reader_t(_Config) ->
    pass(fun(T) -> reader_t:run(T, undefined) end).

test_pass_state_t(_Config) ->
    pass(fun(T) -> state_t:eval(T, undefined) end).

test_pass_writer_t(_Config) ->
    pass(fun(T) -> T end).

writer(F) ->
    M = monad_writer:writer({ok, [hello, world]}),
    M1 = F(M),
    Result = {ok, {ok, [hello, world]}},
    ?assertEqual(Result, error_m:run(reader_t:run(writer_t:run(M1), 5))).
  
tell(F) ->
    MT = do([monad || 
               monad_writer:tell([hello]),
               monad_writer:tell([world]),
               return(ok)
           ]),
    MA = F(MT),
    Result = {ok, [hello, world]},
    ?assertEqual(Result, identity:run(reader_t:run(writer_t:run(MA), 5))).

listen(F) ->
    M = do([monad || 
               monad_writer:tell([hello]),
               monad_writer:tell([world]),
               return(10)
           ]),
    M0 = monad_writer:listen(M),
    M1 = F(M0),
    Result = {ok, {{10, [hello, world]}, [hello, world]}},
    ?assertEqual(Result, error_m:run(reader_t:run(writer_t:run(M1), 5))).    

pass(F) ->
    M = do([monad || 
               monad_writer:tell([hello]),
               monad_writer:tell([world]),
               return({10, fun(W) -> string:join(
                                      lists:map(fun(A) -> atom_to_list(A) end, W), ",") end})
           ]),
    M0 = monad_writer:pass(M),
    M1 = F(M0),
    Result = {ok, {10, "hello,world"}},
    ?assertEqual(Result, error_m:run(reader_t:run(writer_t:run(M1), 5))).

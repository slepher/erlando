%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(reader_t_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-compile({parse_transform, do}).

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
    [test_reader_t_ask, test_monad_laws, test_monad_fail, test_local, test_monad_lift, test_reader].


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
test_reader_t_ask() ->
    [{doc, "Test reader_t"}].

test_reader_t_ask(_Config) ->
    Monad = reader_t:new(identity_m),
    M0 = 
        do([Monad ||
               Local <- Monad:ask(),
               return({Local, world})
           ]),
    ?assertEqual({hello, world}, Monad:run_reader(M0, hello)).


test_monad_laws(_Config) ->
    Monad = reader_t:new(error_m),

    F = fun(A) -> do([Monad || 
                         Value <- Monad:ask(),
                         return(Value + 3 + A)
                     ])
        end,

    G = fun(A) -> do([ Monad ||
                         Value <- Monad:ask(),
                         return(Value * 7 + A)
                     ])
        end,
    M1 = do([Monad ||
                Value <- return(2),
                F(Value)
            ]),

    M2 = F(2),
    
    ?assertEqual({ok, 9}, Monad:run_reader(M1, 4)),
    ?assertEqual({ok, 9}, Monad:run_reader(M2, 4)),
    
    M3 = do([Monad ||
               Value <- Monad:ask(),
               return(Value + 4)]),
                  
    M4 = do([Monad ||
                Value <- M3,
                return(Value)]),
    ?assertEqual({ok, 7}, Monad:run_reader(M3, 3)),
    ?assertEqual({ok, 7}, Monad:run_reader(M4, 3)),
    
    M5 = do([Monad || 
                Y <- do([Monad || X <- M3, F(X) ]),
                G(Y)
            ]),
    M6 = do([Monad ||
                X <- M3,
                do([Monad ||
                       Y <- F(X),
                       G(Y)])
            ]),
    M7 = do([Monad ||
                X <- M3,
                Y <- F(X),
                G(Y)
            ]),
    
    ?assertEqual({ok, 97}, Monad:run_reader(M5, 10)),
    ?assertEqual({ok, 97}, Monad:run_reader(M6, 10)),
    ?assertEqual({ok, 97}, Monad:run_reader(M7, 10)).
               

test_monad_fail(_Config) ->
    Monad = reader_t:new(error_m),
    M0 = do([Monad ||
                Value <- Monad:ask(),
                fail(Value + 3)
            ]),
    
    ?assertEqual({error, 13}, Monad:run_reader(M0, 10)).

test_monad_lift(_Config) ->
    Monad = reader_t:new(error_m),
    M0 = do([Monad ||
                X <- Monad:ask(),
                Y <- Monad:lift({ok, 10}),
                return(X * Y)
            ]),
    
    ?assertEqual({ok, 60}, Monad:run_reader(M0, 6)).

test_local(_Config) ->
    Monad = reader_t:new(error_m),
    M0 = do([Monad ||
                X <- Monad:ask(),
                Y <- Monad:lift({ok, 10}),
                return(X * Y)
            ]),
    
    M1 = Monad:local(fun(X) -> X * 2 end, M0),
    ?assertEqual({ok, 120}, Monad:run_reader(M1, 6)).

test_reader(_Config) ->
    Monad = reader_t:new(error_m),
    Monad0 = reader_t:new(identity_m),
    M0 = do([Monad0 ||
                Value <- Monad0:ask(),
                return(length(Value))
            ]),
    M1 = Monad:reader(reader_t:run_reader_t(M0)),

    ?assertEqual({ok, 3}, Monad:run_reader(M1, [1,2,3])).

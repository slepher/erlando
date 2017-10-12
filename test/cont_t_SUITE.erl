%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepher@issac.local>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Jun 2017 by Chen Slepher <slepher@issac.local>
%%%-------------------------------------------------------------------
-module(cont_t_SUITE).

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
all(doc) ->
    ["Describe the main purpose of this suite"].

all() -> 
    [test_cont_t, test_cont_t_callCC, test_cont_t_local, test_cont_t_shift_reset, test_cont_t_shift_reset2].


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
test_cont_t() ->                 
    [{doc, "Test cont_t"}].

test_cont_t(_Config) ->


    Monad = cont_t:new(error_m),

    M1 = Monad:return(2),
    M2 = Monad:return(3),


    RX = (fun() -> 
                 monad:'>>='(M1,
                             fun(R1) ->
                                     monad:'>>='(M2,
                                                 fun(R2) ->
                                                         monad:return(R1 + R2, Monad)
                                                 end, Monad)
                             end, Monad) 
          end)(),

    R = do([Monad || 
               R1 <- M1,
               R2 <- M2,
               return(R1 + R2)]),
    {cont_t, _} = R,
    CC = fun(X) -> {ok, X} end,
    ?assertEqual({ok, 5}, cont_t:run_cont(RX, CC)),
    %?assertEqual({ok, 5}, cont_t:run_cont(R, CC)),
    ok.


test_cont_t_callCC(_Config) ->
    MonadState = state_t:new(identity),
    Monad = cont_t:new(MonadState),
    
    M0 = 
        do([Monad ||
               Value <- Monad:callCC(
                          fun(K) ->
                                  do([Monad ||
                                         Acc <- Monad:lift(MonadState:get()),
                                         Monad:lift(MonadState:put([K|Acc])),
                                         return(0)
                                     ])
                          end),
               begin
                   do([Monad ||
                          Acc <- Monad:lift(MonadState:get()),
                          case Acc of
                              [] ->
                                  return(1 + Value);
                              [K|T] ->
                                  do([Monad ||
                                         Monad:lift(MonadState:put(T)),
                                         K(3)])
                          end
                      ])
               end
           ]),
    Value = identity:run_identity(MonadState:eval(Monad:run_cont(M0, fun(X) -> MonadState:return(X) end), [])),
    ?assertEqual(4, Value).

test_cont_t_local(_Config) ->
    MR = reader_t:new(identity),
    Monad = cont_t:new(MR),
    RefO = make_ref(),
    Ref = make_ref(),
    
    M0 = do([Monad ||
                Ref0 <- Monad:lift(MR:ask()),
                return(Ref0)
            ]),
    M1 = cont_t_lifted_local(fun(_) -> Ref end, M0),
    M2 = do([Monad ||
                Ref0 <- M0,
                Ref1 <- M1,
                Ref2 <- Monad:lift(MR:ask()),
                return({Ref0, Ref1, Ref2})
            ]),
    Reader = Monad:run_cont(M2, fun(X) -> MR:return(X) end),
    {R0, R1, R2}= identity:run_identity(MR:run_reader(Reader, RefO)),
    ?assertEqual(RefO, R0),
    ?assertEqual(RefO, R2),
    ?assertEqual(Ref, R1).

cont_t_lifted_local(F, C) ->
    MR = reader_t:new(identity),
    Monad = cont_t:new(MR),
    Monad:lift_local(
      fun() -> MR:ask() end,
      fun(IF, X) -> MR:local(IF, X) end,
      F, C).                       

test_cont_t_shift_reset(_Config) ->
    MR = reader_t:new(identity),
    MS = state_t:new(MR),
    MC = cont_t:new(MS),
    M = MC:reset(
          do([MC || 
                 R <- MC:lift(MS:lift(MR:ask())),
                 S <- MC:lift(MS:get()),
                 MC:lift(MS:put(hello)),
                 MC:shift(fun(_K) -> MC:return({R, S}) end),
                 MC:lift(MS:put(world))
             ])),
    Result = identity:run_identity(MR:run_reader(MS:run_state(MC:run_cont(M, fun(A) -> MS:return(A) end), 0), 10)),
    ?assertEqual({{10, 0}, hello}, Result).

test_cont_t_shift_reset2(_Config) ->
    MR = reader_t:new(identity),
    MS = state_t:new(MR),
    MC = cont_t:new(MS),
    M = MC:reset(
          do([MC || 
                 R <- MC:lift(MS:lift(MR:ask())),
                 S <- MC:lift(MS:get()),
                 MC:lift(MS:put(hello)),
                 Val <- MC:shift(fun(K) -> MC:lift(K({R, S})) end),
                 MC:lift(MS:put(world)),
                 MC:return({Val, 1})
             ])),
    Result = identity:run_identity(MR:run_reader(MS:run_state(MC:run_cont(M, fun(A) -> MS:return(A) end), 0), 10)),
    ?assertEqual({{{10, 0}, 1}, world}, Result).


%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2019, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  4 Sep 2019 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(list_t_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("do.hrl").
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
    [test_fmap, test_ap, test_bind, test_run, test_callCC, test_join, test_lift_list, test_catch_error].

%%--------------------------------------------------------------------
%% @spec TestCase() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
my_test_case() -> 
    [].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
test_fmap(_Config) -> 
    ListTA = list_t:from_list([a, b, c]), 
    F = fun(A) -> binary_to_atom(list_to_binary(io_lib:format("~p_~p", [A, A])), utf8)  end,
    ListTB = functor:fmap(F, ListTA),
    MB = list_t:run(ListTB),
    ?assertEqual([a_a, b_b, c_c], identity:run(MB)),
    ok.

test_ap(_Config) ->
    MA = list_t:new(monad),
    FA = fun(A) -> A + 10 end,
    FB = fun(A) -> A - 3 end,
    ListTA = list_t:from_list([1, 2, 3], MA),
    ListTF = list_t:from_list([FA, FB], MA),
    ListTC = applicative:'<*>'(ListTF, ListTA, MA),
    MC = list_t:run(ListTC, MA),
    ?assertEqual([11, 12, 13, -2, -1, 0], identity:run(MC)),
    ok.

test_bind(_Config) ->
    ListTA = list_t:from_list([a, b, c]), 
    F = fun(A) -> binary_to_atom(list_to_binary(io_lib:format("~p_~p", [A, A])), utf8)  end,
    ListTB = monad:'>>='(ListTA, fun(A) -> B = F(A), monad_plus:mplus(monad:return(B), monad:return(B)) end),
    MB = list_t:run(ListTB),
    ?assertEqual([a_a, a_a, b_b, b_b, c_c, c_c], identity:run(MB)),
    ?assertEqual([a_a, a_a, b_b, b_b, c_c, c_c], cont_m:eval(MB)),
    ok.

test_run(_Config) ->
    MA = list_t:new(identity),
    ListTA = monad:return(a, MA),
    ListTB = monad:return(b, MA),
    ListTC = monad_plus:mplus(ListTA, ListTB),
    ?assertEqual({list_t, {identity, {cons, a, {identity, {cons, b, {identity, nil}}}}}}, ListTC),
    IdentityC = list_t:run(ListTC),
    ?assertEqual({identity, [a, b]}, IdentityC).

test_lift_list(_Config) ->
    List = [{just, 3}, {just, 5}, nothing, {just, 6}],
    ListT = list_t:lift_list(List),
    Val = list_t:run(ListT),
    ?assertEqual(nothing, Val),
    ?assertEqual({just, {cons, 3, {just, {cons, 5, nothing}}}}, list_t:run_list_t(ListT)),
    ok.

test_callCC(_Config) ->
    M = 
        do([monad ||
               X <- monad_reader:ask(),
               Y <- 
                   monad_cont:callCC(
                     fun(CC) ->
                             case X of
                                 X when is_integer(X) ->
                                     return(X + 1);
                                 _Other ->
                                     CC(expected_integer)
                             end
                     end),
               return(Y)
           ]),
    M1 = identity:run(reader_t:run(cont_t:eval(list_t:run(M)), 30)),
    %M2 = identity:run(reader_t:run(cont_t:eval(list_t:run(M)), undefined)),
    ?assertEqual([31], M1),
    %?assertEqual([expected_integer], M2),
    ok.

test_catch_error(_Config) ->
    Monad = list_t:new(error_t:new(identity)),
    M1 = monad:return(1, Monad),
    M2 = monad_error:throw_error(error, Monad),
    M3 = monad_plus:mplus(M1, M2, Monad),
    io:format("M3 is ~p~n", [M3]),
    M4 = monad_error:catch_error(
           M3,
           fun(error) ->
                   monad_error:throw_error(error1)
           end),
    io:format("M4 is ~p", [M4]),
    %A = identity:run(error_t:run(list_t:run(M3))),
    %?assertEqual({left, error1}, A),
    ok.
    

test_join(_Config) ->
    ok.

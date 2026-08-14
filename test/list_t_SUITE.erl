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
    [{timetrap, {seconds, 30}}].

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
    [
        test_fmap,
        test_ap,
        test_ap_effect_order,
        test_bind,
        test_bind_effect_order,
        test_append_effect_order,
        test_run,
        test_callCC,
        test_local_all_cells,
        test_join,
        test_lift_list,
        test_catch_error
    ].

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
    F = fun(A) -> binary_to_atom(list_to_binary(io_lib:format("~p_~p", [A, A])), utf8) end,
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

test_ap_effect_order(_Config) ->
    ListT = list_t:new(state_m),
    Functions = list_t:lift_list(
        [
            state_effect(f1, fun(A) -> {f1, A} end),
            state_effect(f2, fun(A) -> {f2, A} end)
        ],
        ListT
    ),
    Values = list_t:lift_list(
        [state_effect(a1, a), state_effect(a2, b)], ListT
    ),
    Applied = applicative:'<*>'(Functions, Values, ListT),
    ?assertEqual(
        {[{f1, a}, {f1, b}, {f2, a}, {f2, b}], [f1, a1, a2, f2, a1, a2]},
        run_state_list(Applied, ListT)
    ).

test_bind(_Config) ->
    ListTA = list_t:from_list([a, b, c]),
    F = fun(A) -> binary_to_atom(list_to_binary(io_lib:format("~p_~p", [A, A])), utf8) end,
    ListTB = monad:'>>='(ListTA, fun(A) ->
        B = F(A),
        monad_plus:mplus(monad:return(B), monad:return(B))
    end),
    MB = list_t:run(ListTB),
    ?assertEqual([a_a, a_a, b_b, b_b, c_c, c_c], identity:run(MB)),
    ?assertEqual([a_a, a_a, b_b, b_b, c_c, c_c], cont_m:eval(MB)),
    ok.

test_bind_effect_order(_Config) ->
    ListT = list_t:new(state_m),
    Outer = list_t:lift_list(
        [state_effect(a, a), state_effect(b, b)], ListT
    ),
    Bound = monad:'>>='(
        Outer,
        fun(A) ->
            list_t:lift_list(
                [
                    state_effect({A, 1}, {A, 1}),
                    state_effect({A, 2}, {A, 2})
                ],
                ListT
            )
        end,
        ListT
    ),
    ?assertEqual(
        {[{a, 1}, {a, 2}, {b, 1}, {b, 2}], [a, {a, 1}, {a, 2}, b, {b, 1}, {b, 2}]},
        run_state_list(Bound, ListT)
    ).

test_append_effect_order(_Config) ->
    ListT = list_t:new(state_m),
    Left = list_t:lift_list(
        [state_effect(a1, a), state_effect(a2, b)], ListT
    ),
    Right = list_t:lift_list(
        [state_effect(b1, c), state_effect(b2, d)], ListT
    ),
    Appended = monad_plus:mplus(Left, Right, ListT),
    ?assertEqual(
        {[a, b, c, d], [a1, a2, b1, b2]},
        run_state_list(Appended, ListT)
    ).

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
        do([
            monad
         || X <- monad_reader:ask(),
            Y <-
                monad_cont:callCC(
                    fun(CC) ->
                        case X of
                            X when is_integer(X) ->
                                return(X + 1);
                            _Other ->
                                CC(expected_integer)
                        end
                    end
                ),
            return(Y)
        ]),
    M1 = identity:run(reader_t:run(cont_t:eval(list_t:run(M)), 30)),
    %M2 = identity:run(reader_t:run(cont_t:eval(list_t:run(M)), undefined)),
    ?assertEqual([31], M1),
    %?assertEqual([expected_integer], M2),
    ok.

test_local_all_cells(_Config) ->
    ListT = list_t:new(reader_m),
    Values = list_t:lift_list([reader_m:ask(), reader_m:ask()], ListT),
    Local = monad_reader:local(fun(N) -> N * 3 end, Values, ListT),
    ?assertEqual([30, 30], reader_m:run(list_t:run(Local, ListT), 10)).

test_catch_error(_Config) ->
    Monad = list_t:new(error_t:new(identity)),
    M1 = monad:return(1, Monad),
    M2 = monad_error:throw_error(error, Monad),
    M3 = monad_plus:mplus(M1, M2, Monad),
    Recovered = monad_error:catch_error(
        M3,
        fun(error) ->
            monad:return(2, Monad)
        end,
        Monad
    ),
    Rethrown = monad_error:catch_error(
        M3,
        fun(error) -> monad_error:throw_error(error1, Monad) end,
        Monad
    ),
    ?assertEqual({right, [1, 2]}, run_error_list(Recovered, Monad)),
    ?assertEqual({left, error1}, run_error_list(Rethrown, Monad)),
    ok.

test_join(_Config) ->
    ListT = list_t:new(identity),
    Nested = list_t:from_list(
        [
            list_t:from_list([1, 2], ListT),
            list_t:from_list([3, 4], ListT)
        ],
        ListT
    ),
    Joined = monad:join(Nested, ListT),
    ?assertEqual([1, 2, 3, 4], identity:run(list_t:run(Joined, ListT))).

state_effect(Tag, Value) ->
    state_m:state(fun(Effects) -> {Value, Effects ++ [Tag]} end).

run_state_list(List, ListT) ->
    state_m:run(list_t:run(List, ListT), []).

run_error_list(List, ListT) ->
    identity:run(error_t:run(list_t:run(List, ListT))).

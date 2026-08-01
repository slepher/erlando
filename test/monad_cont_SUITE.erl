-module(monad_cont_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include("do.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [test_cont_t_callcc,
     test_reader_t_callcc,
     test_state_t_callcc,
     test_maybe_t_callcc,
     test_error_t_callcc,
     test_except_t_callcc,
     test_writer_t_callcc,
     test_list_t_callcc].

test_cont_t_callcc(_Config) ->
    Monad = cont_t:new(identity),
    M = monad_cont:callCC(fun(K) -> K(value) end, Monad),
    ?assertEqual(value, identity:run(cont_t:eval(M, Monad))).

test_reader_t_callcc(_Config) ->
    ContT = cont_t:new(identity),
    Monad = reader_t:new(ContT),
    M = monad_cont:callCC(fun(K) -> K(value) end, Monad),
    Cont = reader_t:run(M, environment, Monad),
    ?assertEqual(value, identity:run(cont_t:eval(Cont, ContT))).

test_state_t_callcc(_Config) ->
    ContT = cont_t:new(identity),
    Monad = state_t:new(ContT),
    M = monad_cont:callCC(fun(K) -> K(value) end, Monad),
    Cont = state_t:run(M, initial_state, Monad),
    ?assertEqual({value, initial_state}, identity:run(cont_t:eval(Cont, ContT))).

test_maybe_t_callcc(_Config) ->
    ContT = cont_t:new(identity),
    Monad = maybe_t:new(ContT),
    M = monad_cont:callCC(fun(K) -> K(value) end, Monad),
    Cont = maybe_t:run(M, Monad),
    ?assertEqual({just, value}, identity:run(cont_t:eval(Cont, ContT))).

test_error_t_callcc(_Config) ->
    ContT = cont_t:new(identity),
    Monad = error_t:new(ContT),
    M = monad_cont:callCC(fun(K) -> K(value) end, Monad),
    Cont = error_t:run(M, Monad),
    ?assertEqual({right, value}, identity:run(cont_t:eval(Cont, ContT))).

test_except_t_callcc(_Config) ->
    ContT = cont_t:new(identity),
    Monad = except_t:new(ContT),
    M = monad_cont:callCC(fun(K) -> K(value) end, Monad),
    Cont = except_t:run(M, Monad),
    ?assertEqual({right, value}, identity:run(cont_t:eval(Cont, ContT))).

test_writer_t_callcc(_Config) ->
    ContT = cont_t:new(identity),
    Monad = writer_t:new(ContT),
    M = do([Monad ||
               Value <- monad_cont:callCC(fun(K) -> K(value) end, Monad),
               monad_writer:tell([after_callcc]),
               return(Value)
           ]),
    Cont = writer_t:run(M, Monad),
    ?assertEqual({value, [after_callcc]}, identity:run(cont_t:eval(Cont, ContT))).

test_list_t_callcc(_Config) ->
    ContT = cont_t:new(identity),
    Monad = list_t:new(ContT),
    M = monad_cont:callCC(fun(K) -> K(value) end, Monad),
    Cont = list_t:run(M, Monad),
    ?assertEqual([value], identity:run(cont_t:eval(Cont, ContT))).

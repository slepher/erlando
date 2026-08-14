-module(transformer_laws_SUITE).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("eunit/include/eunit.hrl").

all() ->
    [
        test_monad_laws_over_identity,
        test_applicative_laws_over_identity,
        test_monad_trans_laws
    ].

test_monad_laws_over_identity(_Config) ->
    lists:foreach(fun assert_monad_laws/1, transformers()).

test_applicative_laws_over_identity(_Config) ->
    lists:foreach(fun assert_applicative_laws/1, transformers()).

test_monad_trans_laws(_Config) ->
    lists:foreach(
        fun(Transformer) ->
            lists:foreach(
                fun(Base) -> assert_monad_trans_laws(Transformer, Base) end,
                [identity, state_m, error_m]
            )
        end,
        transformers()
    ).

assert_monad_laws(Transformer) ->
    T = Transformer:new(identity),
    F = fun(A) -> monad:return(A + 3, T) end,
    G = fun(A) -> monad:return(A * 2, T) end,
    M = monad:return(4, T),

    LeftIdentity = monad:'>>='(monad:return(4, T), F, T),
    ?assertEqual(
        observe(Transformer, identity, F(4)),
        observe(Transformer, identity, LeftIdentity)
    ),

    RightIdentity = monad:'>>='(M, fun(A) -> monad:return(A, T) end, T),
    ?assertEqual(
        observe(Transformer, identity, M),
        observe(Transformer, identity, RightIdentity)
    ),

    LeftAssociative = monad:'>>='(monad:'>>='(M, F, T), G, T),
    RightAssociative =
        monad:'>>='(
            M,
            fun(A) -> monad:'>>='(F(A), G, T) end,
            T
        ),
    ?assertEqual(
        observe(Transformer, identity, LeftAssociative),
        observe(Transformer, identity, RightAssociative)
    ).

assert_applicative_laws(Transformer) ->
    T = Transformer:new(identity),
    Id = fun(A) -> A end,
    F = fun(A) -> A + 3 end,
    U = applicative:pure(F, T),
    V = applicative:pure(4, T),

    Identity = applicative:'<*>'(applicative:pure(Id, T), V, T),
    ?assertEqual(
        observe(Transformer, identity, V),
        observe(Transformer, identity, Identity)
    ),

    HomomorphismLeft =
        applicative:'<*>'(applicative:pure(F, T), applicative:pure(4, T), T),
    HomomorphismRight = applicative:pure(F(4), T),
    ?assertEqual(
        observe(Transformer, identity, HomomorphismRight),
        observe(Transformer, identity, HomomorphismLeft)
    ),

    InterchangeLeft = applicative:'<*>'(U, applicative:pure(4, T), T),
    InterchangeRight =
        applicative:'<*>'(
            applicative:pure(fun(Apply) -> Apply(4) end, T),
            U,
            T
        ),
    ?assertEqual(
        observe(Transformer, identity, InterchangeRight),
        observe(Transformer, identity, InterchangeLeft)
    ),

    Compose = fun(Outer) ->
        fun(Inner) ->
            fun(A) -> Outer(Inner(A)) end
        end
    end,
    Double = applicative:pure(fun(A) -> A * 2 end, T),
    CompositionLeft =
        applicative:'<*>'(
            applicative:'<*>'(
                applicative:'<*>'(applicative:pure(Compose, T), U, T),
                Double,
                T
            ),
            V,
            T
        ),
    CompositionRight = applicative:'<*>'(U, applicative:'<*>'(Double, V, T), T),
    ?assertEqual(
        observe(Transformer, identity, CompositionRight),
        observe(Transformer, identity, CompositionLeft)
    ).

assert_monad_trans_laws(Transformer, Base) ->
    T = Transformer:new(Base),

    LiftReturn = monad_trans:lift(monad:return(4, Base), T),
    TransformerReturn = monad:return(4, T),
    ?assertEqual(
        observe(Transformer, Base, TransformerReturn),
        observe(Transformer, Base, LiftReturn)
    ),

    M = base_action(Base),
    F = fun(A) -> base_continuation(Base, A) end,
    LiftBind = monad_trans:lift(monad:'>>='(M, F, Base), T),
    BindLift =
        monad:'>>='(
            monad_trans:lift(M, T),
            fun(A) -> monad_trans:lift(F(A), T) end,
            T
        ),
    ?assertEqual(
        observe(Transformer, Base, BindLift),
        observe(Transformer, Base, LiftBind)
    ).

base_action(identity) ->
    identity:return(4);
base_action(state_m) ->
    monad_state:get(state_m);
base_action(error_m) ->
    monad_error:throw_error(base_failure, error_m).

base_continuation(identity, A) ->
    identity:return(A + 3);
base_continuation(state_m, A) ->
    monad:'>>'(
        monad_state:put(A + 1, state_m),
        monad:return(A * 2, state_m),
        state_m
    );
base_continuation(error_m, A) ->
    error_m:return(A + 3).

transformers() ->
    [maybe_t, error_t, except_t, reader_t, state_t, writer_t, cont_t, list_t].

observe(Transformer, Base, M) ->
    normalize(Transformer, Base, run_base(Base, run_transformer(Transformer, Base, M))).

run_transformer(maybe_t, Base, M) ->
    maybe_t:run(M, maybe_t:new(Base));
run_transformer(error_t, Base, M) ->
    error_t:run(M, error_t:new(Base));
run_transformer(except_t, Base, M) ->
    except_t:run(M, except_t:new(Base));
run_transformer(reader_t, Base, M) ->
    reader_t:run(M, environment, reader_t:new(Base));
run_transformer(state_t, Base, M) ->
    state_t:run(M, initial_state, state_t:new(Base));
run_transformer(writer_t, Base, M) ->
    writer_t:run(M, writer_t:new(Base));
run_transformer(cont_t, Base, M) ->
    cont_t:eval(M, cont_t:new(Base));
run_transformer(list_t, Base, M) ->
    list_t:run(M, list_t:new(Base)).

run_base(identity, M) ->
    identity:run(M);
run_base(state_m, M) ->
    state_m:run(M, 4);
run_base(error_m, M) ->
    error_m:run(M).

normalize(writer_t, identity, {A, W}) ->
    {A, undetermined:run(W, list)};
normalize(writer_t, state_m, {{A, W}, S}) ->
    {{A, undetermined:run(W, list)}, S};
normalize(writer_t, error_m, {ok, {A, W}}) ->
    {ok, {A, undetermined:run(W, list)}};
normalize(_Transformer, _Base, Value) ->
    Value.

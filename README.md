[![CI](https://github.com/slepher/erlando/actions/workflows/ci.yml/badge.svg?branch=master&event=push)](https://github.com/slepher/erlando/actions/workflows/ci.yml?query=branch%3Amaster)

[![CI](https://github.com/slepher/erlando/actions/workflows/release.yml/badge.svg?branch=2.11.11&event=push)](https://github.com/slepher/erlando/actions/workflows/release.yml?query=branch%3A2.11.11)

# Erlando

[中文](README.zh.md)

Erlando brings lightweight functional-programming extensions to Erlang. It
provides cut expressions, Haskell-style `do` notation, aliased imports, a
typeclass runtime, common typeclasses, monads, and monad transformers.

## Requirements

- Erlang/OTP 21 or later
- rebar3

The project CI configuration currently covers OTP 21 through OTP 29.

## Installation

Add Erlando as a dependency:

```erlang
{deps, [
    {erlando, {git, "https://github.com/slepher/erlando.git", {tag, "2.11.1"}}}
]}.
```

Projects that declare their own typeclasses or instances also need the
`rebar3_erlando` compiler plugin:

```erlang
{plugins, [
    {rebar3_erlando,
        {git, "https://github.com/slepher/rebar3_erlando.git", {tag, "0.3.0"}}}
]}.

{provider_hooks, [
    {post, [{compile, {erlando, compile}}]}
]}.
```

The hook collects typeclass and instance metadata after compilation and
generates the `typeclass` dispatch module. Applications that only use the
instances shipped by Erlando do not need to add this hook.

## Cut expressions

Include `cut.hrl` and use `_` in an expression to create a function. Each hole
becomes an argument, from left to right:

```erlang
-module(cut_example).
-include("cut.hrl").

add(A, B) -> A + B.

example() ->
    Add10 = add(10, _),
    15 = Add10(5),

    Pair = {_, _},
    {left, right} = Pair(left, right).
```

Abstraction happens at the shallowest enclosing expression. For example:

```erlang
list_to_binary([1, math:pow(2, _)])
```

is equivalent to:

```erlang
list_to_binary([1, fun(X) -> math:pow(2, X) end])
```

Cuts work in calls, operators, tuples, lists, maps, records, binaries, case
expressions, and comprehensions. Because a cut produces a normal `fun`, its
arguments follow Erlang's usual eager evaluation rules.

## Do notation

Include `do.hrl` to enable `do/1`. Erlando uses list-comprehension syntax for
monadic binding:

```erlang
-module(do_example).
-include("do.hrl").

safe_square(Value) ->
    do([monad_maybe ||
        true <- return(is_number(Value)),
        return(Value * Value)]).
```

A more typical error-handling example is:

```erlang
read_file(Path) ->
    do([error_m ||
        Handle <- file:open(Path, [read, binary]),
        Data <- file:read(Handle, 4096),
        file:close(Handle),
        return(Data)]).
```

Inside a `do` block:

- `Pattern <- Expression` binds through the selected monad.
- Ordinary expressions are sequenced through the monad.
- Calls to `return(...)` and `fail(...)` are directed to the selected monad.
- `Pattern = Expression` is a normal Erlang match.

The expression:

```erlang
do([Monad || A <- First, Next(A)])
```

is transformed conceptually into:

```erlang
monad:'>>='(First, fun(A) -> Next(A) end, Monad)
```

## Import aliases

The `import_as` parse transform imports a remote function under a local name:

```erlang
-module(import_example).
-compile({parse_transform, import_as}).

-import_as({lists, [{duplicate/2, dup}, {reverse/1, rev}]}).

example() ->
    [a, a, a] = dup(3, a),
    [3, 2, 1] = rev([1, 2, 3]).
```

The alias is implemented as a local function, so expressions such as
`fun dup/2` also work.

## Typeclasses

A typeclass is an Erlang behaviour marked with `-superclass/1`:

```erlang
-module(functor).
-superclass([]).

-callback fmap(fun((A) -> B), f(F, A), F) -> f(F, B).
```

Superclass relationships are declared by module name:

```erlang
-module(monad).
-superclass([applicative]).
```

Erlando includes these typeclasses:

- `functor`, `applicative`, `monad`
- `foldable`, `traversable`
- `alternative`, `monad_plus`
- `monad_reader`, `monad_writer`, `monad_state`, `monad_cont`
- `monad_error`, `monad_fail`, `monad_trans`, `monad_runner`
- `monoid`

The full typeclass entry points accept a type descriptor as their last
argument. Generated convenience forms can infer it from a registered runtime
value. A plain atom selects a concrete instance; a tuple can carry an inner
typeclass for a transformer:

```erlang
{identity, 2} = functor:fmap(fun(X) -> X + 1 end, {identity, 1}),

StateT = {state_t, identity},
StateValue = monad:return(ok, StateT).
```

## Declaring instances with `erlando_instance`

`-erlando_instance(...)` is the single source of truth for an instance. It
registers the represented type and its capabilities, adds the required Erlang
behaviours, generates requested callback adapters, and emits versioned BEAM
metadata for `rebar3_erlando`.

Every module that declares an instance must include the macro header:

```erlang
-include("erlando_instance.hrl").
```

### A local instance

Use `type` for the represented type, `adapters` for generated callback
adapters, and `manual` for capabilities whose callbacks are implemented by the
module itself:

```erlang
-module(identity).
-include("erlando_instance.hrl").

-erlando_instance(#{
    type => {identity, [identity/1]},
    adapters => [#{
        mode => target,
        patterns => [identity],
        capabilities => [functor, applicative, monad, monad_fail]
    }],
    manual => [monad_runner]
}).

-export_type([identity/1]).
-type identity(A) :: {identity, A}.
```

`mode => target` generates callbacks whose type descriptor is matched at the
end of the call. `patterns` lists the descriptor patterns accepted by the
adapter. This is the usual mode for a concrete, non-parameterized type.

The type name does not have to match the module name:

```erlang
-module(function_instance).
-include("erlando_instance.hrl").

-erlando_instance(#{
    type => {function, [function_instance/0]},
    adapters => [#{
        mode => target,
        patterns => [function],
        capabilities => [functor, applicative, monad, monad_reader]
    }],
    manual => [monad_runner]
}).
```

Here the typeclass registry maps the type `function` to the implementation
module `function_instance`.

### A parameterized transformer instance

Use a source adapter when the generated callback must forward an inner
typeclass descriptor to the implementation:

```erlang
-module(state_t).
-include("erlando_instance.hrl").

-erlando_instance(#{
    type => {state_t, [state_t/3]},
    adapters => [
        #{mode => source,
          requires => functor,
          capabilities => [functor]},
        #{mode => source,
          requires => monad,
          capabilities => [applicative, monad, monad_trans, monad_state]}
    ],
    manual => [monad_runner]
}).
```

`mode => source` forwards the inner descriptor required by `requires`. A
source adapter must specify either `requires` or explicit `args`. Adapter
groups may also use the lower-level `remote`, `patterns_group`, `extra_call`,
and `am` options supported by the callback generator.

### Implementing a capability in another module

An implementation module can provide one capability for several types:

```erlang
-module(monad_reader_instance).
-include("erlando_instance.hrl").

-erlando_instance(#{
    types => [state_t, cont_t, maybe_t, error_t, except_t, list_t],
    capability => monad_reader,
    implementation => generic
}).
```

This registers `monad_reader_instance` as the `monad_reader` implementation
for every listed type. The module supplies the generic callbacks itself; no
adapter functions are generated.

For example:

```erlang
monad_reader:ask(state_t)
```

is dispatched to:

```erlang
monad_reader_instance:ask(state_t)
```

### Per-type dispatch

When different types need different local callback implementations, use a
dispatch map. Every callback entry is `{LocalFunction, Arity}`:

```erlang
-erlando_instance(#{
    types => [reader_t, state_t],
    capability => monad_cont,
    implementation => {dispatch, #{
        reader_t => #{callCC => {reader_t_call_cc, 2}},
        state_t  => #{callCC => {state_t_call_cc, 2}}
    }}
}).
```

The macro exports the public typeclass callbacks and routes both atom and tuple
descriptors to the selected local function.

### Declaration reference

Top-level keys:

- `type`: one type name, or `{TypeName, [ExportedType/Arity, ...]}`.
- `types`: a non-empty list of type declarations for a shared implementation.
- `adapters`: adapter groups that generate callbacks.
- `manual`: capabilities implemented directly in the declaring module.
- `capability`: one capability implemented for every declared type.
- `implementation`: `generic`, or `{dispatch, DispatchMap}` when used with
  `capability`.

Adapter-group keys:

- `mode`: `target` or `source`.
- `capabilities`: typeclasses sharing this adapter configuration.
- `patterns`: descriptor patterns used by a target adapter.
- `requires`: inner typeclass required by a source adapter.
- `remote`: module that owns the underlying callback implementation.

A declaration must contain exactly one logical entry for each capability.
Duplicate types or capabilities are rejected during macro expansion, and a
source adapter without `requires` or `args` is invalid.

### Generated metadata and dispatch

The macro stores normalized, versioned `erlando_instance_meta` in the compiled
BEAM. After Erlando's compiler hook runs, the generated `typeclass` module
provides:

- `typeclass:is_typeclass/1` to identify registered typeclasses.
- `typeclass:module/2` to resolve `{Type, Typeclass}` to its implementation.
- `typeclass:type/1` to infer a registered type from a runtime value.

Metadata is compile-time input. Do not edit the generated `typeclass` module by
hand; change the `-superclass(...)` or `-erlando_instance(...)` declaration and
compile again.

## Included data types and transformers

The repository includes concrete instances such as `identity`, `either`,
`monad_maybe`, `error_m`, `reader_m`, `writer_m`, `state_m`, `cont_m`, lists,
functions, and tuples. It also includes `reader_t`, `writer_t`, `state_t`,
`cont_t`, `maybe_t`, `error_t`, `except_t`, and `list_t` transformers.

Transformer descriptors use `{Transformer, InnerTypeclass}`. For example:

```erlang
StateT = state_t:new(identity),
Computation = do([StateT ||
    monad_state:put(initial_state),
    Value <- monad_state:get(),
    return(Value)]),
identity:run(state_t:eval(Computation, undefined, StateT)).
```

## Development

Compile and run the test suite with:

```shell
rebar3 compile
rebar3 ct
```

Other configured checks include:

```shell
rebar3 xref
rebar3 dialyzer
```

## License

Erlando source files are distributed under the Mozilla Public License 1.1; see
the license header in each source file.

# Erlando

[English](README.md)

Erlando 为 Erlang 提供轻量的函数式编程扩展，包括 cut 表达式、Haskell
风格的 `do` 记法、带别名的导入、typeclass 运行时，以及常用的 typeclass、
Monad 和 Monad Transformer。

## 环境要求

- Erlang/OTP 21 或更高版本
- rebar3

项目当前的 CI 配置覆盖 OTP 21 到 OTP 29。

## 安装

将 Erlando 加入依赖：

```erlang
{deps, [
    {erlando, {git, "https://github.com/slepher/erlando.git", {tag, "2.11.1"}}}
]}.
```

如果项目要声明自己的 typeclass 或 instance，还需要配置 `rebar3_erlando`
编译插件：

```erlang
{plugins, [
    {rebar3_erlando,
        {git, "https://github.com/slepher/rebar3_erlando.git", {tag, "0.3.0"}}}
]}.

{provider_hooks, [
    {post, [{compile, {erlando, compile}}]}
]}.
```

该 hook 会在编译后收集 typeclass 与 instance metadata，并生成负责分派的
`typeclass` 模块。只使用 Erlando 自带 instance 的应用无需添加这个 hook。

## Cut 表达式

引入 `cut.hrl` 后，可以在表达式中使用 `_` 创建函数。每个空位按照从左到右
的顺序成为函数参数：

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

抽象发生在包含 `_` 的最浅层表达式。例如：

```erlang
list_to_binary([1, math:pow(2, _)])
```

等价于：

```erlang
list_to_binary([1, fun(X) -> math:pow(2, X) end])
```

Cut 可用于函数调用、运算符、元组、列表、Map、Record、Binary、case 表达式
和推导式。Cut 生成的是普通 `fun`，因此参数仍遵循 Erlang 的求值规则。

## Do 记法

引入 `do.hrl` 即可使用 `do/1`。Erlando 借用列表推导式语法表达 Monad 绑定：

```erlang
-module(do_example).
-include("do.hrl").

safe_square(Value) ->
    do([monad_maybe ||
        true <- return(is_number(Value)),
        return(Value * Value)]).
```

下面是一个更常见的错误处理示例：

```erlang
read_file(Path) ->
    do([error_m ||
        Handle <- file:open(Path, [read, binary]),
        Data <- file:read(Handle, 4096),
        file:close(Handle),
        return(Data)]).
```

在 `do` 块中：

- `Pattern <- Expression` 通过选定的 Monad 完成绑定。
- 普通表达式通过该 Monad 排列执行顺序。
- `return(...)` 和 `fail(...)` 调用会被转发给选定的 Monad。
- `Pattern = Expression` 仍是普通的 Erlang 匹配。

表达式：

```erlang
do([Monad || A <- First, Next(A)])
```

在概念上会转换为：

```erlang
monad:'>>='(First, fun(A) -> Next(A) end, Monad)
```

## 导入别名

`import_as` parse transform 可以用本地别名导入远程函数：

```erlang
-module(import_example).
-compile({parse_transform, import_as}).

-import_as({lists, [{duplicate/2, dup}, {reverse/1, rev}]}).

example() ->
    [a, a, a] = dup(3, a),
    [3, 2, 1] = rev([1, 2, 3]).
```

别名会实现为本地函数，因此也可以使用 `fun dup/2` 这样的表达式。

## Typeclass

Typeclass 是使用 `-superclass/1` 标记的 Erlang behaviour：

```erlang
-module(functor).
-superclass([]).

-callback fmap(fun((A) -> B), f(F, A), F) -> f(F, B).
```

父类关系通过模块名声明：

```erlang
-module(monad).
-superclass([applicative]).
```

Erlando 包含以下 typeclass：

- `functor`、`applicative`、`monad`
- `foldable`、`traversable`
- `alternative`、`monad_plus`
- `monad_reader`、`monad_writer`、`monad_state`、`monad_cont`
- `monad_error`、`monad_fail`、`monad_trans`、`monad_runner`
- `monoid`

完整的 typeclass 入口函数把类型描述符作为最后一个参数；生成的简写形式也可以
根据已注册的运行时值推断描述符。原子表示具体 instance；元组还可以携带
Transformer 的内部 typeclass：

```erlang
{identity, 2} = functor:fmap(fun(X) -> X + 1 end, {identity, 1}),

StateT = {state_t, identity},
StateValue = monad:return(ok, StateT).
```

## 使用 `erlando_instance` 声明 Instance

`-erlando_instance(...)` 是 instance 声明的唯一信息源。它会注册对应的类型与
能力，添加所需的 Erlang behaviour，按需生成 callback adapter，并为
`rebar3_erlando` 写入带版本的 BEAM metadata。

每个声明 instance 的模块都必须引入宏头文件：

```erlang
-include("erlando_instance.hrl").
```

### 本地 Instance

使用 `type` 声明对应类型，使用 `adapters` 生成 callback adapter，使用
`manual` 声明由模块自行实现 callback 的能力：

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

`mode => target` 会生成在调用末尾匹配类型描述符的 callback。`patterns`
列出 adapter 接受的描述符模式。具体且不带参数的类型通常使用这种模式。

类型名不必与模块名相同：

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

这里 typeclass 注册表会把类型 `function` 映射到实现模块
`function_instance`。

### 带参数的 Transformer Instance

如果生成的 callback 需要把内部 typeclass 描述符转发给实现，应使用 source
adapter：

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

`mode => source` 会转发 `requires` 指定的内部描述符。source adapter 必须指定
`requires` 或显式的 `args`。Adapter group 还可以使用 callback 生成器提供的
底层选项 `remote`、`patterns_group`、`extra_call` 和 `am`。

### 在其他模块中实现能力

一个实现模块可以为多个类型提供同一种能力：

```erlang
-module(monad_reader_instance).
-include("erlando_instance.hrl").

-erlando_instance(#{
    types => [state_t, cont_t, maybe_t, error_t, except_t, list_t],
    capability => monad_reader,
    implementation => generic
}).
```

该声明会把 `monad_reader_instance` 注册为所有列出类型的 `monad_reader` 实现。
模块自行提供通用 callback，宏不会生成 adapter 函数。

例如：

```erlang
monad_reader:ask(state_t)
```

会被分派到：

```erlang
monad_reader_instance:ask(state_t)
```

### 按类型分派

当不同类型需要不同的本地 callback 实现时，可以使用 dispatch map。每个
callback 条目都是 `{LocalFunction, Arity}`：

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

宏会导出公共的 typeclass callback，并把原子或元组形式的描述符路由到对应的
本地函数。

### 声明字段参考

顶层字段：

- `type`：一个类型名，或 `{TypeName, [ExportedType/Arity, ...]}`。
- `types`：由同一个模块实现的一组非空类型声明。
- `adapters`：生成 callback 的 adapter group。
- `manual`：由声明模块直接实现的能力。
- `capability`：为所有已声明类型实现的一项能力。
- `implementation`：与 `capability` 配合使用的 `generic` 或
  `{dispatch, DispatchMap}`。

Adapter group 字段：

- `mode`：`target` 或 `source`。
- `capabilities`：共用该 adapter 配置的 typeclass。
- `patterns`：target adapter 使用的描述符模式。
- `requires`：source adapter 所需的内部 typeclass。
- `remote`：拥有底层 callback 实现的模块。

每项能力在一个声明中只能出现一次。宏展开时会拒绝重复的类型或能力；未提供
`requires` 或 `args` 的 source adapter 也是无效声明。

### 生成的 Metadata 与分派

宏会把规范化且带版本的 `erlando_instance_meta` 写入编译后的 BEAM。Erlando
编译 hook 执行后，生成的 `typeclass` 模块提供：

- `typeclass:is_typeclass/1`：判断 typeclass 是否已注册。
- `typeclass:module/2`：把 `{Type, Typeclass}` 解析为实现模块。
- `typeclass:type/1`：根据运行时值推断已注册类型。

Metadata 是编译期输入。不要手工修改生成的 `typeclass` 模块；应修改
`-superclass(...)` 或 `-erlando_instance(...)` 声明后重新编译。

## 内置数据类型与 Transformer

仓库包含 `identity`、`either`、`monad_maybe`、`error_m`、`reader_m`、
`writer_m`、`state_m`、`cont_m`、列表、函数和元组等具体 instance，还包含
`reader_t`、`writer_t`、`state_t`、`cont_t`、`maybe_t`、`error_t`、
`except_t` 和 `list_t` Transformer。

Transformer 描述符使用 `{Transformer, InnerTypeclass}` 形式。例如：

```erlang
StateT = state_t:new(identity),
Computation = do([StateT ||
    monad_state:put(initial_state),
    Value <- monad_state:get(),
    return(Value)]),
identity:run(state_t:eval(Computation, undefined, StateT)).
```

## 开发

编译并运行测试：

```shell
rebar3 compile
rebar3 ct
```

项目还配置了以下检查：

```shell
rebar3 xref
rebar3 dialyzer
```

## 许可证

Erlando 源文件使用 Mozilla Public License 1.1；详见各源文件顶部的许可证
声明。

# Monad Transformer 实现与模块设计审计

## 结论

整体设计思路合理，采用了接近 Haskell `transformers + mtl` 的三层结构：

- `typeclass/*.erl`：类型类接口与动态派发
- `*_t.erl`：Transformer 本体
- `monad_*_instance.erl`：底层 effect 向上提升

七个 Transformer 中，`MaybeT`、`ErrorT`、`ReaderT`、`StateT`、`WriterT`、`ContT` 的核心 `Functor / Applicative / Monad / lift` 基本符合经典实现；`ListT` 的 effect 顺序和局部环境传播不符合“ListT done right”的行为。

跨 Transformer 的 `mtl` 实例目前不可靠，存在数个确定的运行时错误。因此整体还不能称为与原 Monad Transformer 表现一致。

## 实施状态（2026-08-01）

本轮已完成：

- 新增独立 `{except_t, Inner}` 的现代 `ExceptT`，保留原 `{error_t, Inner}` 和 API。
- 提供 `map_except_t/2`、`with_except_t/2` 以及与 ErrorT 的显式双向转换。
- 按现代 transformers 语义区分 `MonadFail` 与 `MonadError`：`ExceptT.fail` 提升底层失败，`throw_error` 才构造 `Left`。
- `ExceptT` 的 `mzero/mplus` 使用错误 Monoid 的 `mempty/mappend`，不沿用旧 ErrorT 简单丢弃第一个错误的行为。
- 修复 `lift_either/2` 的 Left/Right 反转。
- 删除 `error_m`、`either`、`error_t` 和新 `except_t` 对 handler `function_clause` 的吞噬。
- 修复 ReaderT 与 ErrorT 的 `MonadCont`，补齐 WriterT 和 ExceptT 的 `MonadCont`，并回归 StateT、MaybeT、ListT 与 ContT 本体。
- 为 ExceptT 注册 Reader/State/Writer/Cont 的 mtl 提升；StateT 已明确记录为 strict StateT。
- 重写 ListT append 为底层 Monad 的左偏递归展开；bind、join 和 Applicative 不再交错非交换 effect。
- 为 ListT 增加递归 `hoist/3`，使 `MonadReader.local` 覆盖整个流；补齐 join、catch_error 和 effect 顺序断言。

验证结果：ErLando 全套 Common Test 142 项通过，xref 通过；同级 async 项目在 `/tmp` 副本中指向当前 ErLando 后，原有 40 项测试全部通过。

尚未实施：完整的 Monad/Applicative/MonadTrans 法则属性测试，以及让能力注册与实例实现共享单一事实来源；async 自己的 `reply_t:try_emb/3` 仍需在 async 仓库中删除旧的 `function_clause` 回退行为。

语义基线：

- [MonadTrans 的两条 lift 定律](https://hackage.haskell.org/package/transformers-0.1.3.0/docs/Control-Monad-Trans.html)
- [transformers 包结构](https://hackage.haskell.org/package/transformers)
- [mtl 的 MonadCont 实例](https://ghc.gitlab.haskell.org/ghc/doc/libraries/mtl-2.3.2-inplace/Control-Monad-Cont-Class.html)
- [ListT done right](https://hackage.haskell.org/package/list-t/docs/ListT.html)

## 严重问题

### 1. `WriterT` 声明了 `MonadCont`，实际调用必崩

`src/instance/monad_transformer/monad_cont_instance.erl:11` 把 `writer_t` 注册为 `MonadCont` 实例，但 `lift_callCC/3` 没有 `writer_t` 分支。

实际结果：

```erlang
monad_cont:callCC(..., writer_t:new(cont_t:new(identity)))
%% => function_clause in monad_cont_instance:lift_callCC/3
```

这是“注册能力”和“实现能力”分离后发生漂移的直接例子。官方 `mtl` 为 `WriterT` 提供 `MonadCont` 实例，因此这里应补充实现，而不是保留一个不可调用的注册项。

### 2. `ErrorT` 的 `callCC` 使用了错误的类型类实例名

`src/instance/monad_transformer/monad_cont_instance.erl:41`：

```erlang
monad:return(A, error)
```

项目的 Either 实例名是 `either`，不是 `error`。逃逸 continuation 被调用时得到：

```erlang
{'EXIT', {unregisted_module, {error, monad}}}
```

这里应构造成功值 `{right, A}`，即使用 `either:return/1` 或等价操作。

### 3. `ReaderT` 的 `callCC` 返回环境，而不是传给 continuation 的值

`src/instance/monad_transformer/monad_cont_instance.erl:44-50` 中，外层函数参数 `A` 实际是环境 `R`：

```erlang
reader_t:reader_t(fun(A) ->
    F(fun(_) -> CC(A) end)
end)
```

因此 `K(value)` 在环境 `env` 下实际返回 `env`，而不是 `value`。正确结构应为：

```erlang
fun(R) ->
    CallCC(fun(CC) ->
        reader_t:run(
            F(fun(A) ->
                reader_t:reader_t(fun(_) -> CC(A) end)
            end),
            R)
    end)
end
```

### 4. `ListT` 的 append/bind 会错误地交错底层 effect

`ListT` 的表示接近正确的流式表示，但 `src/instance/monad_transformer/list_t.erl:225-228` 的 `append_flist/3` 使用 `applicative:lift_a2` 同时运行左右两侧：

```erlang
applicative:lift_a2(FAB, FListA, FListB, Applicative)
```

这会影响：

- `mplus/<|>`
- `join` 和 bind
- `<*>`

用 `State` 记录四个元素的求值顺序，当前结果是：

```erlang
Values  = [a,b,c,d]
Effects = [a1,b1,a2,b2]
```

左偏 ListT append 应该是：

```erlang
Effects = [a1,a2,b1,b2]
```

这不只是惰性差异；对 `State`、`Writer`、IO 等非交换底层 monad，会产生可观察的错误结果。

`append_flist` 应通过底层 `>>=` 先观察左侧：

```erlang
Left >>= fun
    (nil) ->
        Right;
    ({cons, A, Tail}) ->
        return({cons, A, append_flist(Tail, Right)})
end
```

### 5. `ListT` 的 `local` 只作用于第一个元素

`src/instance/monad_transformer/monad_reader_instance.erl:50-54` 的通用实现调用 Transformer 的 `map`；但 `src/instance/monad_transformer/list_t.erl:144-146` 的 `list_t:map/3` 只变换最外层 `m`，没有递归处理流尾。

复现结果：

```erlang
local(fun(N) -> N * 3 end,
      ListT [ask(), ask()])
```

以环境 `10` 运行：

```erlang
当前: [30,10]
正确: [30,30]
```

`ListT` 需要递归 `hoist` 整个流，而不是只 map 第一层。

### 6. `lift_either/2` 左右语义完全反了

`src/typeclass/monad_error.erl:53-59` 当前把：

- `{left, Value}` 当成功
- `{right, Reason}` 当错误

实测：

```erlang
lift_either({left, bad}, either)   %% {right,bad}
lift_either({right, good}, either) %% {left,good}
```

标准 Either 约定应为 `Left = error`、`Right = success`，项目自己的 `either.erl` 也是这个约定。

## 各 Transformer 审计结果

| Transformer | 核心语义 | 跨 effect 实例 |
|---|---|---|
| `MaybeT` | 基本符合；`nothing` 短路、`mplus` 左偏 | Reader/State/Writer/Error/Cont 实现基本合理 |
| `ErrorT` | 基本符合旧版 `ErrorT/ExceptT` | `MonadCont` 确定损坏 |
| `ReaderT` | 符合；环境传递与 `local` 正确 | `MonadCont` 确定损坏 |
| `StateT` | 符合 strict StateT；bind 正确传递新状态 | Error 捕获回滚到入口状态，符合经典实现 |
| `WriterT` | 符合；日志按左到右 `mappend` | `MonadCont` 被注册但没有实现 |
| `ContT` | 核心 CPS、`lift`、自身 `callCC` 基本符合 | Reader/State 提升合理 |
| `ListT` | 不符合非交换底层 monad 的语义 | `local` 只覆盖第一个 cell；其他实例也缺少系统性验证 |

## 模块划分评价

概念划分是合理的，尤其是按 effect 集中放置提升逻辑：

```text
typeclass API
    → typeclass_trans 动态派发
        → Transformer 自身实例
        → monad_*_instance 跨层实例
```

主要问题不是“模块太多”，而是能力注册和实现分离：

- `-erlando_type([...])` 是一份能力表。
- `lift_callCC/lift_local/lift_listen/...` 的函数分支又是另一份能力表。
- 编译器无法验证两者一致，所以出现了 `writer_t` 已注册但无实现的情况。
- `gen_fun`、`-behaviour`、exports 还形成第三份描述。例如 `maybe_t.erl:26-31` 没有声明 `alternative` 和 `monad_plus` behaviour，但后面实际实现并生成了这些能力。

建议保留现有三层架构，不需要大规模合并模块；应当让实例注册由实现声明生成，或者至少增加一项测试：枚举每个注册的 `{Transformer, Typeclass}`，验证所有必需 callback 都可调用。

类型类契约也需要收紧：

- `src/typeclass/monad_trans.erl:19` 的 callback 第二参数写成了底层 `M`，实际传入的是 `{T, M}`。
- `src/typeclass/monad_reader.erl:14` 把 `local` 的结果类型错误限制为环境类型 `R`。
- `src/typeclass/monad_writer.erl:13-16` 的 callback 把 Writer 输出硬编码成 list，而下面的公开 spec 又使用通用 `monoid:m(W)`。

这些不会立刻改变运行结果，但会削弱 Dialyzer 和 behaviour 检查，本来应该在编译期发现的问题被推迟到了运行时。

## 测试状况

- `rebar3 ct`：109 个测试全部通过。
- `rebar3 xref`：通过。
- CT 期间 Cover 对 `typeclass.beam` 报 `no_file_attribute`，不影响测试通过，但覆盖率数据可能不完整。

当前测试存在明显盲区：

- `test/list_t_SUITE.erl:193-195` 的逃逸 continuation 断言被注释。
- `test/list_t_SUITE.erl:210-211` 的错误传播断言被注释。
- `test/list_t_SUITE.erl:215-216` 的 `test_join` 是空测试。
- 没有用 `State`/`Writer` 这类非交换底层 monad 检查 ListT effect 顺序。
- 没有覆盖 `ReaderT/ErrorT/WriterT` 上的 `MonadCont`。
- 没有 Monad、Applicative、MonadTrans 法则测试。

## 建议修复顺序

1. 修复三处 `MonadCont`：`WriterT` 缺失、`ErrorT` 类型名错误、`ReaderT` 捕获变量错误。
2. 修复 `lift_either/2`，增加左右分支回归测试。
3. 重写 `ListT` 的 append/join，使右侧 action 延迟到左侧耗尽后执行。
4. 为 `ListT local` 增加递归 hoist。
5. 建立法则测试矩阵：每个 Transformer 至少对 `Identity`、`State` 和失败型底层 monad 验证 Monad/MonadTrans 定律。
6. 最后统一 behaviour、`-erlando_type`、`gen_fun` 和 callback 类型契约。

综合判断：核心 Transformer 主体大多可保留；优先重做 `ListT` 的组合逻辑，并修复跨 Transformer 的 `MonadCont` 实例。模块边界无需推倒重来，但实例注册机制必须变成可验证的单一事实来源。

## 新 `ExceptT` 对同级 `async` 项目的影响

审查范围是同父目录的独立项目 `/home/slepher/project/async`，不是 ErLando 子模块。`async` 当前在 `rebar.config:9` 固定依赖 ErLando `2.9.0`，所以新 `except_t` 发布后仍需显式升级依赖版本才会进入该项目。

### 结论

只要保留 `error_t`，并让新实现使用独立的 `{except_t, Inner}` 类型标签，**仅新增 `except_t` 对 async 没有直接行为影响**：生产代码没有调用 `error_t` 或 `except_t`，公开的 `async_m` 仍是 `async_t:new(identity)` 的特化版本。

不应直接用 `ExceptT` 替换 `reply_t`。`reply_t` 除了 `{error, Reason}`，还承载 `{message, Message}`、`ignore`、裸值与 `{ok, Value}`，是异步请求/多消息/最终回复协议的一部分；标准 `ExceptT` 只有 `Left/Right` 两条结果分支，不能等价表达这套协议。

若调用者把泛型 `async_t` 的底层 monad 改成 `except_t:new(M)`，会形成两个错误通道：

```text
async 协议错误：reply_t 的 {error, Reason}
底层 effect 错误：ExceptT 的 {left, Error}
```

`async_t:catch_error/3` 当前经由最外层 `reply_t` 捕获协议错误；底层 ExceptT 的 `Left` 会在产生 reply 之前短路，不能假定它也由同一个 handler 捕获。因此只有在有意区分“业务/远端回复错误”和“底层基础设施错误”时才应采用这种堆叠，并应分别运行和捕获两层。不要把 `Left/Right` 泄漏成 `async_m:wait*` 的新公开返回格式。

### `catch_error` 新语义带来的确定改动

ErLando 侧决定让 handler 自身的程序错误正常暴露后，async 还需要独立配套修改。`async/src/reply_t.erl:193-201` 的 `try_emb/3` 当前捕获 handler 的 `error:function_clause`，然后重新抛出原来的 monadic error；这会掩盖 handler 写错或模式不完整。

`async/test/async_t_SUITE.erl:172-176` 明确依赖旧行为：内层 handler 只匹配 `world`，实际收到 `world1`，其 `function_clause` 被吞掉后才由外层 handler 恢复。新语义下应：

1. 删除 `reply_t:try_emb/3` 对 `function_clause` 的捕获，直接运行 handler。
2. 将现有嵌套恢复测试改成 handler 正常匹配的传播测试。
3. 新增回归测试，断言不匹配的 handler 会暴露 `function_clause`，而不是回退成原来的 `{error, Reason}`。

仅修改 ErLando 的 `error_m`、`either`、`error_t` 或新 `except_t` 不足以改变 async 的该行为，因为 async 的 `reply_t` 自己实现了 `MonadError`。

### 其他兼容点

- `async_r_t` 的真实栈是 `StateT (ReaderT (ReaderT M))`。把 ErLando 的 `StateT` 明确定位为 strict StateT，只要实现保持现有严格求值与状态传递，async 无需迁移；需要重点回归 callback store、local state 和错误后的状态行为。
- `async_t` 自己在 `ReplyT (ContT (AsyncRT M))` 上实现 `callCC`。新 `except_t` 不会自动进入这条路径；但若允许 `ExceptT` 堆在 `ContT` 外层，ErLando 必须为 `{except_t, MonadCont}` 注册并正确实现 `MonadCont` 提升。
- `async/test/async_m_v5.erl` 是旧实现夹具，仍直接生成 `error_t` API。保留 ErrorT 后它应继续编译；不应批量重命名成 ExceptT。
- 新模块必须使用独立标签并加入对应的 Reader/State/Writer/Cont 提升注册；否则可能导致动态类型类派发缺失。唯一标签可避免与现有 `error_t` 冲突。

### async 集成验收

发布新 ErLando 版本并升级 async 依赖后，应至少执行：

1. async 全量 CT、xref 与编译，确认生成 API 未因类型注册变化而漂移。
2. `{error, Reason}` 短路、恢复、timeout、process down 和多消息后最终错误的协议回归。
3. `catch_error` handler 模式不匹配时暴露 `function_clause` 的新回归。
4. strict StateT 下 callback/local state 的保存、合并和恢复回归。
5. 若新增 `async_t:new(except_t:new(...))` 用例，分别验证 ReplyT 错误和 ExceptT 错误的 runner 顺序及捕获边界。

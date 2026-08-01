# Monad Transformer 实现与模块设计审计

## 当前结论

截至 2026-08-02，ErLando 的 Monad Transformer 行为修复和兼容性工作已经完成。当前实现以现代 `transformers + mtl` 为语义基线，同时保留旧 `ErrorT` API：

- `StateT` 明确定位为 strict StateT。
- `ErrorT` 为兼容接口；`ExceptT` 使用独立 `{except_t, Inner}` 标签，两者不冲突。
- `catch_error` 只处理 monadic error，handler 自身的 `function_clause` 等程序错误正常暴露。
- `ListT` 保留兼容实现，但其组合、effect 顺序和递归环境变换已经修正。
- 同父目录 async 已升级到 ErLando 2.10.0，并适配新的错误暴露语义。

模块仍采用合理的三层结构：

```text
typeclass API
    → typeclass_trans 动态派发
        → Transformer 自身实例
        → monad_*_instance 跨层实例
```

当前唯一明确保留的架构改进项，是让 capability 注册、函数分派、behaviour、exports 和 `gen_fun` 共享单一事实来源。该项涉及生成与注册机制重构，不属于本轮修改。

## 已解决问题

### Transformer 与 mtl 行为

- 修复 WriterT、ErrorT 和 ReaderT 的 `MonadCont`，补齐 ExceptT 的 `MonadCont`。
- 回归 StateT、MaybeT、ListT 和 ContT 的 continuation 行为。
- 为 ExceptT 注册 Reader、State、Writer 和 Cont 能力提升。
- 修复 `lift_either/2` 的 Left/Right 语义反转。
- 移除 `error_m`、`either`、`error_t`、`except_t` 对 handler `function_clause` 的吞噬。
- 新增现代 ExceptT，并提供 `map_except_t/2`、`with_except_t/2` 及 ErrorT 双向转换。
- ExceptT 的 `fail` 提升底层失败；`throw_error` 构造 Left。
- ExceptT 的 `mzero/mplus` 使用错误 Monoid 的 `mempty/mappend`。

### ListT

- append 改为通过底层 Monad 左偏递归展开，不再提前运行右侧 effect。
- bind、join 和 Applicative 不再交错非交换底层 effect。
- 增加递归 `hoist/3`，使 `MonadReader.local` 覆盖完整流，而非只覆盖第一个 cell。
- 补齐 join、catch_error、effect 顺序和 local 传播测试。

### 类型契约与 behaviour

- `monad_trans:lift/2` callback 的第二参数改为 `{Transformer, InnerMonad}` 描述符。
- `monad_reader:local/3` 不再把动作结果类型错误限制为环境类型。
- `monad_writer` callback 使用通用 `monoid:m(W)`，不再把 Writer 输出写死为列表。
- MaybeT 补充其已实现的 `alternative` 和 `monad_plus` behaviour 声明。
- 修复 Foldable 对不存在的 `monoid:monoid/1` 类型的引用。

### 其他静态检查问题

- 修复 `foldable:fold_map/3` 错误分派到 `foldmap` 的运行时问题。
- 修复列表 `fold_map` 的 Monoid 拼接顺序，使结果保持输入的从左到右顺序。
- 修复 `monad_runner` 将 `exit/2` 错用于构造本地异常的问题。
- 移除现代 OTP Dialyzer 已不支持的 `race_conditions` warning 配置。
- Cover 明确排除无源码的生成模块 `typeclass`，不再产生 `no_file_attribute` 警报。

## Transformer 状态

| Transformer | 当前定位 | 跨 effect 状态 |
|---|---|---|
| `MaybeT` | `nothing` 短路，`mplus` 左偏 | Reader/State/Writer/Error/Cont 已覆盖 |
| `ErrorT` | 兼容旧版 ErrorT | MonadCont 和错误恢复已修复 |
| `ExceptT` | 现代 ExceptT | Reader/State/Writer/Cont 已注册并测试 |
| `ReaderT` | 标准环境传递与 `local` | MonadCont 已修复 |
| `StateT` | strict StateT | 状态传递和错误回滚符合经典实现 |
| `WriterT` | 日志按左到右 `mappend` | MonadCont 已实现 |
| `ContT` | 标准 CPS、lift 与 callCC | Reader/State 提升已覆盖 |
| `ListT` | 兼容流式实现 | effect 顺序、local、Error/Cont 已覆盖 |

## 法则测试

`test/transformer_laws_SUITE.erl` 对全部八个 Transformer 建立统一矩阵：

- Monad：左单位元、右单位元、结合律。
- Applicative：identity、homomorphism、interchange、composition。
- MonadTrans：`lift (return a) = return a`。
- MonadTrans：`lift (m >>= f) = lift m >>= (lift . f)`。

Monad 和 Applicative 法则在 Identity 上观察完整 Transformer 结果；MonadTrans 法则分别使用 Identity、State 和错误型底层 Monad，覆盖纯值、可观察状态 effect 和失败短路。

`test/foldable_SUITE.erl` 通过公开 API 锁定 Foldable 分派和从左到右的 Monoid 顺序。

## async 兼容结果

同父目录 `/home/slepher/project/async` 已发布 0.5.4：

- ErLando 依赖固定为 2.10.0。
- 删除 `reply_t:try_emb/3` 的 `function_clause` 回退逻辑。
- handler 模式不匹配现在正常暴露 `function_clause`。
- Common Test 41 项通过，xref 通过。

`reply_t` 仍保留 `{ok, Value}`、`{error, Reason}`、`{message, Message}`、`ignore` 等 async 协议，不使用 ExceptT 替换。若调用者主动把 ExceptT 放入 async 的底层栈，ReplyT 协议错误与 ExceptT 底层错误仍是两个独立错误通道。

## 验证结果

- `rebar3 ct`：146 项通过。
- `rebar3 xref`：通过。
- `rebar3 dialyzer`：通过。
- `rebar3 cover --verbose`：通过，无 `typeclass.beam` 警报。
- 当前总行覆盖率：约 37%。

## 唯一未决项

能力信息目前仍在多个位置表达：

- `-erlando_type(...)` 注册动态实例。
- `-behaviour(...)` 声明 callback 契约。
- `-gen_fun(...)` 生成转发 API。
- `lift_callCC/lift_local/lift_listen/...` 等函数分支提供实际实现。

未来可以让实例实现声明生成注册信息，或增加编译期注册覆盖验证。但这属于生成器和实例注册架构调整；当前已知分支均有行为测试覆盖，不影响本轮行为正确性。

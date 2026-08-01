# ErLando 实例声明统一 Interface

## 实现状态

该设计已于 2026-08-02 实现。ErLando 内置实例已全部迁移到
`-erlando_instance(...)`；`rebar3_erlando` 0.3.0 优先消费 schema 1
的 `erlando_instance_meta`，并兼容旧属性协议。以下“当前编译链”描述的是迁移前的 legacy 路径，保留用于解释兼容设计。

## 目标

统一 capability 注册、Erlang behaviour、capability Adapter 生成和跨 Transformer 函数分派，同时保留当前模块划分：

```text
typeclass Interface
    → typeclass_trans 动态派发
        → Transformer 本体 Implementation
        → monad_*_instance 跨层 Implementation
```

本设计不合并类型类模块、Transformer 模块或跨层实例模块。目标是让每个 `{Type, Typeclass}` mapping 只声明一次，并在编译期发现注册、callback、Adapter 和 dispatch 不一致。

## Legacy 编译链

当前 `rebar3_erlando` 从 BEAM attributes 读取：

- `-superclass(...)`：识别 typeclass Interface；superclass 列表当前不参与继承展开。
- `-erlando_type(...)`：识别 type/tag 及 `typeclass:type/1` 所需 pattern。
- `-behaviour(...)`：声明 Implementation 支持的 capability。

插件将同一 Module 的 types 和 behaviours 做笛卡尔积，生成：

```text
{Type, Typeclass} → Implementation Module
```

最终写出生成的 `typeclass.beam`：

- `typeclass:type/1`
- `typeclass:is_typeclass/1`
- `typeclass:module/2`

`gen_fun` 是 Adapter 生成器。它从 `behaviour_info(callbacks)` 读取 callback，根据 `inner_type`、`remote`、patterns 和 arity mode 生成转发函数。它还用于 `map/run/with` 等非 capability helper，因此不能直接成为实例注册 Interface。

跨层实例 Module 使用 `-erlando_type([...]) + -behaviour(...)` 注册多个 Transformer，而实际语义由 `lift_callCC/lift_local/lift_listen/...` 等函数分支提供。当前编译链无法证明注册列表中的每个 type 都有实现分支。

## 设计结论

新增窄语义宏：

```erlang
-erlando_instance(...).
```

该宏是 capability mapping 的单一声明来源。它不接管 Transformer 的语义 Implementation，也不取代用于普通 helper 的 `gen_fun`。

新宏应生成或保留：

- `erlando_type` metadata
- Erlang behaviour attributes
- capability callback exports
- 必要的 `gen_fun` Adapter forms
- 规范化、带版本的 `erlando_instance_meta` BEAM metadata
- 特殊实例所需的公开 callback dispatch clauses

## 宏展开与 metadata 保留协议

`erlando_instance` 是 Astranaut attribute macro。`as_attr` 只负责让：

```erlang
-erlando_instance(Spec).
```

被识别为宏调用；展开完成后，原 attribute 会被生成 forms 替换，不会自动保留在 BEAM 中。因此宏必须显式生成真正的普通 metadata attribute。

宏声明建议为：

```erlang
-export_macro([
    {erlando_instance/2,
     [{inject_attrs, [module]},
      {as_attr, erlando_instance}]}
]).
```

宏展开结果必须包含：

```erlang
-erlando_instance_meta({1, NormalizedInstance}).
```

对应 abstract form：

```erlang
{attribute, Line, erlando_instance_meta,
 {1, NormalizedInstance}}
```

不能重新生成同名 `erlando_instance` attribute，否则 Astranaut scanner 会再次把它识别为宏并递归展开。

metadata 使用独立名称和显式版本号：

- `erlando_instance`：源码层宏 Interface。
- `erlando_instance_meta`：BEAM 层稳定消费 Interface。
- `1`：metadata schema 版本。

`NormalizedInstance` 不能直接保存未经处理的用户输入。宏必须先：

1. 展开 `?MODULE` 等编译期信息。
2. 补齐默认值。
3. 将 capability 列表展开成确定的 mapping。
4. 将 generic/dispatch/adapter 策略转成规范形式。
5. 排序所有 map/list 中不具语义的顺序，保证构建结果稳定。

建议规范化结构表达为普通 Erlang literal，例如：

```erlang
#{
    module => state_t,
    types => [state_t],
    instances => [
        #{type => state_t,
          typeclass => functor,
          implementation => local,
          adapter => #{adapter => source, requires => functor}},
        #{type => state_t,
          typeclass => monad,
          implementation => local,
          adapter => #{adapter => source, requires => monad}}
    ]
}
```

为了兼容一个 Module 中的多次宏调用，插件应使用：

```erlang
proplists:get_all_values(erlando_instance_meta, Attributes)
```

读取全部 metadata，而不是只取第一个值。插件必须拒绝未知 schema 版本，错误中包含 Module 和版本号。

## 普通实例 Interface

以 `StateT` 为例：

```erlang
-erlando_instance(#{
    type => {?MODULE, [state_t/3]},
    capabilities => [
        {functor, #{
            requires => functor,
            adapter => source
        }},
        {applicative, #{
            requires => monad,
            adapter => source
        }},
        {monad, #{
            requires => monad,
            adapter => source
        }},
        {monad_trans, #{
            requires => monad,
            adapter => source
        }},
        {monad_state, #{
            requires => monad,
            adapter => source
        }},
        {alternative, #{
            requires => monad_plus,
            adapter => source
        }},
        {monad_plus, #{
            requires => monad_plus,
            adapter => source
        }},
        {monad_runner, manual}
    ]
}).
```

字段含义：

- `type`：生成现有 `erlando_type` metadata，供类型识别和实例注册使用。
- `capabilities`：该 type 实现的 typeclass Interface。
- `requires`：生成 convenience Adapter 时要求的底层 capability。
- `adapter => source`：根据 typeclass callback 生成面向调用者的低 arity Adapter。
- `adapter => target`：为核心实现补充 type/descriptor 参数。
- `manual`：Module 自己实现并导出 callback，不生成 convenience Adapter。

具体字段名和 source/target 命名可在原型阶段调整，但必须保留“底层约束”“Adapter 策略”和“手写实现”三个概念。

`map/run/with` 等非 capability helper 继续显式使用 `gen_fun`：

```erlang
-gen_fun(#{args => monad, functions => [map/2, with/2]}).
```

## 跨层实例 Interface

### 通用 Implementation

如果 callback 对所有已声明 Transformer 使用同一套完全通用逻辑，可声明：

```erlang
-erlando_instance(#{
    types => [reader_t, writer_t, cont_t, maybe_t,
              error_t, except_t, list_t],
    capability => monad_state,
    implementation => generic
}).
```

`generic` 表示 callback Implementation 不依赖 type-specific 分支。宏生成注册 metadata 和 behaviour，但保留 Module 中的通用 callback Implementation。

### 特殊 dispatch Implementation

如果不同 Transformer 需要不同语义，必须显式提供每个 callback 的本地 Adapter：

```erlang
-erlando_instance(#{
    types => [reader_t, writer_t, state_t, maybe_t,
              error_t, except_t, list_t],
    capability => monad_cont,
    implementation => {dispatch, #{
        reader_t => #{callCC => {reader_t_call_cc, 2}},
        writer_t => #{callCC => {writer_t_call_cc, 2}},
        state_t  => #{callCC => {state_t_call_cc, 2}},
        maybe_t  => #{callCC => {maybe_t_call_cc, 2}},
        error_t  => #{callCC => {error_t_call_cc, 2}},
        except_t => #{callCC => {except_t_call_cc, 2}},
        list_t   => #{callCC => {list_t_call_cc, 2}}
    }}
}).
```

宏根据 typeclass callbacks 生成公开 dispatch：

```erlang
callCC(F, {reader_t, _} = T) -> reader_t_call_cc(F, T);
callCC(F, {writer_t, _} = T) -> writer_t_call_cc(F, T);
%% ...
callCC(F, Type) when is_atom(Type) ->
    callCC(F, {Type, monad_cont}).
```

本地 Adapter 函数保留实际 CPS/状态/日志语义。这样 registration 与 dispatch 来自同一个 map，而编译器可以检查被引用的本地函数是否存在。

对于拥有多个 callback 的 typeclass，dispatch map 必须覆盖每个已注册 type 的全部 required callbacks。可以支持 `default` Adapter 减少真正通用 callback 的重复，但展开后的规范化 metadata 必须是完整的 `{Type, Typeclass, Callback} → Adapter` mapping。

## 编译期验证

新增宏之前，先增强 `rebar3_erlando` validator。

必须检查：

1. 同一个 `{Type, Typeclass}` 不能由不同 Module 静默覆盖。
2. Implementation Module 必须导出 typeclass 的全部 required callbacks。
3. `gen_fun` 引用的 capability 必须存在于 Module 的 instance/behaviour 声明中。
4. dispatch 模式必须覆盖所有 `{Type, Callback}` 组合。
5. dispatch 引用的本地 Adapter 函数必须存在且 arity 正确。
6. 重复声明如果完全相同可以去重；不一致必须编译失败。
7. optional callbacks 不能被当作 required callback。

当前 `maps:merge` 对冲突 mapping 的静默覆盖必须改成显式错误。

不能依赖函数名自动反推 capability。helper、default Implementation、不同 arity Adapter 和宏生成函数会造成歧义；capability 是语义声明，必须显式存在。

也不能仅靠 AST 静态证明跨层语义正确。编译期 validator 负责结构完整性，行为矩阵测试负责运行语义。

## 兼容策略

第一阶段，新宏展开为现有标准 attributes 和 forms：

- `erlando_type`
- `behaviour`
- `gen_fun` 生成结果

因此 `rebar3_erlando` 初期仍可以按旧协议消费 BEAM，无需立即切换 registry 格式。

同时由宏显式生成带版本的 `erlando_instance_meta` attribute，为后续插件直接读取新格式做准备。不能依赖宏调用本身被保留。

兼容周期内：

- 外部项目仍可使用旧 `-erlando_type/-behaviour/-gen_fun` 写法。
- 新宏和旧写法同时存在时必须验证结果一致。
- 新宏完成全量迁移后，再对 capability 相关的旧组合写法发出 deprecated warning。
- 非 capability helper 的 `gen_fun` 不废弃。

## 实施阶段

### 阶段一：characterization 与 validator

在 `rebar3_erlando` 中增加：

- 当前 mapping 生成的 characterization tests。
- callback export 验证。
- mapping 冲突检测。
- `gen_fun`/behaviour capability 一致性验证。
- 缺失 callback 和冲突 mapping 的失败型 fixture。

该阶段不改变 ErLando 源码声明。

### 阶段二：抽取 Adapter 生成核心

从 `gen_fun_macro` 抽取纯生成 Implementation，使以下两个 Interface 共用同一逻辑：

- 旧 `gen_fun`
- 新 `erlando_instance`

旧 `gen_fun` 的现有输出必须保持兼容。

### 阶段三：实现新宏与 metadata 协议

新增建议文件：

```text
src/erlando_instance_macro.erl
include/erlando_instance.hrl
```

宏负责规范化声明、显式生成 `erlando_instance_meta`、生成兼容 attributes、behaviours、capability Adapter 和可选 dispatch。

该阶段必须增加一个 BEAM-level 测试：编译 fixture 后通过 `beam_lib:chunks(Beam, [attributes])` 读取并断言 `erlando_instance_meta` 的版本和规范化内容。只检查宏返回 forms 不足以证明 metadata 真正进入 BEAM。

### 阶段四：迁移代表 Module

先迁移三类 Adapter：

1. `identity` 或 `list_instance`：直接实例、target arity Adapter。
2. `state_t`：带底层约束的 Transformer、source arity Adapter。
3. `monad_cont_instance`：逐 type 特殊 dispatch。

三类迁移通过后再冻结宏 Interface。

### 阶段五：行为矩阵与全量迁移

- 枚举规范化 `{Type, Typeclass}` mapping。
- 为通用跨层实例增加代表性调用测试。
- 为特殊 dispatch 验证每个 registered type。
- 迁移其余直接实例和跨层实例。
- 更新 README 和 review。

### 阶段六：插件读取规范化 metadata

兼容验证稳定后，允许 `rebar3_erlando` 优先读取 `erlando_instance_meta` metadata，并回退到旧 attributes。

只有经过一个外部兼容周期后，才考虑停止通过 `erlando_type × behaviour` 推导 capability mapping。

## 测试与验收

### 宏测试

- 普通实例展开结果。
- source/target arity Adapter。
- manual capability。
- generic 跨层 Implementation。
- dispatch 跨层 Implementation。
- callback exports 和 original function 合并。
- optional callbacks。

### 插件测试

- 规范化 mapping 生成正确。
- legacy attributes 保持兼容。
- 相同 mapping 去重。
- 冲突 mapping 编译失败。
- 缺失 callback 编译失败。
- 缺失 dispatch Adapter 编译失败。
- `gen_fun` capability 漂移编译失败。

### 集成验收

- ErLando Common Test 全部通过。
- xref 通过。
- Dialyzer 通过。
- Cover 无生成模块警报。
- async Common Test 和 xref 通过。
- OTP 21 与 OTP 29 的 docker CI 通过。
- 生成的 `typeclass:module/2` mapping 与迁移前一致。

## 不采用的方案

### 直接扩展 `gen_fun` 成 capability 宏

不采用。`gen_fun` 同时服务 capability Adapter 和普通 helper forwarding，把 registration 塞入其中会扩大 Interface，并让没有 `gen_fun` 的跨层实例无法统一。

### 从实际函数自动猜测 capability

不采用。函数名和 arity 不能可靠表达语义，default/helper/remote forwarding 会造成歧义。

### 为每个 `{Type, Typeclass}` 创建单独 Module

不采用。它会产生大量浅层 Adapter Module，降低 Locality，并把同一 effect 的提升规则分散到多个文件。

### 让宏生成 Transformer 语义函数体

不采用。宏只生成声明、Adapter 和 dispatch boilerplate；`lift_local/lift_listen/callCC` 等真实语义必须留在对应 Implementation Module。

## 最终判断

应当新增 `-erlando_instance(...)` 宏，但必须遵循以下顺序：

1. 先增加编译期 validator。
2. 再抽取并复用 `gen_fun` Adapter 生成核心。
3. 然后实现兼容型 instance 宏。
4. 最后迁移特殊 dispatch 和其余实例。

新宏解决 capability mapping 的单一声明问题；插件 validator 和行为矩阵共同保证结构与语义正确。仅把旧三个属性包装进一个宏，不能解决函数分支漂移问题。

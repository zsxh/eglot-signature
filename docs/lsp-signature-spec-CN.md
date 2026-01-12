# LSP Signature Help 协议规范 (3.17)

## 概述

Signature Help 请求从客户端发送到服务器，用于请求给定光标位置的签名信息。通常在用户输入函数调用参数时触发，显示可用的函数签名及其参数。

## 协议流程

```
┌─────────────┐                    ┌─────────────┐
│   Client    │                    │   Server    │
└──────┬──────┘                    └──────┬──────┘
       │                                   │
       │  textDocument/signatureHelp       │
       │  ──────────────────────────────>  │
       │                                   │
       │     SignatureHelp | null         │
       │  <──────────────────────────────  │
       │                                   │
```

## 客户端能力 (Client Capability)

客户端通过 `initialize` 请求声明支持的能力：

```typescript
export interface SignatureHelpClientCapabilities {
	/**
	 * Whether signature help supports dynamic registration.
	 */
	dynamicRegistration?: boolean;

	/**
	 * The client supports the following `SignatureInformation`
	 * specific properties.
	 */
	signatureInformation?: {
		/**
		 * Client supports the follow content formats for the documentation
		 * property. The order describes the preferred format of the client.
		 */
		documentationFormat?: MarkupKind[];

		/**
		 * Client capabilities specific to parameter information.
		 */
		parameterInformation?: {
			/**
			 * The client supports processing label offsets instead of a
			 * simple label string.
			 *
			 * @since 3.14.0
			 */
			labelOffsetSupport?: boolean;
		};

		/**
		 * The client supports the `activeParameter` property on
		 * `SignatureInformation` literal.
		 *
		 * @since 3.16.0
		 */
		activeParameterSupport?: boolean;
	};

	/**
	 * The client supports to send additional context information for a
	 * `textDocument/signatureHelp` request. A client that opts into
	 * contextSupport will also support the `retriggerCharacters` on
	 * `SignatureHelpOptions`.
	 *
	 * @since 3.15.0
	 */
	contextSupport?: boolean;
}
```

## 服务器能力 (Server Capability)

服务器通过 `signatureHelpProvider` 声明提供的能力：

```typescript
export interface SignatureHelpOptions extends WorkDoneProgressOptions {
	/**
	 * The characters that trigger signature help
	 * automatically.
	 */
	triggerCharacters?: string[];

	/**
	 * List of characters that re-trigger signature help.
	 *
	 * These trigger characters are only active when signature help is already
	 * showing. All trigger characters are also counted as re-trigger
	 * characters.
	 *
	 * @since 3.15.0
	 */
	retriggerCharacters?: string[];
}
```

## 请求 (Request)

**方法**: `textDocument/signatureHelp`

**参数**: `SignatureHelpParams`

```typescript
export interface SignatureHelpParams extends TextDocumentPositionParams,
	WorkDoneProgressParams {
	/**
	 * The signature help context. This is only available if the client
	 * specifies to send this using the client capability
	 * `textDocument.signatureHelp.contextSupport === true`
	 *
	 * @since 3.15.0
	 */
	context?: SignatureHelpContext;
}

/**
 * How a signature help was triggered.
 *
 * @since 3.15.0
 */
export namespace SignatureHelpTriggerKind {
	/**
	 * Signature help was invoked manually by the user or by a command.
	 */
	export const Invoked: 1 = 1;
	/**
	 * Signature help was triggered by a trigger character.
	 */
	export const TriggerCharacter: 2 = 2;
	/**
	 * Signature help was triggered by the cursor moving or by the document
	 * content changing.
	 */
	export const ContentChange: 3 = 3;
}

export type SignatureHelpTriggerKind = 1 | 2 | 3;

/**
 * Additional information about the context in which a signature help request
 * was triggered.
 *
 * @since 3.15.0
 */
export interface SignatureHelpContext {
	/**
	 * Action that caused signature help to be triggered.
	 */
	triggerKind: SignatureHelpTriggerKind;

	/**
	 * Character that caused signature help to be triggered.
	 *
	 * This is undefined when triggerKind !==
	 * SignatureHelpTriggerKind.TriggerCharacter
	 */
	triggerCharacter?: string;

	/**
	 * `true` if signature help was already showing when it was triggered.
	 *
	 * Retriggers occur when the signature help is already active and can be
	 * caused by actions such as typing a trigger character, a cursor move, or
	 * document content changes.
	 */
	isRetrigger: boolean;

	/**
	 * The currently active `SignatureHelp`.
	 *
	 * The `activeSignatureHelp` has its `SignatureHelp.activeSignature` field
	 * updated based on the user navigating through available signatures.
	 */
	activeSignatureHelp?: SignatureHelp;
}
```

## 响应 (Response)

**结果**: `SignatureHelp | null`

```typescript
/**
 * Signature help represents the signature of something
 * callable. There can be multiple signature but only one
 * active and only one active parameter.
 */
export interface SignatureHelp {
	/**
	 * One or more signatures. If no signatures are available the signature help
	 * request should return `null`.
	 */
	signatures: SignatureInformation[];

	/**
	 * The active signature. If omitted or the value lies outside the
	 * range of `signatures` the value defaults to zero or is ignore if
	 * the `SignatureHelp` as no signatures.
	 *
	 * Whenever possible implementors should make an active decision about
	 * the active signature and shouldn't rely on a default value.
	 *
	 * In future version of the protocol this property might become
	 * mandatory to better express this.
	 */
	activeSignature?: uinteger;

	/**
	 * The active parameter of the active signature. If omitted or the value
	 * lies outside the range of `signatures[activeSignature].parameters`
	 * defaults to 0 if the active signature has parameters. If
	 * the active signature has no parameters it is ignored.
	 *
	 * Since version 3.16.0 the `SignatureInformation` itself provides a
	 * `activeParameter` property and it should be used instead of this one.
	 */
	activeParameter?: uinteger;
}

/**
 * Represents the signature of something callable. A signature
 * can have a label, like a function-name, a doc-comment, and
 * a set of parameters.
 */
export interface SignatureInformation {
	/**
	 * The label of this signature. Will be shown in
	 * the UI.
	 */
	label: string;

	/**
	 * The human-readable doc-comment of this signature. Will be shown
	 * in the UI but can be omitted.
	 */
	documentation?: string | MarkupContent;

	/**
	 * The parameters of this signature.
	 */
	parameters?: ParameterInformation[];

	/**
	 * The index of the active parameter.
	 *
	 * If provided, this is used in place of `SignatureHelp.activeParameter`.
	 *
	 * @since 3.16.0
	 */
	activeParameter?: uinteger;
}

/**
 * Represents a parameter of a callable-signature. A parameter can
 * have a label and a doc-comment.
 */
export interface ParameterInformation {

	/**
	 * The label of this parameter information.
	 *
	 * Either a string or an inclusive start and exclusive end offsets within
	 * its containing signature label. (see SignatureInformation.label). The
	 * offsets are based on a UTF-16 string representation as `Position` and
	 * `Range` does.
	 *
	 * *Note*: a label of type string should be a substring of its containing
	 * signature label. Its intended use case is to highlight the parameter
	 * label part in the `SignatureInformation.label`.
	 */
	label: string | [uinteger, uinteger];

	/**
	 * The human-readable doc-comment of this parameter. Will be shown
	 * in the UI but can be omitted.
	 */
	documentation?: string | MarkupContent;
}
```

## 触发类型详解 (triggerKind)

### 1. Invoked (triggerKind = 1) - 手动触发

**触发条件**: 用户通过快捷键或命令主动调用签名帮助

**特点**:
- 首次调用，无先前状态
- `isRetrigger: false`
- 无 `activeSignatureHelp`

**请求示例**:
```json
{
    "context": {
        "triggerKind": 1,
        "isRetrigger": false
    }
}
```

### 2. TriggerCharacter (triggerKind = 2) - 字符触发

**触发条件**: 用户输入了服务器声明的 `triggerCharacters`（如 `(`、`,`）

**特点**:
- `triggerCharacter` 字段存在，指示触发的字符
- 首次触发时 `isRetrigger: false`

**请求示例**:
```json
{
    "context": {
        "triggerKind": 2,
        "triggerCharacter": "(",
        "isRetrigger": false
    }
}
```

### 3. ContentChange (triggerKind = 3) - 内容变化

**触发条件**: 签名帮助已显示，用户继续编辑代码或移动光标

**特点**:
- 必然是重新触发：`isRetrigger: true`
- 必须包含 `activeSignatureHelp`，传递当前签名状态
- 用于更新参数高亮位置，避免签名提示意外消失

**请求示例**:
```json
{
    "context": {
        "triggerKind": 3,
        "isRetrigger": true,
        "activeSignatureHelp": {
            "signatures": [
                {
                    "label": "func(name: str, age: int)",
                    "activeParameter": 0
                }
            ],
            "activeSignature": 0,
            "activeParameter": 0
        }
    }
}
```

**触发场景汇总**:

| 场景 | triggerKind | triggerCharacter | isRetrigger | activeSignatureHelp |
|------|-------------|------------------|-------------|---------------------|
| 按快捷键 | 1 | 无 | false | 无 |
| 输入 `(` | 2 | `(` | false | 无 |
| 输入 `,`（重触发字符） | 2 | `,` | true | 有 |
| 继续编辑/光标移动 | 3 | 无 | true | 有 |

## 关键概念详解

### activeParameter 的两个位置

`activeParameter` 可以出现在两个位置：

| 位置 | 版本 | 说明 |
|------|------|------|
| `SignatureHelp.activeParameter` | 3.0+ | 所有签名共享一个值（已弃用） |
| `SignatureInformation.activeParameter` | 3.16.0+ | 每个签名独立维护（推荐） |

**客户端读取优先级**：

1. 优先使用 `SignatureInformation.activeParameter`
2. 如果不存在，回退到 `SignatureHelp.activeParameter`

**为什么需要两个？**

函数重载场景下，不同重载的参数数量不同：

```json
{
    "signatures": [
        {
            "label": "func(name: str, age: int)",
            "activeParameter": 1  // 两参数版本，在第 2 个参数
        },
        {
            "label": "func(name: str)",
            "activeParameter": 0  // 单参数版本，仍在第 1 个参数
        }
    ]
}
```

### 参数标签的两种格式

#### 字符串标签

```json
{
    "label": "print(*args, **kwargs)",
    "parameters": [
        { "label": "*args" },
        { "label": "**kwargs" }
    ]
}
```

#### 偏移量标签（需客户端支持 labelOffsetSupport）

```json
{
    "label": "print(*args, **kwargs)",
    "parameters": [
        { "label": [6, 11] },   // 指向 "*args"
        { "label": [13, 21] }   // 指向 "**kwargs"
    ]
}
```

## 完整示例

### 请求（触发字符）

```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "textDocument/signatureHelp",
    "params": {
        "textDocument": { "uri": "file:///path/to/file.py" },
        "position": { "line": 10, "character": 25 },
        "context": {
            "triggerKind": 2,
            "triggerCharacter": "(",
            "isRetrigger": false
        }
    }
}
```

### 响应（使用偏移量标签）

```json
{
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
        "signatures": [
            {
                "label": "print(*args, **kwargs) -> None",
                "documentation": "Print values to stdout",
                "parameters": [
                    {
                        "label": [6, 11],
                        "documentation": "Variable length argument list"
                    },
                    {
                        "label": [13, 21],
                        "documentation": "Arbitrary keyword arguments"
                    }
                ],
                "activeParameter": 0
            }
        ],
        "activeSignature": 0
    }
}
```

## 实现要点

### 客户端实现

1. **能力检测**：检查 `labelOffsetSupport` 和 `activeParameterSupport`
2. **触发逻辑**：
   - 未显示时：检查 `triggerCharacters`
   - 已显示时：检查 `triggerCharacters + retriggerCharacters`
3. **读取 activeParameter**：优先从 `SignatureInformation` 读取
4. **渲染参数标签**：
   - 字符串：在签名中搜索匹配
   - 偏移量：直接高亮指定范围

### 服务器实现

1. **触发字符配置**：根据语言特性设置
2. **响应格式**：
   - 优先使用 `SignatureInformation.activeParameter`
   - 如果客户端支持 `labelOffsetSupport`，使用偏移量标签
   - 保留 `SignatureHelp.activeParameter` 以向后兼容

## 版本历史

| 版本 | 新特性 |
|------|--------|
| 3.17.0 | 当前版本 |
| 3.16.0 | `SignatureInformation.activeParameter` |
| 3.15.0 | `contextSupport`、`retriggerCharacters` |
| 3.14.0 | `labelOffsetSupport` |

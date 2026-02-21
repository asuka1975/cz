# 04 - 演算子と式

## 概要

Cz の式は値を生成する構文要素である。MS1 ではすべての式は `i32` 型の値を生成する。

## 構文

```bnf
expression       = logical_or
logical_or       = logical_and ("||" logical_and)*
logical_and      = equality ("&&" equality)*
equality         = comparison (("==" | "!=") comparison)*
comparison       = addition (("<" | ">" | "<=" | ">=") addition)*
addition         = multiplication (("+" | "-") multiplication)*
multiplication   = unary (("*" | "/" | "%") unary)*
unary            = ("!" | "-") unary | primary
primary          = integer_literal
                 | "true" | "false"
                 | identifier
                 | function_call
                 | "(" expression ")"
                 | if_expression
                 | block_expression
function_call    = identifier "(" [expression ("," expression)*] ")"
block_expression = "{" statement* [expression] "}"
if_expression    = "if" expression block_expression ["else" (block_expression | if_expression)]
```

## 意味論

### 演算子の優先順位 (低い順)

| 優先順位 | 演算子 | 結合性 | 説明 |
|----------|--------|--------|------|
| 1 (最低) | `\|\|` | 左 | 論理 OR |
| 2 | `&&` | 左 | 論理 AND |
| 3 | `==` `!=` | 左 | 等価比較 |
| 4 | `<` `>` `<=` `>=` | 左 | 順序比較 |
| 5 | `+` `-` | 左 | 加算・減算 |
| 6 | `*` `/` `%` | 左 | 乗算・除算・剰余 |
| 7 (最高) | `!` `-` (単項) | 右 | 論理否定・符号反転 |

### 算術演算子

- `+`: 加算
- `-`: 減算 (二項)、符号反転 (単項)
- `*`: 乗算
- `/`: 整数除算 (ゼロ除算はランタイムエラー)
- `%`: 剰余 (ゼロ除算はランタイムエラー)

### 比較演算子

- `==`: 等しい
- `!=`: 等しくない
- `<`: 小さい
- `>`: 大きい
- `<=`: 以下
- `>=`: 以上

比較演算子は `true` (1) または `false` (0) を返す。

### 論理演算子

- `&&`: 論理 AND (短絡評価)
- `||`: 論理 OR (短絡評価)
- `!`: 論理否定 (単項)

論理演算子は短絡評価 (short-circuit evaluation) を行う:
- `a && b`: `a` が `false` (0) なら `b` を評価せず `0` を返す
- `a || b`: `a` が `true` (非 0) なら `b` を評価せず `a` の値を返す

### ブロック式

ブロック (`{` ... `}`) は式として使用できる。ブロック内の最後の要素がセミコロンなしの式であれば、その値がブロックの値となる。

```cz
let x: i32 = {
    let a: i32 = 1;
    let b: i32 = 2;
    a + b  // セミコロンなし → ブロックの値は 3
};
```

最後の要素がセミコロン付きの文であるブロックは、値を持たない。値が期待される文脈でこのようなブロックを使用するとコンパイルエラーとなる。

### if 式

`if`/`else` は式であり、値を返す。詳細は [05-control-flow.md](./05-control-flow.md) を参照。

## 例

```cz
// 算術演算
let a: i32 = 2 + 3 * 4;     // 14 (乗算が優先)
let b: i32 = (2 + 3) * 4;   // 20 (括弧で優先順位を変更)

// 比較と論理演算
let c: i32 = 10;
let d: i32 = if c > 5 && c < 20 { 1 } else { 0 };

// 単項演算子
let e: i32 = -42;
let f: i32 = !true;  // 0

// ブロック式
let g: i32 = {
    let tmp: i32 = 10;
    tmp * 2
};
```

## 制約・制限

- ゼロ除算 (`/` および `%`) はランタイムエラーとなる
- 整数オーバーフローの動作は未定義
- ビット演算子 (`&`, `|`, `^`, `<<`, `>>`) は MS1 ではサポートしない

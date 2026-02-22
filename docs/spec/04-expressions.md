# 04 - 演算子と式

## 概要

Cz の式は値を生成する構文要素である。MS2 では複数の型に対する演算、型キャスト、データ構造の構築・アクセスをサポートする。

## 構文

```bnf
expression       = assignment_expr
assignment_expr  = identifier "=" expression | logical_or
logical_or       = logical_and ("||" logical_and)*
logical_and      = equality ("&&" equality)*
equality         = comparison (("==" | "!=") comparison)*
comparison       = addition (("<" | ">" | "<=" | ">=") addition)*
addition         = multiplication (("+" | "-") multiplication)*
multiplication   = unary (("*" | "/" | "%") unary)*
unary            = ("!" | "-") unary | cast_expr
cast_expr        = postfix ("as" type)*
postfix          = primary ("." (identifier | integer_literal))*
primary          = integer_literal
                 | float_literal
                 | "true" | "false"
                 | "()"
                 | identifier
                 | function_call
                 | "(" expression ")"
                 | tuple_expression
                 | struct_expression
                 | if_expression
                 | match_expression
                 | while_expression
                 | block_expression
function_call    = identifier "(" [expression ("," expression)*] ")"
tuple_expression = "(" expression "," expression ("," expression)* ")"
struct_expression = identifier "{" [field_init ("," field_init)* [","]] "}"
field_init       = identifier ":" expression
block_expression = "{" statement* [expression] "}"
if_expression    = "if" expression block_expression ["else" (block_expression | if_expression)]
match_expression = "match" expression "{" match_arm ("," match_arm)* [","] "}"
match_arm        = pattern "=>" expression
while_expression = [label ":"] "while" expression block_expression
```

## 意味論

### 演算子の優先順位 (低い順)

| 優先順位 | 演算子 | 結合性 | 説明 |
|----------|--------|--------|------|
| 1 (最低) | `=` | 右 | 代入 |
| 2 | `\|\|` | 左 | 論理 OR |
| 3 | `&&` | 左 | 論理 AND |
| 4 | `==` `!=` | 左 | 等価比較 |
| 5 | `<` `>` `<=` `>=` | 左 | 順序比較 |
| 6 | `+` `-` | 左 | 加算・減算 |
| 7 | `*` `/` `%` | 左 | 乗算・除算・剰余 |
| 8 | `!` `-` (単項) | 右 | 論理否定・符号反転 |
| 9 | `as` | 左 | 型キャスト |
| 10 (最高) | `.` | 左 | フィールドアクセス |

### 算術演算子

- `+`: 加算 (整数型・浮動小数点型)
- `-`: 減算 (二項)、符号反転 (単項) (整数型・浮動小数点型)
- `*`: 乗算 (整数型・浮動小数点型)
- `/`: 除算 (整数型: ゼロ除算はランタイムエラー、浮動小数点型: IEEE 754 に従う)
- `%`: 剰余 (整数型のみ、ゼロ除算はランタイムエラー)

両辺の型は同一でなければならない。異なる型同士の演算はコンパイルエラー。

### 比較演算子

- `==`, `!=`: 等価比較 (整数型・浮動小数点型・`bool` 型)
- `<`, `>`, `<=`, `>=`: 順序比較 (整数型・浮動小数点型)

比較演算子は `bool` 型を返す。両辺の型は同一でなければならない。

### 論理演算子

- `&&`: 論理 AND (短絡評価)
- `||`: 論理 OR (短絡評価)
- `!`: 論理否定 (単項)

論理演算子のオペランドは `bool` 型でなければならない。結果も `bool` 型。

短絡評価:
- `a && b`: `a` が `false` なら `b` を評価せず `false` を返す
- `a || b`: `a` が `true` なら `b` を評価せず `true` を返す

### 型キャスト式 (MS2)

`as` キーワードで型変換を行う。詳細なキャストルールは [02-types.md](./02-types.md) を参照。

```cz
let x: i64 = 42 as i64;
let y: bool = 1 as bool;
```

### フィールドアクセス (MS2)

`.` 演算子で構造体のフィールドまたはタプルの要素にアクセスする。

```cz
// 構造体フィールド
let p = Point { x: 1, y: 2 };
let px = p.x;

// タプルインデックス
let t = (10, true);
let first = t.0;    // 10
let second = t.1;    // true
```

タプルインデックスは非負の整数リテラルでなければならない。

### 構造体式 (MS2)

構造体の値は `構造体名 { フィールド名: 値, ... }` で構築する。

```cz
let p = Point { x: 1, y: 2 };
```

- すべてのフィールドを初期化しなければならない
- フィールドの順序は定義順と異なっていてもよい
- 末尾カンマは許可

### タプル式 (MS2)

タプルは `(値1, 値2, ...)` で構築する。

```cz
let pair = (42, true);
let triple = (1, 2.0, false);
```

- タプルの要素数は 2 以上
- 各要素は異なる型でもよい
- 単一要素のタプルは存在しない (`(42)` は括弧付き式)

### ブロック式

ブロック (`{` ... `}`) は式として使用できる。ブロック内の最後の要素がセミコロンなしの式であれば、その値がブロックの値となる。

最後の要素がセミコロン付きの文であるブロックは `()` 型の値を持つ。

### if 式

`if`/`else` は式であり、値を返す。詳細は [05-control-flow.md](./05-control-flow.md) を参照。

### match 式 (MS2)

`match` は式であり、パターンマッチの結果に応じた値を返す。詳細は [05-control-flow.md](./05-control-flow.md) および [09-pattern-matching.md](./09-pattern-matching.md) を参照。

### while 式 (MS2)

`while` は式であり、`break` の値または `()` を返す。詳細は [05-control-flow.md](./05-control-flow.md) を参照。

## 例

```cz
// 算術演算
let a = 2 + 3 * 4;           // 14 (乗算が優先)
let b = (2 + 3) * 4;         // 20 (括弧で優先順位を変更)

// 浮動小数点演算
let c = 3.14 * 2.0;          // f64 の演算

// 比較と論理演算 (bool 型)
let x = 10;
let d = x > 5 && x < 20;     // true

// 型キャスト
let e = 42 as i64;
let f = 3.14 as i32;         // 3

// 構造体とフィールドアクセス
let p = Point { x: 1, y: 2 };
let sum = p.x + p.y;

// タプルとインデックスアクセス
let t = (10, 20);
let total = t.0 + t.1;

// ブロック式
let g = {
    let tmp = 10;
    tmp * 2
};
```

## 制約・制限

- ゼロ除算 (`/` および `%` の整数型) はランタイムエラーとなる
- 整数オーバーフローの動作は未定義
- ビット演算子 (`&`, `|`, `^`, `<<`, `>>`) はサポートしない
- `%` (剰余) は整数型のみサポート (浮動小数点型では使用不可)
- 異なる型同士の算術演算は `as` によるキャストが必要

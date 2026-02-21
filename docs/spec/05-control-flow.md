# 05 - 制御フロー

## 概要

Cz の制御フロー構文として `if`/`else` と `while` を提供する。`if`/`else` は式として値を返し、`while` は MS1 では文として扱う。

## 構文

```bnf
if_expression    = "if" expression block_expression ["else" (block_expression | if_expression)]
while_statement  = "while" expression block_expression
block_expression = "{" statement* [expression] "}"
```

条件式を囲む括弧 `()` は不要である (Rust スタイル)。

## 意味論

### if 式

- `if` は式であり、値を返すことができる
- 条件式は `i32` 型で評価され、`0` は偽、それ以外は真として扱う
- `else` 付きの `if` を値として使う場合、両分岐の型が一致しなければならない
- `else` なしの `if` は MS1 では値として使用できない (文としてのみ使用可)
  - 将来のマイルストーンで `Optional<T>` を導入し、`else` なし `if` 式が `Optional<T>` を返すようにする予定

### else if チェーン

`else if` は `else` に続く `if` 式として自然に表現される:

```cz
if cond1 {
    expr1
} else if cond2 {
    expr2
} else {
    expr3
}
```

### while 文

- `while` は MS1 では文であり、値を返さない
- 条件式は `i32` 型で評価され、`0` は偽、それ以外は真として扱う
- 条件が真である間、ブロックを繰り返し実行する
- 将来のマイルストーンで unit 型 (`()`) を導入した後、`while` を式化する予定

### break / continue

- MS1 では `break` および `continue` はサポートしない
- 将来のマイルストーンで導入予定

## 例

### if 式を値として使用

```cz
fn max(a: i32, b: i32) -> i32 {
    if a > b { a } else { b }
}
```

### if 式を文として使用

```cz
fn check(x: i32) -> i32 {
    if x < 0 {
        print_i32(-1);
    }
    x
}
```

### else if チェーン

```cz
fn classify(x: i32) -> i32 {
    if x > 0 {
        1
    } else if x < 0 {
        -1
    } else {
        0
    }
}
```

### while ループ

```cz
fn sum_to(n: i32) -> i32 {
    let mut acc: i32 = 0;
    let mut i: i32 = 1;
    while i <= n {
        acc = acc + i;
        i = i + 1;
    }
    acc
}
```

### 早期リターンとの組み合わせ

```cz
fn factorial(n: i32) -> i32 {
    if n <= 1 {
        return 1;
    }
    n * factorial(n - 1)
}
```

## 制約・制限

- `else` なしの `if` は MS1 では値として使用できない
- `while` は MS1 では値を返さない
- `break` / `continue` は MS1 ではサポートしない
- `for` ループは MS1 ではサポートしない
- `match` (パターンマッチ) は MS1 ではサポートしない

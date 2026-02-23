# 05 - 制御フロー

## 概要

Cz の制御フロー構文として `if`/`else`、`while`、`match` を提供する。すべて式として値を返すことができる。

## 構文

```bnf
if_expression    = "if" expression block_expression ["else" (block_expression | if_expression)]
while_expression = [label ":"] "while" expression block_expression
match_expression = "match" expression "{" match_arm ("," match_arm)* [","] "}"
match_arm        = pattern "=>" expression
break_statement  = "break" [label] [expression] ";"
continue_statement = "continue" [label] ";"
block_expression = "{" statement* [expression] "}"
```

条件式を囲む括弧 `()` は不要である (Rust スタイル)。

## 意味論

### if 式

- `if` は式であり、値を返すことができる
- 条件式は `bool` 型でなければならない (MS1 では `i32` だったが、MS2 で `bool` 必須に変更)
- `else` 付きの `if` を値として使う場合、両分岐の型が一致しなければならない
- `else` なしの `if` は値として使用できない (文としてのみ使用可)

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

### while 式 (MS2)

- `while` は式であり、値を返すことができる
- 条件式は `bool` 型でなければならない
- `break value;` でループから値を返す — while 式の型は `break` で渡す値の型
- `break;` (値なし) は `()` を返す
- ループが `break` なしで正常終了した場合 (条件が `false` になった場合)、`()` を返す
- `break` で値を返す while 式の型が `()` 以外の場合、正常終了パスはコンパイルエラー (値を返すか `break` で終了する必要がある)

```cz
// () を返す while 式
let mut i = 0;
while i < 10 {
    i = i + 1;
}

// 値を返す while 式
let result = 'search: while i < 100 {
    if is_target(i) {
        break 'search i;
    }
    i = i + 1;
};  // break されなかった場合は到達しない
```

### ループラベル (MS2)

- `while` ループにラベルを付けることができる
- ラベルは `'識別子:` の形式で `while` の前に記述する
- `break` と `continue` でラベルを指定して、対象のループを制御できる

```cz
'outer: while cond1 {
    while cond2 {
        if done {
            break 'outer;    // 外側のループを抜ける
        }
        if skip {
            continue 'outer; // 外側のループの次のイテレーションへ
        }
    }
}
```

### break 文 (MS2)

- `break;` — 最も内側のループを抜ける (`()` を返す)
- `break value;` — 最も内側のループを抜け、値を返す
- `break 'label;` — 指定したラベルのループを抜ける (`()` を返す)
- `break 'label value;` — 指定したラベルのループを抜け、値を返す
- `break` はループ (`while`) の内部でのみ使用可能。ループ外で使用するとコンパイルエラー

### continue 文 (MS2)

- `continue;` — 最も内側のループの次のイテレーションへ
- `continue 'label;` — 指定したラベルのループの次のイテレーションへ
- `continue` はループ (`while`) の内部でのみ使用可能。ループ外で使用するとコンパイルエラー

### match 式 (MS2)

- `match` は式であり、パターンマッチの結果に応じた値を返す
- マッチ対象の式を評価し、各アームのパターンと上から順に照合する
- 最初にマッチしたアームの式を評価し、その値が match 式の値となる
- すべてのアームは同じ型の値を返さなければならない
- パターンは網羅的 (exhaustive) でなければならない (コンパイル時にチェック)
- 各アームは `パターン => 式` の形式で記述する
- アーム間はカンマで区切る (末尾カンマは許可)
- アームの式がブロック式の場合、カンマは省略可能

```cz
let x = 42;
let result = match x {
    0 => print_i32(0),
    1..=10 => print_i32(1),
    _ => print_i32(2),
};
```

パターンの詳細は [09-pattern-matching.md](./09-pattern-matching.md) を参照。

### 早期リターン

- `return` キーワードで関数の途中から値を返すことができる
- `return;` は `return ();` と等価 (ユニット型を返す関数用)

## 例

### if 式を値として使用

```cz
fn max(a: i32, b: i32) -> i32 {
    if a > b { a } else { b }
}
```

### if 式を文として使用

```cz
fn check(x: i32) {
    if x < 0 {
        print_i32(-1);
    }
}
```

### while 式

```cz
fn sum_to(n: i32) -> i32 {
    let mut acc = 0;
    let mut i = 1;
    while i <= n {
        acc = acc + i;
        i = i + 1;
    }
    acc
}
```

### break / continue

```cz
fn find_first(n: i32) -> i32 {
    let mut i = 0;
    let result = while i < n {
        if i * i > 100 {
            break i;
        }
        i = i + 1;
    };
    result
}
```

### ループラベル

```cz
fn search_2d() -> i32 {
    let mut i = 0;
    'outer: while i < 10 {
        let mut j = 0;
        while j < 10 {
            if i * 10 + j == 42 {
                break 'outer;
            }
            j = j + 1;
        }
        i = i + 1;
    }
    i
}
```

### match 式

```cz
fn classify(x: i32) -> i32 {
    match x {
        0 => 0,
        1..=9 => 1,
        10..=99 => 2,
        _ => 3,
    }
}
```

### match 式 (enum)

```cz
fn area(shape: Shape) -> f64 {
    match shape {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Rect { w, h } => w * h,
        Shape::None => 0.0,
    }
}
```

## 制約・制限

- `else` なしの `if` は値として使用できない
- `for` ループはサポートしない
- `break` / `continue` はループ内でのみ使用可能
- `match` のパターンは網羅的でなければならない
- `match` のアームで異なる型の値を返すことはできない

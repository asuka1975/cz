# 09 - パターンマッチ

## 概要

MS2 では `match` 式において使用するパターンマッチを提供する。パターンは値の構造を検査し、構成要素を変数に束縛する仕組みである。

## 構文

```bnf
pattern          = literal_pattern
                 | bool_pattern
                 | range_pattern
                 | tuple_pattern
                 | struct_pattern
                 | enum_pattern
                 | wildcard_pattern
                 | binding_pattern

literal_pattern  = ["-"] integer_literal
bool_pattern     = "true" | "false"
range_pattern    = ["-"] integer_literal "..=" ["-"] integer_literal
tuple_pattern    = "(" pattern ("," pattern)* ")"
struct_pattern   = identifier "{" [field_pattern ("," field_pattern)* [","]] "}"
field_pattern    = identifier [":" pattern]
enum_pattern     = identifier "::" identifier [enum_payload_pattern]
enum_payload_pattern = "(" [pattern ("," pattern)*] ")" | "{" [field_pattern ("," field_pattern)* [","]] "}"
wildcard_pattern = "_"
binding_pattern  = identifier
```

## パターンの種類

### リテラルパターン

整数リテラルに完全一致する。

```cz
match x {
    0 => print_i32(0),
    1 => print_i32(1),
    42 => print_i32(42),
    -1 => print_i32(-1),
    _ => print_i32(-999),
}
```

- 整数リテラルのみサポート (浮動小数点リテラルはパターンとして使用不可)
- 負の整数リテラルは `-数値` の形式で記述する
- 型サフィックス付きリテラルもサポート (例: `42i64`)

### bool パターン

`true` または `false` に一致する。

```cz
match flag {
    true => print_i32(1),
    false => print_i32(0),
}
```

### 範囲パターン (MS2)

`開始..=終了` の形式で整数の閉区間を指定する。

```cz
match x {
    1..=5 => print_i32(1),
    6..=10 => print_i32(2),
    _ => print_i32(0),
}
```

- 閉区間 (`..=`) のみサポート (半開区間 `..` はサポートしない)
- 開始値は終了値以下でなければならない (コンパイルエラー)
- 整数型のみサポート

### タプルパターン

タプルの各要素をパターンで分解する。

```cz
let pair = (1, true);
match pair {
    (0, _) => print_i32(0),
    (_, true) => print_i32(1),
    (_, false) => print_i32(2),
}
```

- 要素数がタプル型の要素数と一致しなければコンパイルエラー
- 各要素にはネストしたパターンを使用できる

### 構造体パターン

構造体のフィールドをパターンで分解する。

```cz
struct Point { x: i32, y: i32 }

match p {
    Point { x: 0, y: 0 } => print_i32(0),
    Point { x: 0, y } => print_i32(y),
    Point { x, y: 0 } => print_i32(x),
    Point { x, y } => print_i32(x + y),
}
```

- `フィールド名: パターン` でフィールドの値をパターンに照合する
- `フィールド名` のみの場合は `フィールド名: フィールド名` の省略形 (フィールドの値を同名の変数に束縛する)
- すべてのフィールドを列挙しなくてもよい — 省略されたフィールドはワイルドカードとして扱う
- 存在しないフィールド名を指定するとコンパイルエラー

### 列挙型パターン

列挙型のバリアントとその関連データをパターンで分解する。

```cz
enum Option {
    Some(i32),
    None,
}

match opt {
    Option::Some(v) => print_i32(v),
    Option::None => print_i32(0),
}
```

#### タプルバリアントの分解

```cz
enum Pair {
    Two(i32, i32),
}

match pair {
    Pair::Two(a, b) => print_i32(a + b),
}
```

#### 構造体バリアントの分解

```cz
enum Shape {
    Rect { w: f64, h: f64 },
    Circle(f64),
    None,
}

match shape {
    Shape::Rect { w, h } => print_f64(w * h),
    Shape::Circle(r) => print_f64(3.14 * r * r),
    Shape::None => print_f64(0.0),
}
```

#### ユニットバリアント

```cz
match opt {
    Option::None => print_i32(0),
    Option::Some(v) => print_i32(v),
}
```

### ワイルドカードパターン

`_` は任意の値に一致し、値を束縛しない。

```cz
match x {
    0 => print_i32(0),
    _ => print_i32(1),  // それ以外すべて
}
```

- ワイルドカードはどのパターン位置にも使用できる
- ネストしたパターン内でも使用できる

### 変数束縛パターン

識別子は任意の値に一致し、その値を変数に束縛する。

```cz
match x {
    0 => print_i32(0),
    n => print_i32(n),  // n に値が束縛される
}
```

- 束縛された変数はアームの式内で使用できる
- 変数束縛パターンはワイルドカードと同様にすべての値に一致する
- 束縛変数のスコープはそのアームの式の中に限定される

## 網羅性チェック

`match` 式のパターンは網羅的 (exhaustive) でなければならない。すべての可能な値がいずれかのアームのパターンに一致することをコンパイル時に検証する。

### 型ごとの網羅性ルール

#### `bool` 型

- `true` と `false` の両方をカバーするか、ワイルドカード/変数束縛を含む

```cz
// OK: 網羅的
match flag {
    true => 1,
    false => 0,
}

// OK: ワイルドカードで網羅
match flag {
    true => 1,
    _ => 0,
}

// ERROR: false がカバーされていない
match flag {
    true => 1,
}
```

#### 整数型

- 整数型は値の範囲が広いため、通常はワイルドカードまたは変数束縛パターンが必要
- リテラルパターンと範囲パターンの組み合わせですべての値をカバーできる場合はワイルドカード不要

```cz
// OK: ワイルドカードで網羅
match x {
    0 => 0,
    1..=10 => 1,
    _ => 2,
}

// ERROR: 網羅的でない (11 以上の値がカバーされていない)
match x {
    0 => 0,
    1..=10 => 1,
}
```

#### 列挙型

- すべてのバリアントをカバーするか、ワイルドカード/変数束縛を含む
- バリアントの関連データのパターンも再帰的にチェックされる

```cz
// OK: 全バリアントをカバー
match opt {
    Option::Some(v) => v,
    Option::None => 0,
}

// OK: ワイルドカードで網羅
match opt {
    Option::Some(v) => v,
    _ => 0,
}

// ERROR: None がカバーされていない
match opt {
    Option::Some(v) => v,
}
```

#### タプル型

- 各要素のパターンが網羅的であること

#### 構造体型

- ワイルドカードまたは変数束縛パターンで一致させるか、フィールドパターンが網羅的であること

### 到達不能パターンの警告

- 前のアームによってすでにカバーされているパターンは到達不能 (unreachable)
- 到達不能パターンはコンパイル警告を出す (エラーではない)

```cz
match x {
    _ => print_i32(0),
    0 => print_i32(1),  // 警告: 到達不能パターン
}
```

## パターンのネスト

パターンは自由にネストできる。

```cz
enum Option {
    Some(i32),
    None,
}

fn main() -> i32 {
    let pair = (Option::Some(42), true);

    match pair {
        (Option::Some(v), true) => print_i32(v),
        (Option::Some(v), false) => print_i32(-v),
        (Option::None, _) => print_i32(0),
    }

    0
}
```

## 例

### 整数のパターンマッチ

```cz
fn describe(x: i32) -> i32 {
    match x {
        0 => {
            print_i32(0);
            0
        }
        1..=9 => {
            print_i32(1);
            1
        }
        _ => {
            print_i32(2);
            2
        }
    }
}
```

### Option パターン

```cz
enum Option {
    Some(i32),
    None,
}

fn unwrap_or(opt: Option, default: i32) -> i32 {
    match opt {
        Option::Some(v) => v,
        Option::None => default,
    }
}
```

### 構造体の分解

```cz
struct Point { x: i32, y: i32 }

fn quadrant(p: Point) -> i32 {
    match p {
        Point { x: 0, y: 0 } => 0,
        Point { x, y } => {
            if x > 0 && y > 0 { 1 }
            else if x < 0 && y > 0 { 2 }
            else if x < 0 && y < 0 { 3 }
            else { 4 }
        }
    }
}
```

## 制約・制限

- パターンは `match` 式内でのみ使用可能 (`let` での分解はサポートしない)
- OR パターン (`|`) はサポートしない
- ガード条件 (`if` ガード) はサポートしない
- 浮動小数点リテラルはパターンとして使用不可
- 範囲パターンは整数型のみサポート
- 到達不能パターンは警告 (エラーではない)

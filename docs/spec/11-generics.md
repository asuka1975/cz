# 11 - ジェネリクスと型エイリアス

## 概要

MS3 ではジェネリクス (型パラメータ) と型エイリアスを導入する。ジェネリクスにより、関数・構造体・列挙型を型に対してパラメトリックに定義できる。型エイリアスにより既存の型に別名を付けられる。

ジェネリクスの実装には単相化 (monomorphization) を採用する。コンパイル時に具体的な型引数ごとにコードが生成されるため、実行時オーバーヘッドは発生しない。

## 構文

### ジェネリック型パラメータ

```bnf
type_params     = "<" type_param ("," type_param)* ">"
type_param      = identifier
```

### ジェネリック関数

```bnf
function_definition = "fn" identifier [type_params] "(" [parameter_list] ")" ["->" type] block_expression
```

### ジェネリック構造体

```bnf
struct_definition = "struct" identifier [type_params] "{" field_list "}"
```

### ジェネリック列挙型

```bnf
enum_definition = "enum" identifier [type_params] "{" variant_list "}"
```

### 型エイリアス

```bnf
type_alias = "type" identifier [type_params] "=" type ";"
```

### 型引数の指定

```bnf
generic_type     = identifier "<" type ("," type)* ">"
function_call    = identifier ["<" type ("," type)* ">"] "(" [argument_list] ")"
enum_construction = identifier "::" identifier ["<" type ("," type)* ">"] [variant_args]
```

### 型構文の拡張

```bnf
type = prim_type | unit_type | tuple_type | named_type | generic_type
```

## 意味論

### ジェネリック関数

- 関数名の直後に `<T>` または `<T, U, ...>` で型パラメータを宣言する
- 型パラメータは関数の引数型、戻り値型、関数本体内の型として使用できる
- 呼び出し時に型引数を明示的に指定できる: `identity<i32>(42)`
- 引数の型から型パラメータが一意に決定できる場合、型引数は省略できる (型推論)

```cz
fn identity<T>(x: T) -> T {
    x
}

// 明示的な型引数
let a = identity<i32>(42);

// 型推論 (引数 42 は i32 なので T = i32 と推論)
let b = identity(42);
```

### ジェネリック構造体

- 構造体名の直後に型パラメータを宣言する
- 型パラメータはフィールドの型として使用できる
- 構築時に型引数を明示的に指定できる
- フィールドの初期化式から型パラメータが推論できる場合、型引数は省略できる

```cz
struct Pair<A, B> {
    first: A,
    second: B,
}

// 明示的な型引数
let p1 = Pair<i32, bool> { first: 42, second: true };

// 型推論 (フィールドの値から A = i32, B = bool と推論)
let p2 = Pair { first: 42, second: true };
```

### ジェネリック列挙型

- 列挙型名の直後に型パラメータを宣言する
- 型パラメータはバリアントの関連データの型として使用できる
- 構築時に型引数を明示的に指定できる
- バリアントの値から型パラメータが推論できない場合 (ユニットバリアントなど)、型引数は省略可能だが、後続の使用箇所から推論される必要がある
- 全ての型パラメータが確定しない場合はコンパイルエラー

```cz
enum Maybe<T> {
    Some(T),
    None,
}

// 明示的な型引数
let a = Maybe::Some<i32>(42);

// 型推論 (引数から T = i32)
let b = Maybe::Some(42);

// ユニットバリアント — 後続の使用から推論
let c = Maybe::None;
let d: Maybe<i32> = c;  // ここで T = i32 と確定

// 関数引数から推論
fn unwrap_or<T>(m: Maybe<T>, default: T) -> T {
    match m {
        Maybe::Some(v) => v,
        Maybe::None => default,
    }
}
let e = unwrap_or(Maybe::None, 42);  // T = i32 と確定
```

### 型エイリアス

- `type` キーワードで既存の型に別名を定義する
- トップレベルでのみ定義可能 (関数内では不可)
- ジェネリックな型エイリアスも定義可能
- 型エイリアスは透過的 (transparent) であり、元の型と完全に同一として扱われる
- 型エイリアスの前方参照をサポートする

```cz
type Int = i32;
type Point2D = Point<i32, i32>;
type MaybeInt = Maybe<i32>;

// ジェネリックな型エイリアス
type Pair<T> = (T, T);
```

### 型推論

MS3 の型推論は MS2 の前方推論に加え、ジェネリクスの型変数に対する単一化 (unification) を行う。

推論のルール:
1. 型引数が明示的に指定されている場合、それを使用する
2. 関数呼び出しでは、実引数の型から型パラメータを推論する
3. 構造体・列挙型の構築では、フィールド/バリアントの値の型から推論する
4. 変数宣言では、型注釈または後続の使用箇所から型変数を確定させる
5. コンパイル単位の終了時に未確定の型変数が残っている場合はコンパイルエラー

### 単相化 (Monomorphization)

- ジェネリック関数・構造体・列挙型は、使用される具体的な型引数の組み合わせごとにコードが生成される
- 例: `identity<i32>` と `identity<bool>` は内部的に別々の関数として生成される
- 使用されない型引数の組み合わせに対してはコードは生成されない
- 単相化はコンパイル時に完了し、ランタイムでの型パラメータ解決は行われない

### `<` の曖昧性解消

`identity<i32>(42)` のように関数名の直後に `<` が来る場合、以下の規則で比較演算子かジェネリクスの型引数開始かを判断する:

- 関数呼び出し文脈 (識別子の直後) かつ `<` の後に型名が続き、対応する `>` の後に `(` が続く場合、型引数として解釈する
- それ以外の場合は比較演算子 `<` として解釈する
- 構造体名・列挙型名の直後の `<` は常に型引数として解釈する

## 例

### ジェネリック関数

```cz
fn first<A, B>(pair: Pair<A, B>) -> A {
    pair.first
}

fn make_pair<T>(x: T, y: T) -> Pair<T, T> {
    Pair { first: x, second: y }
}

fn main() -> i32 {
    let p = make_pair(10, 20);
    print_i32(first(p));  // 10
    0
}
```

### ジェネリック列挙型

```cz
enum Either<L, R> {
    Left(L),
    Right(R),
}

fn get_right_or<R>(e: Either<i32, R>, default: R) -> R {
    match e {
        Either::Left(_) => default,
        Either::Right(v) => v,
    }
}
```

### 型エイリアス

```cz
type IntPair = Pair<i32, i32>;

fn sum_pair(p: IntPair) -> i32 {
    p.first + p.second
}

fn main() -> i32 {
    let p: IntPair = Pair { first: 1, second: 2 };
    print_i32(sum_pair(p));  // 3
    0
}
```

### ジェネリクスと型推論の組み合わせ

```cz
fn main() -> i32 {
    // 型推論: Maybe::None の型は後続で確定
    let mut x = Maybe::None;
    x = Maybe::Some(42);  // T = i32 と確定

    // 型注釈で確定
    let y: Maybe<bool> = Maybe::None;

    // 関数引数から確定
    let z = unwrap_or(Maybe::None, 100);  // T = i32

    0
}
```

## 制約・制限

- 型パラメータに対するトレイト境界は SM-A では未サポート (SM-C で導入)
- 型パラメータは常に大文字始まりを推奨するが、強制はしない
- ジェネリック関数の型引数は最大 8 個まで (実装上の制限)
- 再帰的なジェネリクスのインスタンス化は有限でなければならない (無限の単相化はコンパイルエラー)
- 型エイリアスの循環定義はコンパイルエラー
- 型エイリアスは関数内では定義できない

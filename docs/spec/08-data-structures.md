# 08 - データ構造

## 概要

MS2 では構造体 (`struct`)、列挙型 (`enum`)、タプルの 3 種類のデータ構造を提供する。

## 構造体 (struct)

### 構文

```bnf
struct_definition = "struct" identifier "{" field_list "}"
field_list        = field ("," field)* [","]
field             = identifier ":" type
```

### 意味論

- 構造体は名前付きフィールドの集合体である
- 各フィールドは名前と型を持つ
- フィールドの順序は定義順に固定される
- 末尾カンマは許可
- `impl` ブロック (メソッド定義) は MS2 ではサポートしない

### 構造体の構築

```bnf
struct_expression = identifier "{" [field_init ("," field_init)* [","]] "}"
field_init        = identifier ":" expression
```

- すべてのフィールドに値を指定しなければならない
- フィールドの指定順は定義順と異なっていてもよい
- 各フィールドの値の型はフィールドの型と一致しなければならない

### フィールドアクセス

```bnf
field_access = expression "." identifier
```

- `.` 演算子でフィールドにアクセスする
- 存在しないフィールド名を指定するとコンパイルエラー

### 例

```cz
struct Point {
    x: i32,
    y: i32,
}

struct Rect {
    origin: Point,
    width: i32,
    height: i32,
}

fn main() -> i32 {
    let p = Point { x: 10, y: 20 };
    print_i32(p.x);     // 10
    print_i32(p.y);     // 20

    // フィールド順は任意
    let p2 = Point { y: 5, x: 3 };

    // ネストした構造体
    let r = Rect {
        origin: Point { x: 0, y: 0 },
        width: 100,
        height: 50,
    };
    print_i32(r.origin.x);  // 0

    0
}
```

## 列挙型 (enum)

### 構文

```bnf
enum_definition = "enum" identifier "{" variant_list "}"
variant_list    = variant ("," variant)* [","]
variant         = identifier [variant_fields]
variant_fields  = "(" type_list ")" | "{" field_list "}"
type_list       = type ("," type)* [","]
```

### 意味論

- 列挙型は有限個のバリアントの直和型である
- 各バリアントは以下のいずれかの形式を取る:
  - **ユニットバリアント**: 関連データなし (例: `None`)
  - **タプルバリアント**: 位置ベースの関連データ (例: `Some(i32)`)
  - **構造体バリアント**: 名前付きの関連データ (例: `Rect { w: f64, h: f64 }`)
- 同一列挙型内で異なる形式のバリアントを混在させてよい
- 末尾カンマは許可

### 列挙型の構築

```bnf
enum_construction = identifier "::" identifier [variant_args]
variant_args      = "(" expression_list ")" | "{" field_init_list "}"
```

- 列挙型名とバリアント名を `::` で区切って指定する
- タプルバリアントは `EnumName::Variant(値1, 値2)` で構築
- 構造体バリアントは `EnumName::Variant { field: 値 }` で構築
- ユニットバリアントは `EnumName::Variant` で構築

### 列挙型のフィールドアクセス

- 列挙型の値に対する直接的なフィールドアクセスはできない
- 関連データにアクセスするには `match` 式によるパターンマッチを使用する

### 例

```cz
enum Option {
    Some(i32),
    None,
}

enum Shape {
    Circle(f64),
    Rect { w: f64, h: f64 },
    Point,
}

fn main() -> i32 {
    let opt = Option::Some(42);
    let none = Option::None;

    let circle = Shape::Circle(5.0);
    let rect = Shape::Rect { w: 10.0, h: 20.0 };
    let point = Shape::Point;

    // match でアクセス
    let val = match opt {
        Option::Some(v) => v,
        Option::None => 0,
    };
    print_i32(val);  // 42

    0
}
```

## タプル

### 構文

```bnf
tuple_expression = "(" expression "," expression ("," expression)* ")"
tuple_type       = "(" type "," type ("," type)* ")"
```

### 意味論

- タプルは異なる型の値を固定個数まとめた型である
- 要素数は 2 以上 (単一要素のタプルは存在しない)
- `(42)` は括弧付き式であり、タプルではない
- タプルの型は各要素の型の組で表現される (例: `(i32, bool)`)

### タプルの構築

```cz
let pair = (42, true);
let triple = (1, 2.0, false);
```

### タプルインデックスアクセス

```bnf
tuple_index = expression "." integer_literal
```

- `.0`, `.1`, `.2` ... で要素にアクセスする
- インデックスは 0 から始まる
- インデックスが要素数以上の場合はコンパイルエラー
- インデックスはリテラル整数でなければならない (変数や式は不可)

### ユニット型との関係

- `()` はユニット型であり、0 要素のタプルとも解釈できる
- ユニット値は `()` で構築する

### 例

```cz
fn swap(pair: (i32, i32)) -> (i32, i32) {
    (pair.1, pair.0)
}

fn main() -> i32 {
    let t = (10, 20);
    let swapped = swap(t);
    print_i32(swapped.0);  // 20
    print_i32(swapped.1);  // 10

    // 異なる型の要素
    let mixed = (42, true, 3.14);
    print_i32(mixed.0);
    print_bool(mixed.1);
    print_f64(mixed.2);

    0
}
```

## 構造体・列挙型の等価性

- 構造体・列挙型に対する `==`, `!=` 演算子はサポートしない
- 比較が必要な場合はフィールド単位またはパターンマッチで行う

## 制約・制限

- `impl` ブロック (メソッド定義) はサポートしない
- 構造体・列挙型のジェネリクスはサポートしない
- 構造体・列挙型の `==`, `!=` はサポートしない
- タプルの要素数は 2 以上
- 再帰的な構造体 (自身を直接フィールドに持つ) はサポートしない
- 列挙型のバリアント名はグローバルに一意である必要はない (列挙型名で修飾して使用するため)

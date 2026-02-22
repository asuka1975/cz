# 07 - プログラム構造

## 概要

Cz プログラムの全体構造を定義する。MS2 では関数定義に加え、構造体定義と列挙型定義をトップレベルに配置できる。

## 構文

```bnf
program     = top_level_item*
top_level_item = function_definition | struct_definition | enum_definition
```

プログラムはゼロ個以上のトップレベル要素から構成される。

## 意味論

### エントリポイント

- プログラムの実行は `main` 関数から開始される
- `main` 関数のシグネチャは `fn main() -> i32` でなければならない
- `main` 関数の戻り値はプログラムの終了コードとなる
- `main` 関数が存在しない場合、コンパイル (LLVM IR 生成) は成功するがリンクエラーとなる

### トップレベル要素

MS2 ではトップレベルに以下を配置できる:

- **関数定義** (`fn`): MS1 から引き続きサポート
- **構造体定義** (`struct`): MS2 で追加
- **列挙型定義** (`enum`): MS2 で追加

グローバル変数やトップレベルの式は許可しない。

### 定義の順序と前方参照

- 関数は定義順に関係なく相互参照可能 (前方参照をサポート)
- 構造体・列挙型も定義順に関係なく関数内で使用可能
- 同名の関数を複数定義した場合はコンパイルエラー
- 同名の構造体・列挙型を複数定義した場合はコンパイルエラー
- 関数名と構造体名・列挙型名の名前空間は分離される (同名可)

### ソースファイル

- ソースファイルの拡張子は `.cz`
- エンコーディングは UTF-8
- 単一ファイルのプログラムのみサポート (複数ファイル / モジュールは将来対応)

## 例

### 構造体を含むプログラム (MS2)

```cz
struct Point {
    x: i32,
    y: i32,
}

fn distance_sq(p: Point) -> i32 {
    p.x * p.x + p.y * p.y
}

fn main() -> i32 {
    let p = Point { x: 3, y: 4 };
    print_i32(distance_sq(p));
    0
}
```

### 列挙型を含むプログラム (MS2)

```cz
enum Shape {
    Circle(f64),
    Rect { w: f64, h: f64 },
    None,
}

fn area(shape: Shape) -> f64 {
    match shape {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Rect { w, h } => w * h,
        Shape::None => 0.0,
    }
}

fn main() -> i32 {
    let s = Shape::Circle(5.0);
    print_f64(area(s));
    0
}
```

### 相互再帰

```cz
fn is_even(n: i32) -> bool {
    if n == 0 {
        true
    } else {
        is_odd(n - 1)
    }
}

fn is_odd(n: i32) -> bool {
    if n == 0 {
        false
    } else {
        is_even(n - 1)
    }
}

fn main() -> i32 {
    print_bool(is_even(4));
    0
}
```

## 制約・制限

- 単一ファイルのプログラムのみサポート
- モジュールシステム / `import` は将来対応
- グローバル変数はサポートしない
- `main` 関数は必須であり、シグネチャは `fn main() -> i32` に固定
- `impl` ブロックはサポートしない (MS2)

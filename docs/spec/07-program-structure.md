# 07 - プログラム構造

## 概要

Cz プログラムの全体構造を定義する。MS4 では複数ファイルによるモジュール構成をサポートする。

## 構文

```bnf
program        = import_decl* top_level_item*
import_decl    = "import" import_path ["as" identifier] ";"
import_path    = identifier ("::" identifier)*
top_level_item = [visibility] (function_definition | struct_definition | enum_definition | type_alias)
visibility     = "pub"?
type_alias     = "type" identifier [type_params] "=" type ";"
```

プログラムはゼロ個以上の import 文と、ゼロ個以上のトップレベル要素から構成される。import 文はすべてのトップレベル定義より前に記述する。

## 意味論

### エントリポイント

- プログラムのエントリポイントは `app` モジュール (`src/main.cz`) 内の `main` 関数である
- `main` 関数のシグネチャは `fn main() -> i32` でなければならない
- `main` 関数の戻り値はプログラムの終了コードとなる
- `main` 関数が存在しない場合、コンパイル (LLVM IR 生成) は成功するがリンクエラーとなる
- `main` 関数に `pub` は不要

### トップレベル要素

MS4 ではトップレベルに以下を配置できる:

- **import 文** (`import`): MS4 で追加。すべての定義より前に記述する
- **関数定義** (`fn`): MS1 から引き続きサポート。`pub` 修飾子を付けられる (MS4)
- **構造体定義** (`struct`): MS2 で追加。`pub` 修飾子を付けられる (MS4)
- **列挙型定義** (`enum`): MS2 で追加。`pub` 修飾子を付けられる (MS4)
- **型エイリアス** (`type`): MS3 で追加。`pub` 修飾子を付けられる (MS4)

グローバル変数やトップレベルの式は許可しない。

### 定義の順序と前方参照

- 関数は定義順に関係なく相互参照可能 (前方参照をサポート)
- 構造体・列挙型も定義順に関係なく関数内で使用可能
- 同名の関数を複数定義した場合はコンパイルエラー
- 同名の構造体・列挙型を複数定義した場合はコンパイルエラー
- 関数名と構造体名・列挙型名の名前空間は分離される (同名可)

### ソースファイルとモジュール (MS4)

- ソースファイルの拡張子は `.cz`
- エンコーディングは UTF-8
- 1 ファイル = 1 モジュール。ファイルパスからモジュール名が決定される
- `src/main.cz` が `app` モジュール (エントリポイント) に対応する
- `src/foo.cz` は `app::foo` モジュールに対応する
- `src/foo/bar.cz` は `app::foo::bar` モジュールに対応する
- `base` 名前空間はコンパイラ内蔵の仮想モジュールである
- 詳細は [12-modules.md](./12-modules.md) を参照

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

- グローバル変数はサポートしない
- `main` 関数は `app` モジュール (`src/main.cz`) に必須であり、シグネチャは `fn main() -> i32` に固定
- `impl` ブロックはサポートしない (SM-A 時点)
- 型エイリアスは関数内では定義できない (MS3)
- import 文はすべてのトップレベル定義より前に記述しなければならない (MS4)
- モジュールシステムの詳細な制約は [12-modules.md](./12-modules.md) を参照

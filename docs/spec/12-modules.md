# 12 - モジュールシステム

## 概要

MS4 ではモジュールシステムを導入する。モジュールシステムにより、プログラムを複数ファイルに分割し、名前空間を管理できる。

基本モデルは **1 ファイル = 1 モジュール** である。各ファイルはファイルパスに基づくモジュール名を持ち、`import` 文で他モジュールのアイテムを参照する。`pub` キーワードでアイテムの可視性を制御する。

モジュール名前空間は 2 つのルートを持つ:

- `app` — ユーザープログラムのルート
- `base` — コンパイラ提供の標準ライブラリ

## 構文

### import 文

```bnf
import_decl = "import" import_path ["as" identifier] ";"
import_path = identifier ("::" identifier)*
```

- `import` 文はファイルの先頭、トップレベル定義より前に記述する
- `as` 節で別名を付けられる

### 可視性修飾子

```bnf
visibility = "pub"?
```

可視性修飾子はトップレベル要素の前に付与する（`top_level_item` の定義を参照）。各定義（`function_definition`, `struct_definition` 等）自体には可視性を含めない。

### 修飾パス

```bnf
qualified_path   = identifier ("::" identifier)*
```

修飾パスは以下の構文位置で使用できる:

- 関数呼び出し: `module::func(args)` または `module::func<type_args>(args)`
- 型名: `module::TypeName` または `module::TypeName<type_args>`
- 構造体式: `module::StructName { fields }` または `module::StructName<type_args> { fields }`
- 列挙型式: `module::EnumName::Variant(args)` または `EnumName::Variant(args)`

### プログラム構造の更新

```bnf
program        = import_decl* top_level_item*
top_level_item = [visibility] (function_definition | struct_definition | enum_definition | type_alias)
```

### 新規トークン

- キーワード: `import`, `pub`
- 記号: `::` (パス区切り)

## 意味論

### モジュール名の対応規則

ファイルパスからモジュール名が決定される。明示的な `module` 宣言は不要。

#### `app` ルート (ユーザーコード)

| ファイルパス | モジュール名 |
|---|---|
| `src/main.cz` | `app` |
| `src/math.cz` | `app::math` |
| `src/utils/string.cz` | `app::utils::string` |

- `src/` ディレクトリをユーザーコードのルートとする
- `src/main.cz` はプログラムのエントリポイントであり、`app` モジュールに対応する
- サブディレクトリは入れ子のモジュール名に対応する

#### `base` ルート (標準ライブラリ)

`base` はコンパイラが内蔵する仮想モジュール群である。ファイルシステム上の `.cz` ファイルには対応しない。

コンパイラが提供する `base` モジュールの詳細は [13-base-module.md](./13-base-module.md) を参照。

将来、FFI (Foreign Function Interface) の導入後、`base` の一部を実 `.cz` ファイルへ移行する計画がある。

### import の種類

#### モジュール import

モジュール全体を import する。import 後、モジュール名を接頭辞として修飾パスでアイテムにアクセスする。

```cz
import base::io;

fn main() -> i32 {
    io::print_i32(42);
    0
}
```

`import base::io;` により、`io` という名前が現在のモジュールのスコープに導入される。`io::print_i32` のように修飾パスでアイテムにアクセスする。

#### アイテム import

特定のアイテム (関数、構造体、列挙型、型エイリアス) を直接 import する。import 後、アイテム名を非修飾で使用できる。

```cz
import base::io::print_i32;

fn main() -> i32 {
    print_i32(42);
    0
}
```

`import base::io::print_i32;` により、`print_i32` という名前が現在のモジュールのスコープに導入される。

#### import の判定

import パスがモジュールを指すかアイテムを指すかは、コンパイラがパスの最後のセグメントを以下の優先順位で解決する:

1. パスの末尾がモジュール名に一致する場合 → モジュール import
2. パスの末尾の一つ手前までがモジュールに一致し、末尾がそのモジュール内のアイテムに一致する場合 → アイテム import
3. いずれにも一致しない場合 → コンパイルエラー

### as 別名

`as` キーワードで import した名前に別名を付けられる。

```cz
import base::io as stdio;
import app::math as m;

fn main() -> i32 {
    stdio::print_i32(m::add(1, 2));
    0
}
```

アイテム import にも `as` を使用できる:

```cz
import base::io::print_i32 as put_i32;

fn main() -> i32 {
    put_i32(42);
    0
}
```

### 可視性 (pub)

- `pub` なし (デフォルト): モジュール内のみ参照可能
- `pub` あり: 他モジュールから `import` 可能

```cz
// math.cz
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn helper(x: i32) -> i32 {  // モジュール外から参照不可
    x * 2
}
```

`pub` なしアイテムを他モジュールから import しようとした場合はコンパイルエラーとなる。

#### 構造体の pub

`pub struct` の場合、全フィールドが自動的に公開される。フィールド単位の可視性制御は将来対応。

```cz
// geometry.cz
pub struct Point {
    x: i32,    // pub struct なので外部からアクセス可能
    y: i32,
}
```

#### 列挙型の pub

`pub enum` の場合、全バリアントが自動的に公開される。

### 修飾パスの解決

修飾パス `a::b` は以下の順序で解決される:

1. `a` が import されたモジュール名の場合: `a` モジュール内のアイテム `b` として解決
2. `a` がスコープ内の列挙型名の場合: 列挙型 `a` のバリアント `b` として解決

3 セグメント以上の修飾パス `a::b::c` は以下のように解決される:

1. `a` が import されたモジュール名で、`b` がそのモジュール内の列挙型の場合: 列挙型 `a.b` のバリアント `c` として解決

修飾パスが上記のいずれにも該当しない場合はコンパイルエラーとなる。

### 名前衝突

以下の場合はコンパイルエラーとなる:

- 同名のモジュールまたはアイテムが複数 import される
- import された名前がモジュール内のローカル定義と衝突する

名前衝突は `as` 別名で回避する:

```cz
import app::foo::Point;
import app::bar::Point as BarPoint;
```

### 循環 import

モジュール A が モジュール B を import し、モジュール B がモジュール A を import するような循環依存はコンパイルエラーとなる。

### エントリポイント

プログラムのエントリポイントは `app` モジュール (`src/main.cz`) 内の `main` 関数である。シグネチャは従来通り `fn main() -> i32` に固定される。`main` 関数に `pub` は不要。

### コンパイルモデル

コンパイラはエントリファイル (`src/main.cz`) から `import` 文を辿り、依存するすべてのモジュールを発見・解析する。全モジュールは単一のコンパイル単位として処理される。

1. エントリファイルの `import` を解析し、依存モジュールを再帰的に発見する
2. 全モジュールを字句解析・構文解析して AST を生成する
3. 全モジュールの AST から統合的に型コンテキストを構築する (可視性を考慮)
4. 全モジュールを統合的に Lowering し、単一の HIR を生成する
5. 単相化・意味解析・コード生成を実行する

## 例

### 基本的なモジュール分割

```
src/
  main.cz
  math.cz
```

```cz
// src/math.cz
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn sub(a: i32, b: i32) -> i32 {
    a - b
}

fn internal_helper(x: i32) -> i32 {
    x * 2
}
```

```cz
// src/main.cz
import app::math;
import base::io;

fn main() -> i32 {
    let result = math::add(10, 20);
    io::print_i32(result);
    0
}
```

### アイテム import

```cz
// src/main.cz
import base::io::print_i32;
import app::math::add;

fn main() -> i32 {
    print_i32(add(10, 20));
    0
}
```

### 別名付き import

```cz
// src/main.cz
import base::io as stdio;
import app::math as m;

fn main() -> i32 {
    stdio::print_i32(m::add(10, 20));
    0
}
```

### 構造体の公開

```cz
// src/geometry.cz
pub struct Point {
    x: i32,
    y: i32,
}

pub fn distance_sq(p: Point) -> i32 {
    p.x * p.x + p.y * p.y
}
```

```cz
// src/main.cz
import app::geometry::Point;
import app::geometry::distance_sq;
import base::io::print_i32;

fn main() -> i32 {
    let p = Point { x: 3, y: 4 };
    print_i32(distance_sq(p));
    0
}
```

### 列挙型の公開とパターンマッチ

```cz
// src/shapes.cz
pub enum Shape {
    Circle(f64),
    Rect { w: f64, h: f64 },
}

pub fn area(s: Shape) -> f64 {
    match s {
        Shape::Circle(r) => 3.14 * r * r,
        Shape::Rect { w, h } => w * h,
    }
}
```

```cz
// src/main.cz
import app::shapes::Shape;
import app::shapes::area;
import base::io::print_f64;

fn main() -> i32 {
    let s = Shape::Circle(5.0);
    print_f64(area(s));
    0
}
```

### ジェネリック型の import

```cz
// src/main.cz
import base::option::Option;

fn unwrap_or_zero(x: Option<i32>) -> i32 {
    match x {
        Option::Some(v) => v,
        Option::None => 0,
    }
}

fn main() -> i32 {
    let a = Option::Some(42);
    let b: Option<i32> = Option::None;
    0
}
```

### モジュール修飾での列挙型バリアント

```cz
// src/main.cz
import app::shapes;

fn main() -> i32 {
    // shapes モジュールの Shape 列挙型のバリアント
    let s = shapes::Shape::Circle(5.0);
    0
}
```

## 制約・制限

- グループ import (`import base::io::{print_i32, print_bool}`) はサポートしない
- ワイルドカード import (`import base::io::*`) はサポートしない
- 相対 import (`super`, `self`) はサポートしない
- 再エクスポートはサポートしない
- 自動 prelude はサポートしない
- 循環 import はサポートしない (コンパイルエラー)
- `app` および `base` のルート名は固定であり、変更できない
- フィールド単位の可視性制御はサポートしない (pub struct は全フィールド公開)
- `module` 宣言はサポートしない (ファイルパスからモジュール名を推定)
- インクリメンタルコンパイルはサポートしない (全モジュールを毎回コンパイル)

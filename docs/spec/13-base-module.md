# 13 - base 標準ライブラリ

## 概要

`base` はコンパイラが提供する標準ライブラリである。`base` 名前空間のモジュールはコンパイラに内蔵されており、ファイルシステム上の `.cz` ファイルには対応しない (仮想モジュール)。

将来、FFI (Foreign Function Interface) の導入後、`base` の一部を実 `.cz` ファイルへ移行する計画がある。

## base::io

入出力関数を提供するモジュール。

### 関数一覧

| 関数 | シグネチャ | 説明 |
|---|---|---|
| `print_i8` | `pub fn print_i8(x: i8)` | `i8` 値を標準出力に出力 |
| `print_i16` | `pub fn print_i16(x: i16)` | `i16` 値を標準出力に出力 |
| `print_i32` | `pub fn print_i32(x: i32)` | `i32` 値を標準出力に出力 |
| `print_i64` | `pub fn print_i64(x: i64)` | `i64` 値を標準出力に出力 |
| `print_f32` | `pub fn print_f32(x: f32)` | `f32` 値を標準出力に出力 |
| `print_f64` | `pub fn print_f64(x: f64)` | `f64` 値を標準出力に出力 |
| `print_bool` | `pub fn print_bool(x: bool)` | `true` または `false` を標準出力に出力 |

- すべての print 関数は値の後に改行 (`\n`) を出力する
- すべての print 関数の戻り値型は `()` (ユニット型)
- `print_f32` / `print_f64` の出力フォーマットは実装定義 (小数点以下の桁数等)

これらは MS3 以前の組み込み関数 (`print_i32(x)` 等) と同一の機能である。MS4 以降は `base::io` 経由での使用が必須となる。

### 使用例

```cz
// モジュール import
import base::io;

fn main() -> i32 {
    io::print_i32(42);
    io::print_bool(true);
    0
}
```

```cz
// アイテム import
import base::io::print_i32;
import base::io::print_bool;

fn main() -> i32 {
    print_i32(42);
    print_bool(true);
    0
}
```

```cz
// 別名付き import
import base::io as stdio;

fn main() -> i32 {
    stdio::print_i32(42);
    0
}
```

## base::option

オプショナル値を表す列挙型を提供するモジュール。

### 定義

```cz
pub enum Option<T> {
    Some(T),
    None,
}
```

`Option<T>` は値が存在するかしないかを型安全に表現する。`Some(T)` は値の存在を、`None` は値の不在を表す。

### 使用例

```cz
import base::option::Option;

fn find_positive(x: i32) -> Option<i32> {
    if x > 0 {
        Option::Some(x)
    } else {
        Option::None
    }
}

fn unwrap_or(opt: Option<i32>, default: i32) -> i32 {
    match opt {
        Option::Some(v) => v,
        Option::None => default,
    }
}

fn main() -> i32 {
    let a = find_positive(42);
    let b = find_positive(-1);
    unwrap_or(a, 0) + unwrap_or(b, 0)
}
```

```cz
// モジュール import で修飾パスを使う場合
import base::option;

fn main() -> i32 {
    let x: option::Option<i32> = option::Option::Some(42);
    0
}
```

## base::result

成功値またはエラー値を表す列挙型を提供するモジュール。

### 定義

```cz
pub enum Result<T, E> {
    Ok(T),
    Err(E),
}
```

`Result<T, E>` は処理の成功 (`Ok(T)`) または失敗 (`Err(E)`) を型安全に表現する。

### 使用例

```cz
import base::result::Result;

fn divide(a: i32, b: i32) -> Result<i32, i32> {
    if b == 0 {
        Result::Err(-1)
    } else {
        Result::Ok(a / b)
    }
}

fn main() -> i32 {
    match divide(10, 3) {
        Result::Ok(v) => v,
        Result::Err(_) => -1,
    }
}
```

## モジュール一覧

| モジュール | 提供するアイテム | 説明 |
|---|---|---|
| `base::io` | `print_i8`, `print_i16`, `print_i32`, `print_i64`, `print_f32`, `print_f64`, `print_bool` | 入出力関数 |
| `base::option` | `Option<T>` | オプショナル値 |
| `base::result` | `Result<T, E>` | 成功/エラー値 |

## 将来の拡張

以下は将来のマイルストーンで `base` に追加される可能性がある:

- `base::string` — 文字列型
- `base::iter` — イテレータ
- `base::ops` — 演算子トレイト
- `base::convert` — 型変換トレイト
- `base::fmt` — フォーマッティング

これらは FFI 導入後、実 `.cz` ファイルとして提供される予定である。

## 制約・制限

- `base` モジュールは仮想モジュールであり、ユーザーが変更・追加することはできない
- `base` 内のアイテムの再エクスポートはサポートしない
- `base::option::Option` および `base::result::Result` にメソッド (`impl`) はない (将来対応)

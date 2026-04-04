# 06 - 関数

## 概要

Cz の関数は `fn` キーワードで定義する。関数はブロック末尾の式で暗黙的に値を返すか、`return` で早期リターンする。

## 構文

```bnf
function_definition = "fn" identifier [type_params] "(" [parameter_list] ")" ["->" type] block_expression
type_params         = "<" identifier ("," identifier)* ">"
parameter_list      = parameter ("," parameter)*
parameter           = identifier ":" type
function_call       = identifier ["<" type ("," type)* ">"] "(" [argument_list] ")"
argument_list       = expression ("," expression)*
return_statement    = "return" [expression] ";"
```

## 意味論

### 関数定義

- 関数は `fn` キーワードで定義する
- 引数は `名前: 型` の形式で宣言する (引数の型注釈は省略不可)
- 戻り値の型は `->` の後に記述する

### 戻り値型 (MS2 更新)

- `-> type` で戻り値の型を明示する
- `-> ()` を省略できる: 戻り値型の記述がない場合、ユニット型 `()` を返す関数として扱う
- `fn foo() { ... }` は `fn foo() -> () { ... }` と等価

```cz
// 明示的に i32 を返す
fn add(a: i32, b: i32) -> i32 {
    a + b
}

// ユニット型を返す (-> () を省略)
fn greet() {
    print_i32(42);
}

// 明示的にユニット型を返す
fn greet2() -> () {
    print_i32(42);
}
```

### 暗黙の戻り値 (末尾式)

- ブロック内の最後の要素がセミコロンなしの式であれば、その値が関数の戻り値となる
- `return` を書かずに値を返せるため、コードが簡潔になる
- ユニット型を返す関数では、ブロックの最後がセミコロン付き文であればよい

### 早期リターン

- `return expr;` で関数の途中から値を返すことができる
- `return;` は `return ();` と等価 (ユニット型を返す関数用)
- `return` 文はセミコロンで終端する

### 関数呼び出し

- 関数名の後に `()` で引数を渡す
- 引数の数と型が関数定義と一致しなければコンパイルエラー
- 再帰呼び出しをサポートする

### 組み込み関数 (MS4 で非推奨)

MS3 以前では以下の関数がグローバルスコープで使用可能であった:

| 関数 | 引数の型 | 説明 |
|---|---|---|
| `print_i8(x)` | `i8` | `i8` 値を標準出力に出力 (MS2) |
| `print_i16(x)` | `i16` | `i16` 値を標準出力に出力 (MS2) |
| `print_i32(x)` | `i32` | `i32` 値を標準出力に出力 |
| `print_i64(x)` | `i64` | `i64` 値を標準出力に出力 (MS2) |
| `print_f32(x)` | `f32` | `f32` 値を標準出力に出力 (MS2) |
| `print_f64(x)` | `f64` | `f64` 値を標準出力に出力 (MS2) |
| `print_bool(x)` | `bool` | `true` または `false` を標準出力に出力 (MS2) |

**MS4 以降**: これらの関数は `base::io` モジュールに移設された。使用するには `import base::io;` または `import base::io::print_i32;` 等の import が必要。詳細は [13-base-module.md](./13-base-module.md) を参照。

- すべての print 関数は値の後に改行 (`\n`) を出力する
- すべての print 関数の戻り値型は `()` (MS2 で変更: MS1 では `i32` を返していた)
- `print_f32` / `print_f64` の出力フォーマットは実装定義 (小数点以下の桁数等)

### ジェネリック関数 (MS3)

- 関数名の直後に `<T>` または `<T, U, ...>` で型パラメータを宣言する
- 型パラメータは引数型、戻り値型、関数本体内の型として使用可能
- 呼び出し時に型引数を直接指定できる: `identity<i32>(42)`
- 引数の型から推論可能な場合、型引数は省略可能: `identity(42)`
- 詳細は [11-generics.md](./11-generics.md) を参照

```cz
fn identity<T>(x: T) -> T {
    x
}

let a = identity<i32>(42);  // 明示的
let b = identity(42);       // 型推論
```

### 関数の前方参照

- 関数は定義順に関係なく他の関数から呼び出し可能とする (前方参照をサポート)

## 例

### 基本的な関数

```cz
fn add(a: i32, b: i32) -> i32 {
    a + b
}
```

### ユニット型を返す関数 (MS2)

```cz
fn print_result(x: i32) {
    print_i32(x);
}
```

### 早期リターン

```cz
fn abs(x: i32) -> i32 {
    if x < 0 {
        return -x;
    }
    x
}
```

### 再帰

```cz
fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}
```

### 各種 print 関数 (MS2)

```cz
fn main() -> i32 {
    print_i32(42);
    print_i64(1000000i64);
    print_f64(3.14);
    print_bool(true);
    0
}
```

## 制約・制限

- 引数の型注釈は省略不可
- クロージャ / ラムダ式はサポートしない
- 関数のオーバーロードはサポートしない
- 可変長引数はサポートしない
- 関数を値として扱うこと (関数ポインタ、高階関数) はサポートしない (SM-A 時点)
- ジェネリック関数の型パラメータに対するトレイト境界は SM-A ではサポートしない

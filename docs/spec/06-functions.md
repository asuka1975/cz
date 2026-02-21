# 06 - 関数

## 概要

Cz の関数は `fn` キーワードで定義する。関数はブロック末尾の式で暗黙的に値を返すか、`return` で早期リターンする。

## 構文

```bnf
function_definition = "fn" identifier "(" [parameter_list] ")" "->" type block_expression
parameter_list      = parameter ("," parameter)*
parameter           = identifier ":" type
function_call       = identifier "(" [argument_list] ")"
argument_list       = expression ("," expression)*
return_statement    = "return" expression ";"
```

## 意味論

### 関数定義

- 関数は `fn` キーワードで定義する
- 引数は `名前: 型` の形式で宣言する
- 戻り値の型は `->` の後に記述する (MS1 では必須)
- 関数の本体はブロック式である

### 暗黙の戻り値 (末尾式)

- ブロック内の最後の要素がセミコロンなしの式であれば、その値が関数の戻り値となる
- `return` を書かずに値を返せるため、コードが簡潔になる

### 早期リターン

- `return` キーワードで関数の途中から値を返すことができる
- `return` は常に値を伴う (MS1 では戻り値なしの関数は存在しない)
- `return` 文はセミコロンで終端する

### 関数呼び出し

- 関数名の後に `()` で引数を渡す
- 引数の数と型が関数定義と一致しなければコンパイルエラー
- 再帰呼び出しをサポートする

### 組み込み関数

MS1 では以下の組み込み関数を提供する:

- `print_i32(x: i32)`: 引数の `i32` 値を標準出力に出力する

### 関数の前方参照

- MS1 では、関数は定義順に関係なく他の関数から呼び出し可能とする (前方参照をサポート)

## 例

### 基本的な関数

```cz
fn add(a: i32, b: i32) -> i32 {
    a + b
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

### 組み込み関数の使用

```cz
fn main() -> i32 {
    print_i32(42);
    0
}
```

## 制約・制限

- MS1 ではすべての関数に戻り値型の注釈が必要
- 戻り値なし (将来の unit 型) の関数は MS1 ではサポートしない
- クロージャ / ラムダ式は MS1 ではサポートしない
- 関数のオーバーロードはサポートしない
- 可変長引数はサポートしない

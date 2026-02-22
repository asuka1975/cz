# 01 - 字句構造

## 概要

Cz のソースコードは UTF-8 でエンコードされたテキストファイルである。字句解析器 (lexer) はソースコードをトークン列に変換する。

## 構文

### キーワード

以下の識別子はキーワードとして予約されており、変数名や関数名として使用できない:

```
fn let mut return if else while true false
match break continue struct enum as
```

**MS2 で追加**: `match`, `break`, `continue`, `struct`, `enum`, `as`

### 型キーワード

以下はプリミティブ型を表すキーワードである:

```
i8 i16 i32 i64 f32 f64 bool
```

**MS2 で追加**: `i8`, `i16`, `i64`, `f32`, `f64`, `bool`

### 識別子

```bnf
identifier = (letter | '_') (letter | digit | '_')*
letter     = 'a'..'z' | 'A'..'Z'
digit      = '0'..'9'
```

- 識別子はアルファベットまたはアンダースコアで始まる
- 2 文字目以降はアルファベット、数字、アンダースコアが使用可能
- キーワード・型キーワードと同名の識別子は使用不可

### ラベル (MS2)

```bnf
label = "'" identifier
```

- ループラベルはシングルクォートで始まる識別子で表現する
- 例: `'outer`, `'loop1`
- ラベルは `while` ループの前に `'label:` の形式で付与する

### 整数リテラル

```bnf
integer_literal = digit+ [integer_suffix]
integer_suffix  = "i8" | "i16" | "i32" | "i64"
```

- 10 進整数のみサポート
- 先頭のゼロは許可する (例: `007`)
- 負の整数リテラルは存在しない (単項マイナス演算子と整数リテラルの組み合わせで表現)
- **MS2**: 型サフィックスを付けることで整数の型を明示できる (例: `42i64`)
- サフィックスなしの整数リテラルのデフォルト型は `i32`

### 浮動小数点リテラル (MS2)

```bnf
float_literal  = digit+ "." digit+ [float_suffix]
float_suffix   = "f32" | "f64"
```

- 小数点の前後に少なくとも 1 桁の数字が必要 (`.5` や `1.` は不可、`0.5` や `1.0` と書く)
- 型サフィックスを付けることで浮動小数点の型を明示できる (例: `3.14f32`)
- サフィックスなしの浮動小数点リテラルのデフォルト型は `f64`
- 指数表記 (`1.0e10`) はサポートしない

### 記号・演算子トークン

```
+  -  *  /  %
== != < > <= >=
&& || !
=
( ) { }
,  :  ;
->
.             (MS2: フィールドアクセス / タプルインデックス)
..=           (MS2: 範囲パターン)
=>            (MS2: match アーム)
```

**MS2 で追加**: `.`, `..=`, `=>`

### コメント

```bnf
line_comment = '//' (任意の文字)* (改行 | EOF)
```

- `//` から行末までが行コメント
- ブロックコメント (`/* ... */`) はサポートしない

### 空白

空白文字 (スペース、タブ、改行、キャリッジリターン) はトークンの区切りとして機能し、それ自体はトークンとして生成されない。

## 意味論

字句解析器は以下のトークン種別を生成する:

| トークン種別 | 例 |
|---|---|
| Keyword | `fn`, `let`, `mut`, `return`, `if`, `else`, `while`, `true`, `false`, `match`, `break`, `continue`, `struct`, `enum`, `as` |
| TypeKeyword | `i8`, `i16`, `i32`, `i64`, `f32`, `f64`, `bool` |
| Identifier | `foo`, `bar_baz`, `_x`, `main` |
| Label | `'outer`, `'loop1` |
| IntegerLiteral | `0`, `42`, `100i64` |
| FloatLiteral | `3.14`, `1.0f32` |
| Operator / Punctuation | `+`, `->`, `==`, `{`, `;`, `.`, `..=`, `=>` 等 |
| EOF | ファイル終端 |

## 例

```cz
// MS2 の新しいトークン例
struct Point { x: i32, y: i32 }

enum Color {
    Red,
    Rgb(i32, i32, i32),
}

fn main() -> i32 {
    let val = 42i64;
    let pi = 3.14f32;
    let flag: bool = true;
    let p = Point { x: 1, y: 2 };
    let t = (1, true);

    'outer: while true {
        break 'outer;
    }

    match val as i32 {
        1..=10 => print_i32(1),
        _ => print_i32(0),
    }

    p.x
}
```

## 制約・制限

- 10 進整数リテラルのみサポートする (16 進、8 進、2 進は将来対応)
- 文字列リテラルはサポートしない
- ブロックコメントはサポートしない
- 浮動小数点の指数表記はサポートしない

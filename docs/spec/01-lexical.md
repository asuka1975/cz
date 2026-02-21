# 01 - 字句構造

## 概要

Cz のソースコードは UTF-8 でエンコードされたテキストファイルである。字句解析器 (lexer) はソースコードをトークン列に変換する。

## 構文

### キーワード

以下の識別子はキーワードとして予約されており、変数名や関数名として使用できない:

```
fn let mut return if else while true false
```

### 識別子

```bnf
identifier = (letter | '_') (letter | digit | '_')*
letter     = 'a'..'z' | 'A'..'Z'
digit      = '0'..'9'
```

- 識別子はアルファベットまたはアンダースコアで始まる
- 2 文字目以降はアルファベット、数字、アンダースコアが使用可能
- キーワードと同名の識別子は使用不可

### 整数リテラル

```bnf
integer_literal = digit+
```

- 10 進整数のみサポート (MS1)
- 先頭のゼロは許可する (例: `007`)
- 負の整数リテラルは存在しない (単項マイナス演算子と整数リテラルの組み合わせで表現)

### 記号・演算子トークン

```
+  -  *  /  %
== != < > <= >=
&& || !
=
( ) { }
,  :  ;
->
```

### コメント

```bnf
line_comment = '//' (任意の文字)* (改行 | EOF)
```

- `//` から行末までが行コメント
- ブロックコメント (`/* ... */`) は MS1 ではサポートしない

### 空白

空白文字 (スペース、タブ、改行、キャリッジリターン) はトークンの区切りとして機能し、それ自体はトークンとして生成されない。

## 意味論

字句解析器は以下のトークン種別を生成する:

| トークン種別 | 例 |
|---|---|
| Keyword | `fn`, `let`, `mut`, `return`, `if`, `else`, `while`, `true`, `false` |
| Identifier | `foo`, `bar_baz`, `_x`, `main` |
| IntegerLiteral | `0`, `42`, `100` |
| Operator / Punctuation | `+`, `->`, `==`, `{`, `;` 等 |
| EOF | ファイル終端 |

## 例

```cz
// これはコメント
fn main() -> i32 {
    let x: i32 = 42;
    x
}
```

上記のソースコードは以下のトークン列に変換される:

```
Keyword(fn) Identifier(main) LParen RParen Arrow Keyword(i32)
LBrace Keyword(let) Identifier(x) Colon Keyword(i32) Assign
IntegerLiteral(42) Semicolon Identifier(x) RBrace EOF
```

## 制約・制限

- MS1 では 10 進整数リテラルのみサポートする (16 進、8 進、2 進は将来対応)
- 文字列リテラルは MS1 ではサポートしない
- ブロックコメントは MS1 ではサポートしない

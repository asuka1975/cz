# Rule: テストケースのフォーマット

## Priority: HIGH

テストケースの `.cz` ファイルには必ず以下のメタデータヘッダーをコメントとして含めること:

```cz
// TEST: <テスト名>
// EXPECT: <success | warning | compile-error | link-error | runtime-error>
// DESCRIPTION: <このテストが検証する内容の説明>
// EXPECTED_EXIT_CODE: <期待される終了コード>    ← EXPECT: success のときのみ必須
// EXPECTED_STDOUT: <期待される標準出力>          ← EXPECT: success のときのみ必須
```

## EXPECT の種類

- `success` - コンパイルと実行の両方が成功することを期待
- `warning` - コンパイルは成功するが警告が発生することを期待
- `compile-error` - コンパイル時にエラーが発生することを期待 (不正なコードのテスト)
- `link-error` - コンパイル (LLVM IR 生成) は成功するがリンク時にエラーが発生することを期待
- `runtime-error` - コンパイルは成功するがランタイムエラーが発生することを期待

## success テストの検証フィールド

`EXPECT: success` のテストケースには以下のフィールドを必ず記述すること:

- `EXPECTED_EXIT_CODE: <数値>` - プログラム実行後の終了コード（main() の戻り値）
- `EXPECTED_STDOUT: <テキスト>` - 期待される標準出力。改行は `\n` でエスケープする。出力がない場合は値を空にする

例:
```cz
// TEST: Hello World
// EXPECT: success
// DESCRIPTION: print_i32 で値を出力する
// EXPECTED_EXIT_CODE: 0
// EXPECTED_STDOUT: 42\n
```

## ディレクトリ構成

テストケースは検証対象のコンパイラフェーズに応じて適切なディレクトリに配置すること:

- `tests/lexer/` - 字句解析のテスト
- `tests/parser/` - 構文解析のテスト
- `tests/typesystem/` - 型システムのテスト
- `tests/codegen/` - コード生成のテスト
- `tests/integration/` - エンドツーエンドテスト

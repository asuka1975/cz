# Rule: テストケースのフォーマット

## Priority: HIGH

テストケースの `.cz` ファイルには必ず以下のメタデータヘッダーをコメントとして含めること:

```cz
// TEST: <テスト名>
// EXPECT: <success | warning | compile-error | link-error | runtime-error>
// DESCRIPTION: <このテストが検証する内容の説明>
```

## EXPECT の種類

- `success` - コンパイルと実行の両方が成功することを期待
- `warning` - コンパイルは成功するが警告が発生することを期待
- `compile-error` - コンパイル時にエラーが発生することを期待 (不正なコードのテスト)
- `link-error` - コンパイル (LLVM IR 生成) は成功するがリンク時にエラーが発生することを期待
- `runtime-error` - コンパイルは成功するがランタイムエラーが発生することを期待

## ディレクトリ構成

テストケースは検証対象のコンパイラフェーズに応じて適切なディレクトリに配置すること:

- `tests/lexer/` - 字句解析のテスト
- `tests/parser/` - 構文解析のテスト
- `tests/typesystem/` - 型システムのテスト
- `tests/codegen/` - コード生成のテスト
- `tests/integration/` - エンドツーエンドテスト

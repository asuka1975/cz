# Cz Compiler Development Guide

## Project Overview

Cz は C-like 言語のコンパイラプロジェクトである。Rust の安全性と C++ の柔軟性を兼ね備えることを目指す。コンパイラは Rust で実装する。

## Development Flow (厳守)

Cz コンパイラの開発は以下の **3フェーズ** を厳密に順守して進める。フェーズを飛ばしたり、順序を入れ替えてはならない。

### Phase 1: 仕様議論 (Specification Discussion)

- ユーザーと対話しながら言語仕様を議論・決定する
- 仕様が確定するまでコンパイラの実装コード (Rust コード) を一切書いてはならない
- 議論中に仮の構文例やコード例を示すことは許可されるが、それらを実装として commit してはならない

### Phase 2: 仕様文書化・テストケース作成 (Documentation & Test Cases)

- Phase 1 で確定した仕様を `docs/spec/` 配下に Markdown ドキュメントとして記述する
- 確定した仕様に基づき、GCC テストスイート形式のテストケースを `tests/` 配下に作成する
- テストケースは Cz のソースファイル (`.cz`) として記述する
- ドキュメントとテストケースの作成が完了し、ユーザーの承認を得るまで Phase 3 に進んではならない

### Phase 3: 実装 (Implementation)

- Phase 2 で作成したドキュメントとテストケースに基づいてコンパイラを実装する
- テストケースが全て通ることを実装の完了基準とする
- 仕様変更が必要な場合は Phase 1 に戻って議論する

## Directory Structure

```
<repository-root>/
├── CLAUDE.md                  # This file
├── docs/
│   └── spec/                  # Language specification documents
│       ├── 00-overview.md     # Language overview
│       ├── 01-lexical.md      # Lexical structure
│       ├── 02-types.md        # Type system
│       ├── ...                # Other spec documents
│       └── index.md           # Spec table of contents
├── tests/
│   ├── lexer/                 # Lexer test cases (.cz files)
│   ├── parser/                # Parser test cases (.cz files)
│   ├── typesystem/            # Type system test cases (.cz files)
│   ├── codegen/               # Code generation test cases (.cz files)
│   └── integration/           # End-to-end test cases (.cz files)
├── src/                       # Compiler source code (Rust)
└── Cargo.toml
```

## Test Case Format

テストケースの `.cz` ファイルには以下のメタデータをコメントで埋め込む:

```cz
// TEST: <テスト名>
// EXPECT: <success | warning | compile-error | link-error | runtime-error>
// DESCRIPTION: <このテストが検証する内容の説明>
```

- `EXPECT: success` - コンパイル・実行ともに成功することを期待
- `EXPECT: warning` - コンパイルは成功するが警告が発生することを期待
- `EXPECT: compile-error` - コンパイルエラーになることを期待
- `EXPECT: link-error` - コンパイル (LLVM IR 生成) は成功するがリンクエラーになることを期待
- `EXPECT: runtime-error` - コンパイルは成功するがランタイムエラーになることを期待

## Definition of Done

実装の完了判定には以下の **すべて** を満たすことを必須とする。
すべてのコマンドは **Docker コンテナ上で実行すること**（ホスト環境では LLVM 等の依存が揃わないため）。

```bash
# コンテナ実行例
docker run --rm -v $(pwd):/workspace -w /workspace -e LLVM_SYS_191_PREFIX=/usr/lib/llvm-19 czc-dev <command>
```

1. **Lint チェックが通ること** — `cargo fmt --check` および `cargo clippy -- -D warnings` がエラー・警告なしで通ること
2. **ユニットテストが通ること** — `cargo test` が全件パスすること
3. **統合テストが通ること** — `bash run_tests.sh` が全件パスすること

## Rules

- 実装コードを書く前に必ず仕様ドキュメントとテストケースが存在すること
- 仕様の変更は必ずドキュメントとテストケースの両方に反映すること
- コミットメッセージは日本語で記述すること

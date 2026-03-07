# 10 - コンパイラ内部アーキテクチャ

## 概要

本文書は Cz コンパイラの内部アーキテクチャを定義する。言語仕様 (00-09) は変更せず、コンパイラの実装構造を再設計する。

## コンパイルパイプライン

ソースコードは以下の 6 段階を経て実行可能バイナリに変換される。

```
Source
  │
  ▼
[Lexer] ─────────────── Token 列
  │
  ▼
[Parser] ────────────── AST (構文木、型情報なし)
  │
  ▼
[Lowering] ──────────── HIR (型付き中間表現、名前解決済み)
  │                      ├── 型推論
  │                      ├── 名前解決 (変数 → VarId, 関数 → FuncId)
  │                      └── フィールド名 → インデックス解決
  ▼
[Semantic Analysis] ─── HIR (検証済み)
  │                      ├── 可変性チェック
  │                      ├── 型互換性検証
  │                      ├── 制御フロー検証
  │                      └── パターン網羅性チェック
  ▼
[CodeGen] ───────────── LLVM IR → オブジェクトファイル
  │
  ▼
[Link] ──────────────── 実行可能バイナリ (clang)
```

各段階はエラーを統一的な `Diagnostics` に蓄積する。いずれかの段階でエラーが発生した場合、後続の段階は実行しない。

## Arena とノード ID

### Arena

全 AST ノードおよび HIR ノードは Arena アロケータで管理する。個別のヒープ確保 (`Box<T>`) は使用しない。

```rust
pub struct Arena<T> {
    nodes: Vec<T>,
}

impl<T> Arena<T> {
    pub fn alloc(&mut self, node: T) -> Id<T>;
    pub fn get(&self, id: Id<T>) -> &T;
    pub fn get_mut(&mut self, id: Id<T>) -> &mut T;
}
```

### Id<T>

ノードの参照は型安全な整数 ID で行う。

```rust
pub struct Id<T> {
    raw: u32,
    _marker: PhantomData<T>,
}
```

- `Id<T>` は `Copy`, `Clone`, `Eq`, `Hash` を実装する
- `u32` で最大 42 億ノードを扱える (十分)
- `PhantomData<T>` により `Id<Expr>` と `Id<Stmt>` を型レベルで区別する

### 利点

- ポインタアドレスに依存しない安定した ID
- メモリ局所性が高い (ノードが `Vec` 上で連続配置)
- 型情報の格納に `Vec<Option<Type>>` が使え、`HashMap` のルックアップが不要

## ソース位置情報 (Span)

全ノードにソース位置情報を付与する。

```rust
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,  // ソースファイル内のバイトオフセット (開始)
    pub end: u32,    // ソースファイル内のバイトオフセット (終了、排他)
}
```

- バイトオフセットからの行番号・列番号の変換は `Diagnostics` の表示時に行う
- 各 Token, AST ノード, HIR ノードに `Span` を持たせる
- サイズは 8 バイト

## AST (抽象構文木)

パーサーが生成する **型情報なし** の構文木。名前は文字列のまま。

### データ構造

```rust
pub type ExprId = Id<Expr>;
pub type StmtId = Id<Stmt>;

pub struct Program {
    pub functions: Vec<FunctionDef>,
    pub structs: Vec<StructDef>,
    pub enums: Vec<EnumDef>,
}

pub struct FunctionDef {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: Type,
    pub body: Block,
    pub span: Span,
}

pub struct Block {
    pub stmts: Vec<StmtId>,
    pub expr: Option<ExprId>,
}

pub enum Stmt {
    Let {
        name: String,
        mutable: bool,
        var_type: Option<Type>,
        init: ExprId,
        span: Span,
    },
    Return { value: Option<ExprId>, span: Span },
    Break { label: Option<String>, value: Option<ExprId>, span: Span },
    Continue { label: Option<String>, span: Span },
    Expr(ExprId),
}

pub enum Expr {
    IntegerLiteral { value: i64, suffix: Option<IntSuffix>, span: Span },
    FloatLiteral { value: f64, suffix: Option<FloatSuffix>, span: Span },
    BoolLiteral { value: bool, span: Span },
    UnitLiteral { span: Span },
    Identifier { name: String, span: Span },
    BinaryOp { op: BinOp, left: ExprId, right: ExprId, span: Span },
    UnaryOp { op: UnaryOp, operand: ExprId, span: Span },
    Cast { expr: ExprId, target_type: Type, span: Span },
    Call { name: String, args: Vec<ExprId>, span: Span },
    Assign { name: String, value: ExprId, span: Span },
    If { condition: ExprId, then_block: Block, else_block: Option<ElseClause>, span: Span },
    While { label: Option<String>, condition: ExprId, body: Block, span: Span },
    Match { expr: ExprId, arms: Vec<MatchArm>, span: Span },
    Block { block: Block, span: Span },
    FieldAccess { expr: ExprId, field: String, span: Span },
    TupleIndex { expr: ExprId, index: u32, span: Span },
    TupleExpr { elements: Vec<ExprId>, span: Span },
    StructExpr { name: String, fields: Vec<(String, ExprId)>, span: Span },
    EnumExpr { enum_name: String, variant: String, args: EnumArgs, span: Span },
}

pub enum EnumArgs {
    Unit,
    Tuple(Vec<ExprId>),
    Struct(Vec<(String, ExprId)>),
}

// ElseClause, MatchArm, Pattern 等は現行仕様から ExprId ベースに変換
```

### パーサーの責務

- Token 列を受け取り、AST を Arena に確保して返す
- 構文エラーを `Diagnostics` に報告する
- 型情報・名前解決は一切行わない

## HIR (型付き高レベル中間表現)

Lowering が AST から生成する **型付き・名前解決済み** の中間表現。

### ID 体系

```rust
pub type HirExprId = Id<HirExpr>;
pub type HirStmtId = Id<HirStmt>;

/// 変数を一意に識別する ID。Lowering でスコープ解決時に割り当てられる。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct VarId(pub u32);

/// 関数を一意に識別する ID。
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FuncId(pub u32);
```

### HIR ノード

各ノードに型 (`ty`) とソース位置 (`span`) が付随する。

```rust
pub struct HirExpr {
    pub kind: HirExprKind,
    pub ty: Type,
    pub span: Span,
}

pub enum HirExprKind {
    IntegerLiteral { value: i64 },
    FloatLiteral { value: f64 },
    BoolLiteral(bool),
    UnitLiteral,
    Var(VarId),                    // 名前解決済み
    BinaryOp { op: BinOp, left: HirExprId, right: HirExprId },
    UnaryOp { op: UnaryOp, operand: HirExprId },
    Cast { expr: HirExprId, target_type: Type },
    Call { func: FuncId, args: Vec<HirExprId> },  // 名前解決済み
    Assign { var: VarId, value: HirExprId },       // 名前解決済み
    If { condition: HirExprId, then_block: HirBlock, else_block: Option<HirElseClause> },
    While { label: Option<String>, condition: HirExprId, body: HirBlock },
    Match { expr: HirExprId, arms: Vec<HirMatchArm> },
    Block(HirBlock),
    FieldAccess { expr: HirExprId, struct_name: String, field_index: usize },  // インデックス解決済み
    TupleIndex { expr: HirExprId, index: u32 },
    TupleExpr(Vec<HirExprId>),
    StructExpr { name: String, fields: Vec<HirExprId> },   // フィールド順序で格納
    EnumExpr { enum_name: String, variant_index: usize, args: HirEnumArgs },
}

pub struct HirBlock {
    pub stmts: Vec<HirStmtId>,
    pub expr: Option<HirExprId>,
}

pub enum HirStmt {
    Let {
        var: VarId,
        mutable: bool,
        init: HirExprId,
        span: Span,
    },
    Return { value: Option<HirExprId>, span: Span },
    Break { label: Option<String>, value: Option<HirExprId>, span: Span },
    Continue { label: Option<String>, span: Span },
    Expr(HirExprId),
}
```

### VarInfo テーブル

Lowering は VarId → 変数情報のテーブルを構築する。

```rust
pub struct VarInfo {
    pub name: String,       // デバッグ・エラー表示用
    pub ty: Type,
    pub mutable: bool,
    pub span: Span,         // 定義箇所
}

pub struct FuncInfo {
    pub name: String,
    pub params: Vec<VarId>,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}
```

Semantic と CodeGen はこのテーブルを参照するだけで、スコープの走査は不要。

## Lowering (AST → HIR)

### 責務

1. **型推論**: 式の型を推論し、`HirExpr.ty` に格納する
2. **名前解決**: 変数参照を `VarId` に、関数呼び出しを `FuncId` に解決する
3. **フィールド解決**: 構造体フィールド名をインデックスに変換する
4. **バリアント解決**: 列挙型バリアント名をインデックスに変換する
5. **型定義の登録**: 構造体・列挙型・関数のシグネチャを TypeContext に登録する

### ScopeStack

Lowering は `ScopeStack<VarId>` を使用して変数のスコープを管理する。

```rust
pub struct ScopeStack<V> {
    scopes: Vec<HashMap<String, V>>,
}

impl<V: Copy> ScopeStack<V> {
    pub fn push(&mut self);
    pub fn pop(&mut self);
    pub fn define(&mut self, name: String, value: V);
    pub fn lookup(&self, name: &str) -> Option<V>;
}
```

Lowering 以外のパス (Semantic, CodeGen) はスコープ管理を行わない。

### エラー処理

名前解決や型推論に失敗した場合:
- `Diagnostics` にエラーを報告する
- 該当ノードの型を `Type::Error` に設定する (カスケードエラー防止)
- Lowering は可能な限り処理を継続し、複数のエラーを一度に報告する

## Semantic Analysis (意味解析)

### 責務

HIR を走査して以下を検証する。Lowering で名前解決・型解決は完了しているため、Semantic はスコープ管理を行わない。

1. **可変性チェック**: `let` (非 `mut`) 変数への再代入を検出する。`VarInfo.mutable` を参照するだけで判定可能
2. **型互換性検証**: 代入・関数引数・return 文・break 値等の型が一致することを検証する。`HirExpr.ty` を直接参照する
3. **制御フロー検証**: `break`/`continue` がループ内にあること、ラベルの整合性を検証する
4. **パターン網羅性チェック**: `match` 式の全パターンが対象の型を網羅していることを検証する
5. **未初期化変数検出**: 使用前に初期化されていない変数へのアクセスを検出する

### 入出力

- 入力: HIR + VarInfo テーブル + TypeContext
- 出力: エラーは `Diagnostics` に蓄積。HIR は変更しない (読み取り専用)

## CodeGen (コード生成)

### 責務

検証済み HIR から LLVM IR を生成する。

### 入出力

- 入力: HIR + VarInfo テーブル + TypeContext
- 出力: LLVM IR → オブジェクトファイル

### 設計

- `VarId` → `PointerValue` のマッピングテーブルを持つ (スコープスタック不要)
- `FuncId` → `FunctionValue` のマッピングテーブルを持つ
- `HirExpr.ty` から直接 LLVM 型を決定する (`TypeMap` の引き回し不要)
- 構造体は LLVM opaque struct type、列挙型は tagged union (i32 tag + payload) として表現する (現行と同様)

## 統一 Diagnostic

### データ構造

```rust
pub enum Severity {
    Error,
    Warning,
}

pub struct Diagnostic {
    pub severity: Severity,
    pub span: Span,
    pub message: String,
}

pub struct Diagnostics {
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn error(&mut self, span: Span, message: impl Into<String>);
    pub fn warning(&mut self, span: Span, message: impl Into<String>);
    pub fn has_errors(&self) -> bool;
    pub fn render(&self, source: &str) -> String;
}
```

### 表示フォーマット

```
エラー: 未定義の変数 'x'
  --> main.cz:5:10
   |
 5 |     let y = x + 1;
   |             ^
```

`render` メソッドがバイトオフセットから行番号・列番号を計算し、該当行のソースコードと下線付きの位置表示を生成する。

## モジュール構成

```
src/
├── main.rs                # CLI エントリポイント、パイプライン統括
├── arena.rs               # Arena<T>, Id<T>
├── diagnostics.rs         # Span, Diagnostic, Diagnostics
├── scope.rs               # ScopeStack<V>
├── syntax/
│   ├── mod.rs             # pub use
│   ├── token.rs           # Token 定義
│   ├── lexer.rs           # 字句解析 (Source → Token 列)
│   ├── ast.rs             # AST データ構造
│   └── parser.rs          # 構文解析 (Token 列 → AST)
├── hir/
│   ├── mod.rs             # HIR データ構造, VarId, FuncId, VarInfo, FuncInfo
│   ├── lower.rs           # AST → HIR 変換 (型推論 + 名前解決)
│   └── types.rs           # Type enum, TypeContext, 型ユーティリティ
├── analysis/
│   ├── mod.rs             # SemanticAnalyzer 本体 (ディスパッチ)
│   ├── mutability.rs      # 可変性チェック
│   ├── exhaustiveness.rs  # match 網羅性チェック
│   └── control_flow.rs    # break/continue/return 検証
└── codegen/
    ├── mod.rs             # CodeGen 本体、初期化
    ├── expr.rs            # 式のコード生成
    ├── stmt.rs            # 文のコード生成
    ├── pattern.rs         # パターンマッチのコード生成
    └── types.rs           # Cz Type → LLVM Type マッピング
```

## 制約・制限

- Arena は単一スレッドでの使用を前提とする (並列コンパイルは将来検討)
- `Id<T>` は生成元の Arena 以外では無効だが、実行時チェックは行わない (型安全性のみ)
- HIR は immutable とし、Semantic Analysis で変更しない
- Lowering で解決できないエラー (未定義変数等) は `Type::Error` を使用してカスケードを防止する

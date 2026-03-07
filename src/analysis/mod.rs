use crate::arena::Arena;
use crate::diagnostics::{Diagnostics, Span};
use crate::hir::types::{TypeContext, VariantInfo};
use crate::hir::*;
use crate::syntax::ast::{BinOp, EnumPatternArgs, Pattern, UnaryOp};

struct LoopContext {
    label: Option<String>,
}

pub struct SemanticAnalyzer<'a> {
    ctx: &'a TypeContext,
    hir_exprs: &'a Arena<HirExpr>,
    hir_stmts: &'a Arena<HirStmt>,
    vars: &'a [VarInfo],
    func_names: &'a [String],
    source: &'a str,
    errors: Vec<String>,
    current_return_type: Type,
    loop_stack: Vec<LoopContext>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(
        ctx: &'a TypeContext,
        hir_exprs: &'a Arena<HirExpr>,
        hir_stmts: &'a Arena<HirStmt>,
        vars: &'a [VarInfo],
        func_names: &'a [String],
        source: &'a str,
    ) -> Self {
        Self {
            ctx,
            hir_exprs,
            hir_stmts,
            vars,
            func_names,
            source,
            errors: Vec::new(),
            current_return_type: Type::I32,
            loop_stack: Vec::new(),
        }
    }

    pub fn analyze(&mut self, program: &HirProgram) -> Result<(), Vec<String>> {
        // Validate types used in struct fields and enum variant fields
        for struct_name in &program.struct_order {
            if let Some(struct_info) = self.ctx.structs.get(struct_name) {
                for (_, field_type) in &struct_info.fields {
                    self.validate_type(field_type, Span::dummy());
                }
            }
        }
        for enum_name in &program.enum_order {
            if let Some(enum_info) = self.ctx.enums.get(enum_name) {
                for (_, variant_info) in &enum_info.variants {
                    match variant_info {
                        VariantInfo::Tuple(types) => {
                            for t in types {
                                self.validate_type(t, Span::dummy());
                            }
                        }
                        VariantInfo::Struct(fields) => {
                            for (_, t) in fields {
                                self.validate_type(t, Span::dummy());
                            }
                        }
                        VariantInfo::Unit => {}
                    }
                }
            }
        }

        // Analyze function bodies
        for func in &program.functions {
            self.analyze_function(func);
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    // --- Error helpers ---

    fn error(&mut self, span: Span, message: String) {
        let line = Diagnostics::span_to_line(self.source, span);
        self.errors
            .push(format!("意味解析エラー: {}行目: {}", line, message));
    }

    fn error_no_line(&mut self, message: String) {
        self.errors.push(format!("意味解析エラー: {}", message));
    }

    // --- Type validation ---

    fn validate_type(&mut self, ty: &Type, span: Span) {
        match ty {
            Type::Error => {}
            Type::Named(name) => {
                if !self.ctx.structs.contains_key(name) && !self.ctx.enums.contains_key(name) {
                    self.error(span, format!("未定義の型 '{}'", name));
                }
            }
            Type::Tuple(types) => {
                for t in types {
                    self.validate_type(t, span);
                }
            }
            _ => {}
        }
    }

    // --- Type matching ---

    fn check_type_match(&mut self, expected: &Type, actual: &Type, span: Span, context: &str) {
        // Type::Error が含まれる場合はカスケードエラーを防止
        if *expected == Type::Error || *actual == Type::Error {
            return;
        }
        if expected != actual {
            self.error(
                span,
                format!(
                    "{}: {:?} が期待されましたが {:?} が見つかりました",
                    context, expected, actual
                ),
            );
        }
    }

    // --- Function analysis ---

    fn analyze_function(&mut self, func: &HirFunctionDef) {
        self.current_return_type = func.return_type.clone();

        // Validate param types
        for &param_var in &func.params {
            let var_info = &self.vars[param_var.0 as usize];
            self.validate_type(&var_info.ty, func.span);
        }

        let needs_value = func.return_type != Type::Unit;
        let block_type = self.analyze_block_inner(&func.body, needs_value);

        if func.return_type == Type::Unit {
            // Unit return type: ok
        } else if let Some(ty) = block_type {
            if !self.block_always_returns(&func.body) {
                self.check_type_match(&func.return_type, &ty, func.span, "関数の戻り値");
            }
        } else if !self.block_always_returns(&func.body) {
            self.error(
                func.span,
                format!(
                    "関数 '{}' は {:?} の戻り値が必要ですが、末尾式がありません",
                    func.name, func.return_type
                ),
            );
        }
    }

    // --- Block analysis ---

    fn analyze_block(&mut self, block: &HirBlock) -> Option<Type> {
        self.analyze_block_inner(block, false)
    }

    fn analyze_block_inner(&mut self, block: &HirBlock, needs_value: bool) -> Option<Type> {
        for &stmt_id in &block.stmts {
            self.analyze_stmt(stmt_id);
        }
        if let Some(expr_id) = block.expr {
            self.analyze_expr(expr_id);
            if needs_value {
                self.check_expr_has_value(expr_id);
            }
            let ty = self.hir_exprs.get(expr_id).ty.clone();
            Some(ty)
        } else {
            None
        }
    }

    // --- Always returns check ---

    fn block_always_returns(&self, block: &HirBlock) -> bool {
        for &stmt_id in &block.stmts {
            let stmt = self.hir_stmts.get(stmt_id);
            if let HirStmt::Return { .. } = stmt {
                return true;
            }
            if let HirStmt::Expr(expr_id) = stmt
                && self.expr_always_returns(*expr_id)
            {
                return true;
            }
        }
        false
    }

    fn expr_always_returns(&self, expr_id: HirExprId) -> bool {
        let expr = self.hir_exprs.get(expr_id);
        if let HirExprKind::If {
            then_block,
            else_block: Some(else_clause),
            ..
        } = &expr.kind
        {
            let then_returns = self.block_always_returns(then_block);
            let else_returns = match else_clause {
                HirElseClause::ElseBlock(b) => self.block_always_returns(b),
                HirElseClause::ElseIf(else_expr_id) => self.expr_always_returns(*else_expr_id),
            };
            then_returns && else_returns
        } else {
            false
        }
    }

    // --- Statement analysis ---

    fn analyze_stmt(&mut self, stmt_id: HirStmtId) {
        // Extract all data from the statement before making mutable calls.
        let (kind, span) = {
            let stmt = self.hir_stmts.get(stmt_id);
            match stmt {
                HirStmt::Let { var, init, span } => (
                    StmtData::Let {
                        var: *var,
                        init: *init,
                    },
                    *span,
                ),
                HirStmt::Return { value, span } => (StmtData::Return { value: *value }, *span),
                HirStmt::Break { label, value, span } => (
                    StmtData::Break {
                        label: label.clone(),
                        value: *value,
                    },
                    *span,
                ),
                HirStmt::Continue { label, span } => (
                    StmtData::Continue {
                        label: label.clone(),
                    },
                    *span,
                ),
                HirStmt::Expr(expr_id) => (StmtData::Expr(*expr_id), Span::dummy()),
            }
        };

        match kind {
            StmtData::Let { var, init } => {
                self.analyze_expr(init);
                self.check_expr_has_value(init);

                let var_info = &self.vars[var.0 as usize];
                let init_type = self.hir_exprs.get(init).ty.clone();
                let var_type = var_info.ty.clone();
                self.check_type_match(&var_type, &init_type, span, "let 初期化式");
            }
            StmtData::Return { value } => {
                if let Some(val_id) = value {
                    self.analyze_expr(val_id);
                    self.check_expr_has_value(val_id);
                    let ty = self.hir_exprs.get(val_id).ty.clone();
                    let ret_type = self.current_return_type.clone();
                    self.check_type_match(&ret_type, &ty, span, "return 文");
                } else {
                    let ret_type = self.current_return_type.clone();
                    if ret_type != Type::Unit {
                        self.error(
                            span,
                            format!(
                                "return 文に値がありませんが、関数の戻り値型は {:?} です",
                                ret_type
                            ),
                        );
                    }
                }
            }
            StmtData::Break { label, value } => {
                if self.loop_stack.is_empty() {
                    self.error(span, "break はループ内でのみ使用できます".to_string());
                } else if let Some(ref label_name) = label
                    && !self
                        .loop_stack
                        .iter()
                        .any(|l| l.label.as_deref() == Some(label_name.as_str()))
                {
                    self.error(span, format!("ラベル '{}' が見つかりません", label_name));
                }
                if let Some(val_id) = value {
                    self.analyze_expr(val_id);
                }
            }
            StmtData::Continue { label } => {
                if self.loop_stack.is_empty() {
                    self.error(span, "continue はループ内でのみ使用できます".to_string());
                } else if let Some(ref label_name) = label
                    && !self
                        .loop_stack
                        .iter()
                        .any(|l| l.label.as_deref() == Some(label_name.as_str()))
                {
                    self.error(span, format!("ラベル '{}' が見つかりません", label_name));
                }
            }
            StmtData::Expr(expr_id) => {
                self.analyze_expr(expr_id);
            }
        }
    }

    // --- Expression has value check ---

    fn check_expr_has_value(&mut self, expr_id: HirExprId) {
        // Extract data to avoid borrow issues.
        let data = {
            let expr = self.hir_exprs.get(expr_id);
            match &expr.kind {
                HirExprKind::If {
                    then_block,
                    else_block,
                    ..
                } => {
                    let then_has_tail = then_block.expr.is_some();
                    let then_stmts = then_block.stmts.clone();
                    let then_expr = then_block.expr;

                    match else_block {
                        None => ExprValueData::IfNoElse,
                        Some(ec) => match ec {
                            HirElseClause::ElseBlock(block) => ExprValueData::IfElseBlock {
                                then_has_tail,
                                then_block: BlockSnapshot {
                                    stmts: then_stmts,
                                    expr: then_expr,
                                },
                                else_has_tail: block.expr.is_some(),
                                else_block: BlockSnapshot {
                                    stmts: block.stmts.clone(),
                                    expr: block.expr,
                                },
                            },
                            HirElseClause::ElseIf(else_if_id) => ExprValueData::IfElseIf {
                                then_has_tail,
                                then_block: BlockSnapshot {
                                    stmts: then_stmts,
                                    expr: then_expr,
                                },
                                else_if_id: *else_if_id,
                            },
                        },
                    }
                }
                HirExprKind::Block(block) => ExprValueData::Block(BlockSnapshot {
                    stmts: block.stmts.clone(),
                    expr: block.expr,
                }),
                _ => ExprValueData::Other,
            }
        };

        match data {
            ExprValueData::IfNoElse => {
                self.error_no_line("if 式を値として使用するには else 分岐が必要です".to_string());
            }
            ExprValueData::IfElseBlock {
                then_has_tail,
                then_block,
                else_has_tail,
                else_block,
            } => {
                if !then_has_tail {
                    let block = then_block.to_hir_block();
                    if !self.block_always_returns(&block) {
                        self.error_no_line("if 式の then 分岐に値がありません".to_string());
                    }
                }
                if !else_has_tail {
                    let block = else_block.to_hir_block();
                    if !self.block_always_returns(&block) {
                        self.error_no_line("if 式の else 分岐に値がありません".to_string());
                    }
                }
            }
            ExprValueData::IfElseIf {
                then_has_tail,
                then_block,
                else_if_id,
            } => {
                if !then_has_tail {
                    let block = then_block.to_hir_block();
                    if !self.block_always_returns(&block) {
                        self.error_no_line("if 式の then 分岐に値がありません".to_string());
                    }
                }
                self.check_expr_has_value(else_if_id);
            }
            ExprValueData::Block(snap) => {
                if snap.expr.is_none() {
                    let block = snap.to_hir_block();
                    if !self.block_always_returns(&block) {
                        self.error_no_line("ブロック式に値がありません".to_string());
                    }
                }
            }
            ExprValueData::Other => {}
        }
    }

    // --- Expression analysis ---

    fn analyze_expr(&mut self, expr_id: HirExprId) {
        // Extract all data needed from the expression to avoid borrow conflicts.
        let data = {
            let expr = self.hir_exprs.get(expr_id);
            ExprData::from_hir(expr)
        };

        match data {
            ExprData::Literal => {}
            ExprData::Var => {}
            ExprData::BinaryOp { op, left, right } => {
                self.analyze_expr(left);
                self.analyze_expr(right);

                let left_type = self.hir_exprs.get(left).ty.clone();
                let right_type = self.hir_exprs.get(right).ty.clone();

                if left_type == Type::Error || right_type == Type::Error {
                    return;
                }

                match op {
                    BinOp::And | BinOp::Or => {
                        if left_type != Type::Bool {
                            self.error_no_line(format!(
                                "論理演算子の左辺には bool 型が必要ですが {:?} が見つかりました",
                                left_type
                            ));
                        }
                        if right_type != Type::Bool {
                            self.error_no_line(format!(
                                "論理演算子の右辺には bool 型が必要ですが {:?} が見つかりました",
                                right_type
                            ));
                        }
                    }
                    BinOp::Mod => {
                        if !left_type.is_integer() {
                            self.error_no_line(format!(
                                "剰余演算子は整数型にのみ使用できますが {:?} が見つかりました",
                                left_type
                            ));
                        } else if left_type != right_type {
                            self.error_no_line(format!(
                                "算術演算の両辺は同じ型でなければなりません: {:?} と {:?}",
                                left_type, right_type
                            ));
                        }
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if !left_type.is_numeric() {
                            self.error_no_line(format!(
                                "算術演算子には数値型が必要ですが {:?} が見つかりました",
                                left_type
                            ));
                        } else if left_type != right_type {
                            self.error_no_line(format!(
                                "算術演算の両辺は同じ型でなければなりません: {:?} と {:?}",
                                left_type, right_type
                            ));
                        }
                    }
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq => {
                        if left_type != right_type {
                            self.error_no_line(format!(
                                "比較演算の両辺は同じ型でなければなりません: {:?} と {:?}",
                                left_type, right_type
                            ));
                        }
                    }
                }
            }
            ExprData::UnaryOp { op, operand } => {
                self.analyze_expr(operand);
                let ty = self.hir_exprs.get(operand).ty.clone();

                if ty == Type::Error {
                    return;
                }

                match op {
                    UnaryOp::Neg => {
                        if !ty.is_numeric() {
                            self.error_no_line(format!(
                                "単項マイナスには数値型が必要ですが {:?} が見つかりました",
                                ty
                            ));
                        }
                    }
                    UnaryOp::Not => {
                        if ty != Type::Bool {
                            self.error_no_line(format!(
                                "論理否定には bool 型が必要ですが {:?} が見つかりました",
                                ty
                            ));
                        }
                    }
                }
            }
            ExprData::Cast {
                expr: inner,
                target_type,
                ..
            } => {
                self.analyze_expr(inner);
                let source_type = self.hir_exprs.get(inner).ty.clone();

                if source_type == Type::Error {
                    return;
                }

                if source_type == Type::Unit || target_type == Type::Unit {
                    self.error_no_line(
                        "() 型へのキャストまたは () 型からのキャストはできません".to_string(),
                    );
                } else if !(source_type.is_numeric() || source_type == Type::Bool)
                    || !(target_type.is_numeric() || target_type == Type::Bool)
                {
                    self.error_no_line(format!(
                        "{:?} から {:?} へのキャストはできません",
                        source_type, target_type
                    ));
                }
            }
            ExprData::Call { func, args, span } => {
                for &arg_id in &args {
                    self.analyze_expr(arg_id);
                    self.check_expr_has_value(arg_id);
                }

                let func_name = self.func_names.get(func.0 as usize).cloned();
                if let Some(func_info) = func_name
                    .as_ref()
                    .and_then(|name| self.ctx.functions.get(name))
                {
                    let func_info = func_info.clone();
                    let name = func_name.unwrap();
                    if args.len() != func_info.param_count {
                        self.error(
                            span,
                            format!(
                                "関数 '{}' は {} 個の引数を取りますが {} 個渡されました",
                                name,
                                func_info.param_count,
                                args.len()
                            ),
                        );
                    } else {
                        for (i, &arg_id) in args.iter().enumerate() {
                            let arg_type = self.hir_exprs.get(arg_id).ty.clone();
                            if arg_type == Type::Error {
                                continue;
                            }
                            if arg_type != func_info.param_types[i] {
                                self.error(
                                    span,
                                    format!(
                                        "関数 '{}' の第 {} 引数: {:?} が期待されましたが {:?} が渡されました",
                                        name,
                                        i + 1,
                                        func_info.param_types[i],
                                        arg_type
                                    ),
                                );
                            }
                        }
                    }
                }
            }
            ExprData::Assign { var, value, span } => {
                self.analyze_expr(value);
                self.check_expr_has_value(value);

                let var_info = &self.vars[var.0 as usize];
                let var_name = var_info.name.clone();
                let var_mutable = var_info.mutable;
                let var_type = var_info.ty.clone();

                if !var_mutable {
                    self.error(
                        span,
                        format!("イミュータブル変数 '{}' への再代入はできません", var_name),
                    );
                }
                let val_type = self.hir_exprs.get(value).ty.clone();
                self.check_type_match(&var_type, &val_type, span, "代入");
            }
            ExprData::If {
                condition,
                then_block,
                else_block,
            } => {
                self.analyze_expr(condition);
                let cond_type = self.hir_exprs.get(condition).ty.clone();

                if cond_type != Type::Error && cond_type != Type::Bool {
                    self.error_no_line(format!(
                        "if 条件には bool 型が必要ですが {:?} が見つかりました",
                        cond_type
                    ));
                }

                let block = then_block.to_hir_block();
                self.analyze_block(&block);

                if let Some(else_clause) = else_block {
                    match else_clause {
                        ElseData::ElseBlock(snap) => {
                            let block = snap.to_hir_block();
                            self.analyze_block(&block);
                        }
                        ElseData::ElseIf(expr_id) => {
                            self.analyze_expr(expr_id);
                        }
                    }
                }
            }
            ExprData::While {
                label,
                condition,
                body,
            } => {
                self.analyze_expr(condition);
                let cond_type = self.hir_exprs.get(condition).ty.clone();

                if cond_type != Type::Error && cond_type != Type::Bool {
                    self.error_no_line(format!(
                        "while 条件には bool 型が必要ですが {:?} が見つかりました",
                        cond_type
                    ));
                }

                self.loop_stack.push(LoopContext { label });
                let block = body.to_hir_block();
                self.analyze_block(&block);
                self.loop_stack.pop();
            }
            ExprData::Match {
                expr: match_expr_id,
                arm_count,
            } => {
                self.analyze_expr(match_expr_id);
                let match_type = self.hir_exprs.get(match_expr_id).ty.clone();

                // Process each arm: analyze pattern then body.
                // Pattern does not implement Clone, so we use a free function
                // that collects errors into a separate Vec, then extend self.errors.
                let mut arm_types = Vec::new();
                for arm_idx in 0..arm_count {
                    // Analyze pattern using a free function to avoid borrow conflict.
                    // Collect errors separately, then push them after releasing
                    // the arena borrow.
                    let pattern_errors = {
                        let match_expr = self.hir_exprs.get(expr_id);
                        if let HirExprKind::Match { arms, .. } = &match_expr.kind {
                            let mut errs = Vec::new();
                            analyze_pattern(
                                &arms[arm_idx].pattern,
                                &match_type,
                                self.ctx,
                                &mut errs,
                            );
                            errs
                        } else {
                            Vec::new()
                        }
                    };
                    for msg in pattern_errors {
                        self.error_no_line(msg);
                    }

                    // Get body id and analyze it
                    let body_id = {
                        let match_expr = self.hir_exprs.get(expr_id);
                        if let HirExprKind::Match { arms, .. } = &match_expr.kind {
                            arms[arm_idx].body
                        } else {
                            unreachable!()
                        }
                    };
                    self.analyze_expr(body_id);
                    let arm_type = self.hir_exprs.get(body_id).ty.clone();
                    arm_types.push(arm_type);
                }

                // Check all arms have same type
                if arm_types.len() >= 2 {
                    let first = &arm_types[0];
                    if *first != Type::Error {
                        for (i, ty) in arm_types.iter().enumerate().skip(1) {
                            if *ty != Type::Error && ty != first {
                                self.error_no_line(format!(
                                    "match アームの型が一致しません: アーム0は {:?} ですがアーム{}は {:?} です",
                                    first, i, ty
                                ));
                            }
                        }
                    }
                }

                // Exhaustiveness check: access patterns from the arena
                self.check_exhaustiveness_from_arena(expr_id, &match_type);
            }
            ExprData::Block(snap) => {
                let block = snap.to_hir_block();
                self.analyze_block(&block);
            }
            ExprData::FieldAccess {
                expr: inner,
                struct_name,
            } => {
                self.analyze_expr(inner);
                let base_type = self.hir_exprs.get(inner).ty.clone();

                if base_type == Type::Error {
                    return;
                }

                match &base_type {
                    Type::Named(name) => {
                        if !self.ctx.structs.contains_key(name) && struct_name.is_empty() {
                            // Error already reported in lowering
                        } else if !self.ctx.structs.contains_key(name) {
                            self.error_no_line(format!(
                                "型 '{:?}' にフィールドアクセスはできません",
                                base_type
                            ));
                        }
                    }
                    _ => {
                        self.error_no_line(format!(
                            "型 '{:?}' にフィールドアクセスはできません",
                            base_type
                        ));
                    }
                }
            }
            ExprData::TupleIndex { expr: inner, index } => {
                self.analyze_expr(inner);
                let base_type = self.hir_exprs.get(inner).ty.clone();

                if base_type == Type::Error {
                    return;
                }

                if let Type::Tuple(types) = &base_type {
                    if (index as usize) >= types.len() {
                        self.error_no_line(format!(
                            "タプルのインデックス {} は範囲外です (要素数: {})",
                            index,
                            types.len()
                        ));
                    }
                } else {
                    self.error_no_line(format!(
                        "型 '{:?}' にタプルインデックスアクセスはできません",
                        base_type
                    ));
                }
            }
            ExprData::TupleExpr(elems) => {
                for &elem_id in &elems {
                    self.analyze_expr(elem_id);
                }
            }
            ExprData::StructExpr { name, fields, span } => {
                if let Some(struct_info) = self.ctx.structs.get(&name) {
                    let struct_info = struct_info.clone();

                    // Fields in HIR are already reordered to match definition order.
                    // Check count.
                    if fields.len() != struct_info.fields.len() {
                        for (i, (field_name, _)) in struct_info.fields.iter().enumerate() {
                            if i >= fields.len() {
                                self.error(
                                    span,
                                    format!(
                                        "構造体 '{}' のフィールド '{}' が指定されていません",
                                        name, field_name
                                    ),
                                );
                            }
                        }
                    }

                    // Type check each field
                    for (i, &field_id) in fields.iter().enumerate() {
                        self.analyze_expr(field_id);
                        if i < struct_info.fields.len() {
                            let actual_type = self.hir_exprs.get(field_id).ty.clone();
                            let (ref field_name, ref expected_type) = struct_info.fields[i];
                            self.check_type_match(
                                expected_type,
                                &actual_type,
                                span,
                                &format!("構造体フィールド '{}'", field_name),
                            );
                        }
                    }
                } else {
                    for &field_id in &fields {
                        self.analyze_expr(field_id);
                    }
                }
            }
            ExprData::EnumExpr {
                enum_name,
                variant_index,
                args,
                span,
            } => {
                if let Some(enum_info) = self.ctx.enums.get(&enum_name) {
                    let enum_info = enum_info.clone();
                    if variant_index < enum_info.variants.len() {
                        let (ref variant_name, ref variant_info) =
                            enum_info.variants[variant_index];
                        match (variant_info, &args) {
                            (VariantInfo::Unit, EnumArgsData::Unit) => {}
                            (VariantInfo::Tuple(expected_types), EnumArgsData::Tuple(exprs)) => {
                                if exprs.len() != expected_types.len() {
                                    self.error(
                                        span,
                                        format!(
                                            "バリアント '{}::{}' は {} 個の引数を取りますが {} 個渡されました",
                                            enum_name,
                                            variant_name,
                                            expected_types.len(),
                                            exprs.len()
                                        ),
                                    );
                                } else {
                                    for (i, &expr_id) in exprs.iter().enumerate() {
                                        self.analyze_expr(expr_id);
                                        let actual = self.hir_exprs.get(expr_id).ty.clone();
                                        self.check_type_match(
                                            &expected_types[i],
                                            &actual,
                                            span,
                                            &format!("enum バリアント引数 {}", i),
                                        );
                                    }
                                }
                            }
                            (VariantInfo::Struct(expected_fields), EnumArgsData::Struct(exprs)) => {
                                for (i, &expr_id) in exprs.iter().enumerate() {
                                    self.analyze_expr(expr_id);
                                    if i < expected_fields.len() {
                                        let actual = self.hir_exprs.get(expr_id).ty.clone();
                                        let (ref field_name, ref expected_type) =
                                            expected_fields[i];
                                        self.check_type_match(
                                            expected_type,
                                            &actual,
                                            span,
                                            &format!("enum フィールド '{}'", field_name),
                                        );
                                    }
                                }
                            }
                            _ => {
                                self.error(
                                    span,
                                    format!(
                                        "バリアント '{}::{}' の引数形式が不正です",
                                        enum_name, variant_name
                                    ),
                                );
                                self.analyze_enum_args_exprs(&args);
                            }
                        }
                    } else {
                        self.analyze_enum_args_exprs(&args);
                    }
                } else {
                    self.analyze_enum_args_exprs(&args);
                }
            }
        }
    }

    fn analyze_enum_args_exprs(&mut self, args: &EnumArgsData) {
        match args {
            EnumArgsData::Tuple(exprs) | EnumArgsData::Struct(exprs) => {
                for &e in exprs {
                    self.analyze_expr(e);
                }
            }
            EnumArgsData::Unit => {}
        }
    }

    // Pattern analysis is handled by the free function `analyze_pattern`
    // to avoid borrow conflicts when the pattern is stored in the arena.

    // --- Exhaustiveness ---

    /// Check exhaustiveness by accessing patterns directly from the arena.
    fn check_exhaustiveness_from_arena(&mut self, match_expr_id: HirExprId, match_type: &Type) {
        // First, collect the exhaustiveness-relevant information from patterns
        // without holding a mutable borrow.
        let pattern_info = {
            let expr = self.hir_exprs.get(match_expr_id);
            if let HirExprKind::Match { arms, .. } = &expr.kind {
                collect_exhaustiveness_info(arms)
            } else {
                return;
            }
        };

        // Check if there is a wildcard or binding that catches all
        if pattern_info.has_catch_all {
            return;
        }

        match match_type {
            Type::Bool => {
                if !pattern_info.has_bool_true || !pattern_info.has_bool_false {
                    self.error_no_line(
                        "match 式が網羅的ではありません: bool 型は true と false の両方が必要です"
                            .to_string(),
                    );
                }
            }
            Type::Named(name) => {
                if let Some(enum_info) = self.ctx.enums.get(name) {
                    let enum_info = enum_info.clone();
                    let all_variants: Vec<&str> =
                        enum_info.variants.iter().map(|(n, _)| n.as_str()).collect();
                    for v in &all_variants {
                        if !pattern_info.covered_enum_variants.iter().any(|cv| cv == v) {
                            self.error_no_line(format!(
                                "match 式が網羅的ではありません: バリアント '{}' がカバーされていません",
                                v
                            ));
                        }
                    }
                }
            }
            _ if match_type.is_integer() => {
                self.error_no_line(
                    "match 式が網羅的ではありません: 整数型にはワイルドカード '_' パターンが必要です"
                        .to_string(),
                );
            }
            _ => {}
        }
    }
}

// ============================================================
// Data extraction types to break arena borrows
// ============================================================

/// Information collected from match patterns for exhaustiveness checking.
struct ExhaustivenessInfo {
    has_catch_all: bool,
    has_bool_true: bool,
    has_bool_false: bool,
    covered_enum_variants: Vec<String>,
}

/// Collect exhaustiveness information from match arms without requiring Clone on Pattern.
fn collect_exhaustiveness_info(arms: &[HirMatchArm]) -> ExhaustivenessInfo {
    let mut info = ExhaustivenessInfo {
        has_catch_all: false,
        has_bool_true: false,
        has_bool_false: false,
        covered_enum_variants: Vec::new(),
    };

    for arm in arms {
        match &arm.pattern {
            Pattern::Wildcard | Pattern::Binding(_) => {
                info.has_catch_all = true;
            }
            Pattern::BoolLiteral(true) => {
                info.has_bool_true = true;
            }
            Pattern::BoolLiteral(false) => {
                info.has_bool_false = true;
            }
            Pattern::Enum { variant, .. } => {
                info.covered_enum_variants.push(variant.clone());
            }
            _ => {}
        }
    }

    info
}

/// Analyze a pattern against an expected type, collecting errors into the provided Vec.
/// This is a free function to avoid borrow conflicts when the pattern is stored in the arena.
fn analyze_pattern(
    pattern: &Pattern,
    expected_type: &Type,
    ctx: &TypeContext,
    errors: &mut Vec<String>,
) {
    match pattern {
        Pattern::IntLiteral(_) | Pattern::Range { .. } => {
            if !expected_type.is_integer() {
                errors.push(format!(
                    "整数パターンは整数型に対してのみ使用できます (型: {:?})",
                    expected_type
                ));
            }
        }
        Pattern::BoolLiteral(_) => {
            if *expected_type != Type::Bool {
                errors.push(format!(
                    "bool パターンは bool 型に対してのみ使用できます (型: {:?})",
                    expected_type
                ));
            }
        }
        Pattern::Wildcard | Pattern::Binding(_) => {}
        Pattern::Tuple(patterns) => {
            if let Type::Tuple(types) = expected_type {
                if patterns.len() != types.len() {
                    errors.push(format!(
                        "タプルパターンの要素数が一致しません: {} が期待されましたが {} です",
                        types.len(),
                        patterns.len()
                    ));
                } else {
                    for (pat, ty) in patterns.iter().zip(types.iter()) {
                        analyze_pattern(pat, ty, ctx, errors);
                    }
                }
            } else {
                errors.push(format!(
                    "タプルパターンはタプル型に対してのみ使用できます (型: {:?})",
                    expected_type
                ));
            }
        }
        Pattern::Struct { name, fields } => {
            if let Type::Named(type_name) = expected_type {
                if name != type_name {
                    errors.push(format!(
                        "構造体パターン '{}' は型 '{:?}' と一致しません",
                        name, expected_type
                    ));
                } else if let Some(struct_info) = ctx.structs.get(name) {
                    let struct_info = struct_info.clone();
                    for (field_name, field_pat) in fields {
                        if let Some((_, field_type)) =
                            struct_info.fields.iter().find(|(n, _)| n == field_name)
                        {
                            analyze_pattern(field_pat, field_type, ctx, errors);
                        } else {
                            errors.push(format!(
                                "構造体 '{}' にフィールド '{}' はありません",
                                name, field_name
                            ));
                        }
                    }
                }
            }
        }
        Pattern::Enum {
            enum_name,
            variant,
            args,
        } => {
            if let Type::Named(type_name) = expected_type {
                if enum_name != type_name {
                    errors.push(format!(
                        "enum パターン '{}' は型 '{:?}' と一致しません",
                        enum_name, expected_type
                    ));
                } else if let Some(enum_info) = ctx.enums.get(enum_name) {
                    let enum_info = enum_info.clone();
                    if let Some((_, variant_info)) =
                        enum_info.variants.iter().find(|(n, _)| n == variant)
                    {
                        match (variant_info, args) {
                            (VariantInfo::Unit, EnumPatternArgs::Unit) => {}
                            (VariantInfo::Tuple(types), EnumPatternArgs::Tuple(pats)) => {
                                if pats.len() != types.len() {
                                    errors.push(format!(
                                        "バリアント '{}::{}' のパターン引数数が一致しません",
                                        enum_name, variant
                                    ));
                                } else {
                                    for (pat, ty) in pats.iter().zip(types.iter()) {
                                        analyze_pattern(pat, ty, ctx, errors);
                                    }
                                }
                            }
                            (VariantInfo::Struct(fields), EnumPatternArgs::Struct(pat_fields)) => {
                                for (field_name, field_pat) in pat_fields {
                                    if let Some((_, field_type)) =
                                        fields.iter().find(|(n, _)| n == field_name)
                                    {
                                        analyze_pattern(field_pat, field_type, ctx, errors);
                                    }
                                }
                            }
                            _ => {
                                errors.push(format!(
                                    "バリアント '{}::{}' のパターン形式が不正です",
                                    enum_name, variant
                                ));
                            }
                        }
                    } else {
                        errors.push(format!(
                            "列挙型 '{}' にバリアント '{}' はありません",
                            enum_name, variant
                        ));
                    }
                }
            }
        }
    }
}

/// Snapshot of a block's structure (no arena references).
struct BlockSnapshot {
    stmts: Vec<HirStmtId>,
    expr: Option<HirExprId>,
}

impl BlockSnapshot {
    fn to_hir_block(&self) -> HirBlock {
        HirBlock {
            stmts: self.stmts.clone(),
            expr: self.expr,
        }
    }
}

/// Extracted statement data.
enum StmtData {
    Let {
        var: VarId,
        init: HirExprId,
    },
    Return {
        value: Option<HirExprId>,
    },
    Break {
        label: Option<String>,
        value: Option<HirExprId>,
    },
    Continue {
        label: Option<String>,
    },
    Expr(HirExprId),
}

/// Extracted expression data.
#[allow(dead_code)]
enum ExprData {
    Literal,
    Var,
    BinaryOp {
        op: BinOp,
        left: HirExprId,
        right: HirExprId,
    },
    UnaryOp {
        op: UnaryOp,
        operand: HirExprId,
    },
    Cast {
        expr: HirExprId,
        target_type: Type,
        span: Span,
    },
    Call {
        func: FuncId,
        args: Vec<HirExprId>,
        span: Span,
    },
    Assign {
        var: VarId,
        value: HirExprId,
        span: Span,
    },
    If {
        condition: HirExprId,
        then_block: BlockSnapshot,
        else_block: Option<ElseData>,
    },
    While {
        label: Option<String>,
        condition: HirExprId,
        body: BlockSnapshot,
    },
    Match {
        expr: HirExprId,
        arm_count: usize,
    },
    Block(BlockSnapshot),
    FieldAccess {
        expr: HirExprId,
        struct_name: String,
    },
    TupleIndex {
        expr: HirExprId,
        index: u32,
    },
    TupleExpr(Vec<HirExprId>),
    StructExpr {
        name: String,
        fields: Vec<HirExprId>,
        span: Span,
    },
    EnumExpr {
        enum_name: String,
        variant_index: usize,
        args: EnumArgsData,
        span: Span,
    },
}

enum ElseData {
    ElseBlock(BlockSnapshot),
    ElseIf(HirExprId),
}

enum EnumArgsData {
    Unit,
    Tuple(Vec<HirExprId>),
    Struct(Vec<HirExprId>),
}

/// Extracted check_expr_has_value data.
enum ExprValueData {
    IfNoElse,
    IfElseBlock {
        then_has_tail: bool,
        then_block: BlockSnapshot,
        else_has_tail: bool,
        else_block: BlockSnapshot,
    },
    IfElseIf {
        then_has_tail: bool,
        then_block: BlockSnapshot,
        else_if_id: HirExprId,
    },
    Block(BlockSnapshot),
    Other,
}

impl ExprData {
    fn from_hir(expr: &HirExpr) -> Self {
        let span = expr.span;
        match &expr.kind {
            HirExprKind::IntegerLiteral { .. }
            | HirExprKind::FloatLiteral { .. }
            | HirExprKind::BoolLiteral(_)
            | HirExprKind::UnitLiteral => ExprData::Literal,

            HirExprKind::Var(_) => ExprData::Var,

            HirExprKind::BinaryOp { op, left, right } => ExprData::BinaryOp {
                op: *op,
                left: *left,
                right: *right,
            },

            HirExprKind::UnaryOp { op, operand } => ExprData::UnaryOp {
                op: *op,
                operand: *operand,
            },

            HirExprKind::Cast { expr, target_type } => ExprData::Cast {
                expr: *expr,
                target_type: target_type.clone(),
                span,
            },

            HirExprKind::Call { func, args } => ExprData::Call {
                func: *func,
                args: args.clone(),
                span,
            },

            HirExprKind::Assign { var, value } => ExprData::Assign {
                var: *var,
                value: *value,
                span,
            },

            HirExprKind::If {
                condition,
                then_block,
                else_block,
            } => {
                let else_data = else_block.as_ref().map(|ec| match ec {
                    HirElseClause::ElseBlock(block) => ElseData::ElseBlock(BlockSnapshot {
                        stmts: block.stmts.clone(),
                        expr: block.expr,
                    }),
                    HirElseClause::ElseIf(id) => ElseData::ElseIf(*id),
                });
                ExprData::If {
                    condition: *condition,
                    then_block: BlockSnapshot {
                        stmts: then_block.stmts.clone(),
                        expr: then_block.expr,
                    },
                    else_block: else_data,
                }
            }

            HirExprKind::While {
                label,
                condition,
                body,
            } => ExprData::While {
                label: label.clone(),
                condition: *condition,
                body: BlockSnapshot {
                    stmts: body.stmts.clone(),
                    expr: body.expr,
                },
            },

            HirExprKind::Match { expr, arms } => ExprData::Match {
                expr: *expr,
                arm_count: arms.len(),
            },

            HirExprKind::Block(block) => ExprData::Block(BlockSnapshot {
                stmts: block.stmts.clone(),
                expr: block.expr,
            }),

            HirExprKind::FieldAccess {
                expr, struct_name, ..
            } => ExprData::FieldAccess {
                expr: *expr,
                struct_name: struct_name.clone(),
            },

            HirExprKind::TupleIndex { expr, index } => ExprData::TupleIndex {
                expr: *expr,
                index: *index,
            },

            HirExprKind::TupleExpr(elems) => ExprData::TupleExpr(elems.clone()),

            HirExprKind::StructExpr { name, fields } => ExprData::StructExpr {
                name: name.clone(),
                fields: fields.clone(),
                span,
            },

            HirExprKind::EnumExpr {
                enum_name,
                variant_index,
                args,
            } => ExprData::EnumExpr {
                enum_name: enum_name.clone(),
                variant_index: *variant_index,
                args: match args {
                    HirEnumArgs::Unit => EnumArgsData::Unit,
                    HirEnumArgs::Tuple(exprs) => EnumArgsData::Tuple(exprs.clone()),
                    HirEnumArgs::Struct(exprs) => EnumArgsData::Struct(exprs.clone()),
                },
                span,
            },
        }
    }
}

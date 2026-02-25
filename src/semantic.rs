use crate::ast::*;
use crate::type_context::{TypeContext, TypeMap, VariantInfo, collect_pattern_bindings};
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct VarInfo {
    mutable: bool,
    var_type: Type,
}

struct Scope {
    vars: HashMap<String, VarInfo>,
}

struct LoopContext {
    label: Option<String>,
}

pub struct SemanticAnalyzer<'a> {
    ctx: &'a TypeContext,
    type_map: &'a TypeMap,
    scopes: Vec<Scope>,
    errors: Vec<String>,
    current_return_type: Type,
    loop_stack: Vec<LoopContext>,
}

impl<'a> SemanticAnalyzer<'a> {
    pub fn new(ctx: &'a TypeContext, type_map: &'a TypeMap) -> Self {
        Self {
            ctx,
            type_map,
            scopes: Vec::new(),
            errors: Vec::new(),
            current_return_type: Type::I32,
            loop_stack: Vec::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // Validate types used in struct fields and enum variant fields
        for s in &program.structs {
            for f in &s.fields {
                self.validate_type(&f.field_type, s.line);
            }
        }
        for e in &program.enums {
            for v in &e.variants {
                match &v.kind {
                    VariantKind::Tuple(types) => {
                        for t in types {
                            self.validate_type(t, e.line);
                        }
                    }
                    VariantKind::Struct(fields) => {
                        for f in fields {
                            self.validate_type(&f.field_type, e.line);
                        }
                    }
                    VariantKind::Unit => {}
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

    fn validate_type(&mut self, ty: &Type, line: usize) {
        match ty {
            Type::Error => {}
            Type::Named(name) => {
                if !self.ctx.structs.contains_key(name) && !self.ctx.enums.contains_key(name) {
                    self.errors
                        .push(format!("{}行目: 未定義の型 '{}'", line, name));
                }
            }
            Type::Tuple(types) => {
                for t in types {
                    self.validate_type(t, line);
                }
            }
            _ => {}
        }
    }

    fn analyze_function(&mut self, func: &FunctionDef) {
        self.current_return_type = func.return_type.clone();
        self.push_scope();
        for param in &func.params {
            self.validate_type(&param.param_type, func.line);
            self.define_var(&param.name, false, param.param_type.clone(), func.line);
        }
        let needs_value = func.return_type != Type::Unit;
        let block_type = self.analyze_block_inner(&func.body, needs_value);

        if func.return_type == Type::Unit {
            // Unit return type: ok if no tail expression or tail is unit
        } else if let Some(ty) = block_type {
            if !self.block_always_returns(&func.body) {
                self.check_type_match(&func.return_type, &ty, func.line, "関数の戻り値");
            }
        } else if !self.block_always_returns(&func.body) {
            self.errors.push(format!(
                "{}行目: 関数 '{}' は {:?} の戻り値が必要ですが、末尾式がありません",
                func.line, func.name, func.return_type
            ));
        }

        self.pop_scope();
    }

    fn block_always_returns(&self, block: &Block) -> bool {
        for stmt in &block.stmts {
            if let Stmt::Return { .. } = stmt {
                return true;
            }
            if let Stmt::Expr(Expr::If {
                then_block,
                else_block: Some(else_clause),
                ..
            }) = stmt
            {
                let then_returns = self.block_always_returns(then_block);
                let else_returns = match else_clause.as_ref() {
                    ElseClause::ElseBlock(b) => self.block_always_returns(b),
                    ElseClause::ElseIf(expr) => self.expr_always_returns(expr),
                };
                if then_returns && else_returns {
                    return true;
                }
            }
        }
        false
    }

    fn expr_always_returns(&self, expr: &Expr) -> bool {
        if let Expr::If {
            then_block,
            else_block: Some(else_clause),
            ..
        } = expr
        {
            let then_returns = self.block_always_returns(then_block);
            let else_returns = match else_clause.as_ref() {
                ElseClause::ElseBlock(b) => self.block_always_returns(b),
                ElseClause::ElseIf(e) => self.expr_always_returns(e),
            };
            then_returns && else_returns
        } else {
            false
        }
    }

    fn analyze_block(&mut self, block: &Block) -> Option<Type> {
        self.analyze_block_inner(block, false)
    }

    fn analyze_block_inner(&mut self, block: &Block, needs_value: bool) -> Option<Type> {
        for stmt in &block.stmts {
            self.analyze_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            self.analyze_expr(expr);
            if needs_value {
                self.check_expr_has_value(expr);
            }
            let ty = self.type_map.get(expr).clone();
            Some(ty)
        } else {
            None
        }
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                name,
                mutable,
                var_type,
                init,
                line,
            } => {
                self.analyze_expr(init);
                self.check_expr_has_value(init);
                let init_type = self.type_map.get(init);

                let resolved_type = if let Some(declared_type) = var_type {
                    self.validate_type(declared_type, *line);
                    self.check_type_match(declared_type, init_type, *line, "let 初期化式");
                    declared_type.clone()
                } else {
                    init_type.clone()
                };

                self.define_var(name, *mutable, resolved_type, *line);
            }
            Stmt::Return { value, line } => {
                if let Some(val) = value {
                    self.analyze_expr(val);
                    self.check_expr_has_value(val);
                    let ty = self.type_map.get(val);
                    let ret_type = self.current_return_type.clone();
                    self.check_type_match(&ret_type, ty, *line, "return 文");
                } else {
                    let ret_type = self.current_return_type.clone();
                    if ret_type != Type::Unit {
                        self.errors.push(format!(
                            "{}行目: return 文に値がありませんが、関数の戻り値型は {:?} です",
                            line, ret_type
                        ));
                    }
                }
            }
            Stmt::Break { label, value, line } => {
                if self.loop_stack.is_empty() {
                    self.errors
                        .push(format!("{}行目: break はループ内でのみ使用できます", line));
                } else if let Some(label_name) = label
                    && !self
                        .loop_stack
                        .iter()
                        .any(|l| l.label.as_deref() == Some(label_name))
                {
                    self.errors.push(format!(
                        "{}行目: ラベル '{}' が見つかりません",
                        line, label_name
                    ));
                }
                if let Some(val) = value {
                    self.analyze_expr(val);
                }
            }
            Stmt::Continue { label, line } => {
                if self.loop_stack.is_empty() {
                    self.errors.push(format!(
                        "{}行目: continue はループ内でのみ使用できます",
                        line
                    ));
                } else if let Some(label_name) = label
                    && !self
                        .loop_stack
                        .iter()
                        .any(|l| l.label.as_deref() == Some(label_name))
                {
                    self.errors.push(format!(
                        "{}行目: ラベル '{}' が見つかりません",
                        line, label_name
                    ));
                }
            }
            Stmt::Expr(expr) => {
                self.analyze_expr(expr);
            }
        }
    }

    fn check_expr_has_value(&mut self, expr: &Expr) {
        match expr {
            Expr::If {
                then_block,
                else_block,
                ..
            } => match else_block {
                None => {
                    self.errors
                        .push("if 式を値として使用するには else 分岐が必要です".to_string());
                }
                Some(else_clause) => {
                    if then_block.expr.is_none() && !self.block_always_returns(then_block) {
                        self.errors
                            .push("if 式の then 分岐に値がありません".to_string());
                    }
                    match else_clause.as_ref() {
                        ElseClause::ElseBlock(block) => {
                            if block.expr.is_none() && !self.block_always_returns(block) {
                                self.errors
                                    .push("if 式の else 分岐に値がありません".to_string());
                            }
                        }
                        ElseClause::ElseIf(if_expr) => {
                            self.check_expr_has_value(if_expr);
                        }
                    }
                }
            },
            Expr::Block(block) => {
                if block.expr.is_none() && !self.block_always_returns(block) {
                    self.errors.push("ブロック式に値がありません".to_string());
                }
            }
            _ => {}
        }
    }

    fn check_type_match(&mut self, expected: &Type, actual: &Type, line: usize, context: &str) {
        // Type::Error が含まれる場合はカスケードエラーを防止
        if *expected == Type::Error || *actual == Type::Error {
            return;
        }
        if expected != actual {
            self.errors.push(format!(
                "{}行目: {}: {:?} が期待されましたが {:?} が見つかりました",
                line, context, expected, actual
            ));
        }
    }

    fn analyze_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::IntegerLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::BoolLiteral(_)
            | Expr::UnitLiteral => {}
            Expr::Identifier(name, line) => {
                if !self.lookup_var(name) {
                    self.errors
                        .push(format!("{}行目: 未定義の変数 '{}'", line, name));
                }
            }
            Expr::BinaryOp { op, left, right } => {
                self.analyze_expr(left);
                self.analyze_expr(right);
                let left_type = self.type_map.get(left);
                let right_type = self.type_map.get(right);

                // Type::Error が含まれる場合はカスケードエラーを防止
                if *left_type == Type::Error || *right_type == Type::Error {
                    return;
                }

                match op {
                    BinOp::And | BinOp::Or => {
                        if *left_type != Type::Bool {
                            self.errors.push(format!(
                                "論理演算子の左辺には bool 型が必要ですが {:?} が見つかりました",
                                left_type
                            ));
                        }
                        if *right_type != Type::Bool {
                            self.errors.push(format!(
                                "論理演算子の右辺には bool 型が必要ですが {:?} が見つかりました",
                                right_type
                            ));
                        }
                    }
                    BinOp::Mod => {
                        if !left_type.is_integer() {
                            self.errors.push(format!(
                                "剰余演算子は整数型にのみ使用できますが {:?} が見つかりました",
                                left_type
                            ));
                        } else if left_type != right_type {
                            self.errors.push(format!(
                                "算術演算の両辺は同じ型でなければなりません: {:?} と {:?}",
                                left_type, right_type
                            ));
                        }
                    }
                    BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                        if !left_type.is_numeric() {
                            self.errors.push(format!(
                                "算術演算子には数値型が必要ですが {:?} が見つかりました",
                                left_type
                            ));
                        } else if left_type != right_type {
                            self.errors.push(format!(
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
                            self.errors.push(format!(
                                "比較演算の両辺は同じ型でなければなりません: {:?} と {:?}",
                                left_type, right_type
                            ));
                        }
                    }
                }
            }
            Expr::UnaryOp { op, operand } => {
                self.analyze_expr(operand);
                let ty = self.type_map.get(operand);
                if *ty == Type::Error {
                    return;
                }
                match op {
                    UnaryOp::Neg => {
                        if !ty.is_numeric() {
                            self.errors.push(format!(
                                "単項マイナスには数値型が必要ですが {:?} が見つかりました",
                                ty
                            ));
                        }
                    }
                    UnaryOp::Not => {
                        if *ty != Type::Bool {
                            self.errors.push(format!(
                                "論理否定には bool 型が必要ですが {:?} が見つかりました",
                                ty
                            ));
                        }
                    }
                }
            }
            Expr::Cast {
                expr: inner,
                target_type,
            } => {
                self.analyze_expr(inner);
                let source_type = self.type_map.get(inner);
                if *source_type == Type::Error {
                    return;
                }
                // Validate cast
                if *source_type == Type::Unit || *target_type == Type::Unit {
                    self.errors.push(
                        "() 型へのキャストまたは () 型からのキャストはできません".to_string(),
                    );
                } else if !(source_type.is_numeric() || *source_type == Type::Bool)
                    || !(target_type.is_numeric() || *target_type == Type::Bool)
                {
                    self.errors.push(format!(
                        "{:?} から {:?} へのキャストはできません",
                        source_type, target_type
                    ));
                }
            }
            Expr::Call { name, args, line } => {
                for arg in args {
                    self.analyze_expr(arg);
                    self.check_expr_has_value(arg);
                }
                if let Some(func_info) = self.ctx.functions.get(name) {
                    if args.len() != func_info.param_count {
                        self.errors.push(format!(
                            "{}行目: 関数 '{}' は {} 個の引数を取りますが {} 個渡されました",
                            line,
                            name,
                            func_info.param_count,
                            args.len()
                        ));
                    } else {
                        for (i, arg) in args.iter().enumerate() {
                            let arg_type = self.type_map.get(arg);
                            if *arg_type == Type::Error {
                                continue;
                            }
                            if *arg_type != func_info.param_types[i] {
                                self.errors.push(format!(
                                    "{}行目: 関数 '{}' の第 {} 引数: {:?} が期待されましたが {:?} が渡されました",
                                    line, name, i + 1, func_info.param_types[i], arg_type
                                ));
                            }
                        }
                    }
                } else {
                    self.errors
                        .push(format!("{}行目: 未定義の関数 '{}'", line, name));
                }
            }
            Expr::Assign { name, value, line } => {
                self.analyze_expr(value);
                self.check_expr_has_value(value);
                if let Some(info) = self.find_var(name).cloned() {
                    if !info.mutable {
                        self.errors.push(format!(
                            "{}行目: イミュータブル変数 '{}' への再代入はできません",
                            line, name
                        ));
                    }
                    let val_type = self.type_map.get(value);
                    self.check_type_match(&info.var_type, val_type, *line, "代入");
                } else {
                    self.errors
                        .push(format!("{}行目: 未定義の変数 '{}'", line, name));
                }
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.analyze_expr(condition);
                let cond_type = self.type_map.get(condition);
                if *cond_type != Type::Error && *cond_type != Type::Bool {
                    self.errors.push(format!(
                        "if 条件には bool 型が必要ですが {:?} が見つかりました",
                        cond_type
                    ));
                }
                self.push_scope();
                self.analyze_block(then_block);
                self.pop_scope();
                if let Some(else_clause) = else_block {
                    match else_clause.as_ref() {
                        ElseClause::ElseBlock(block) => {
                            self.push_scope();
                            self.analyze_block(block);
                            self.pop_scope();
                        }
                        ElseClause::ElseIf(expr) => {
                            self.analyze_expr(expr);
                        }
                    }
                }
            }
            Expr::While {
                label,
                condition,
                body,
            } => {
                self.analyze_expr(condition);
                let cond_type = self.type_map.get(condition);
                if *cond_type != Type::Error && *cond_type != Type::Bool {
                    self.errors.push(format!(
                        "while 条件には bool 型が必要ですが {:?} が見つかりました",
                        cond_type
                    ));
                }
                self.loop_stack.push(LoopContext {
                    label: label.clone(),
                });
                self.push_scope();
                self.analyze_block(body);
                self.pop_scope();
                self.loop_stack.pop();
            }
            Expr::Match {
                expr: match_expr,
                arms,
            } => {
                self.analyze_expr(match_expr);
                let match_type = self.type_map.get(match_expr).clone();

                let mut arm_types = Vec::new();
                for arm in arms {
                    self.analyze_pattern(&arm.pattern, &match_type);
                    self.push_scope();
                    self.bind_pattern_vars(&arm.pattern, &match_type);
                    self.analyze_expr(&arm.body);
                    let arm_type = self.type_map.get(&arm.body).clone();
                    arm_types.push(arm_type);
                    self.pop_scope();
                }

                // Check all arms have same type (Type::Error はスキップ)
                if arm_types.len() >= 2 {
                    let first = &arm_types[0];
                    if *first != Type::Error {
                        for (i, ty) in arm_types.iter().enumerate().skip(1) {
                            if *ty != Type::Error && ty != first {
                                self.errors.push(format!(
                                    "match アームの型が一致しません: アーム0は {:?} ですがアーム{}は {:?} です",
                                    first, i, ty
                                ));
                            }
                        }
                    }
                }

                // Exhaustiveness check
                self.check_exhaustiveness(
                    &match_type,
                    &arms.iter().map(|a| &a.pattern).collect::<Vec<_>>(),
                );
            }
            Expr::Block(block) => {
                self.push_scope();
                self.analyze_block(block);
                self.pop_scope();
            }
            Expr::FieldAccess { expr: inner, field } => {
                self.analyze_expr(inner);
                let base_type = self.type_map.get(inner);
                if *base_type == Type::Error {
                    return;
                }
                match base_type {
                    Type::Named(name) => {
                        if let Some(struct_info) = self.ctx.structs.get(name) {
                            if !struct_info.fields.iter().any(|(fname, _)| fname == field) {
                                self.errors.push(format!(
                                    "構造体 '{}' にフィールド '{}' はありません",
                                    name, field
                                ));
                            }
                        } else {
                            self.errors.push(format!(
                                "型 '{:?}' にフィールドアクセスはできません",
                                base_type
                            ));
                        }
                    }
                    _ => {
                        self.errors.push(format!(
                            "型 '{:?}' にフィールドアクセスはできません",
                            base_type
                        ));
                    }
                }
            }
            Expr::TupleIndex { expr: inner, index } => {
                self.analyze_expr(inner);
                let base_type = self.type_map.get(inner);
                if *base_type == Type::Error {
                    return;
                }
                if let Type::Tuple(types) = base_type {
                    if (*index as usize) >= types.len() {
                        self.errors.push(format!(
                            "タプルのインデックス {} は範囲外です (要素数: {})",
                            index,
                            types.len()
                        ));
                    }
                } else {
                    self.errors.push(format!(
                        "型 '{:?}' にタプルインデックスアクセスはできません",
                        base_type
                    ));
                }
            }
            Expr::TupleExpr(elems) => {
                for elem in elems {
                    self.analyze_expr(elem);
                }
            }
            Expr::StructExpr { name, fields, line } => {
                if let Some(struct_info) = self.ctx.structs.get(name) {
                    // Check all fields are provided
                    let expected_fields: Vec<&str> =
                        struct_info.fields.iter().map(|(n, _)| n.as_str()).collect();
                    let provided_fields: Vec<&str> =
                        fields.iter().map(|(n, _)| n.as_str()).collect();

                    for ef in &expected_fields {
                        if !provided_fields.contains(ef) {
                            self.errors.push(format!(
                                "{}行目: 構造体 '{}' のフィールド '{}' が指定されていません",
                                line, name, ef
                            ));
                        }
                    }
                    for pf in &provided_fields {
                        if !expected_fields.contains(pf) {
                            self.errors.push(format!(
                                "{}行目: 構造体 '{}' に未知のフィールド '{}' があります",
                                line, name, pf
                            ));
                        }
                    }

                    // Type check each field
                    for (field_name, field_expr) in fields {
                        self.analyze_expr(field_expr);
                        if let Some((_, expected_type)) =
                            struct_info.fields.iter().find(|(n, _)| n == field_name)
                        {
                            let actual_type = self.type_map.get(field_expr);
                            self.check_type_match(
                                expected_type,
                                actual_type,
                                *line,
                                &format!("構造体フィールド '{}'", field_name),
                            );
                        }
                    }
                } else {
                    self.errors
                        .push(format!("{}行目: 未定義の構造体 '{}'", line, name));
                }
            }
            Expr::EnumExpr {
                enum_name,
                variant,
                args,
                line,
            } => {
                if let Some(enum_info) = self.ctx.enums.get(enum_name) {
                    if let Some((_, variant_info)) =
                        enum_info.variants.iter().find(|(n, _)| n == variant)
                    {
                        match (variant_info, args) {
                            (VariantInfo::Unit, EnumArgs::Unit) => {}
                            (VariantInfo::Tuple(expected_types), EnumArgs::Tuple(exprs)) => {
                                if exprs.len() != expected_types.len() {
                                    self.errors.push(format!(
                                        "{}行目: バリアント '{}::{}' は {} 個の引数を取りますが {} 個渡されました",
                                        line, enum_name, variant, expected_types.len(), exprs.len()
                                    ));
                                } else {
                                    for (i, expr) in exprs.iter().enumerate() {
                                        self.analyze_expr(expr);
                                        let actual = self.type_map.get(expr);
                                        self.check_type_match(
                                            &expected_types[i],
                                            actual,
                                            *line,
                                            &format!("enum バリアント引数 {}", i),
                                        );
                                    }
                                }
                            }
                            (VariantInfo::Struct(expected_fields), EnumArgs::Struct(fields)) => {
                                for (field_name, field_expr) in fields {
                                    self.analyze_expr(field_expr);
                                    if let Some((_, expected_type)) =
                                        expected_fields.iter().find(|(n, _)| n == field_name)
                                    {
                                        let actual = self.type_map.get(field_expr);
                                        self.check_type_match(
                                            expected_type,
                                            actual,
                                            *line,
                                            &format!("enum フィールド '{}'", field_name),
                                        );
                                    }
                                }
                            }
                            _ => {
                                self.errors.push(format!(
                                    "{}行目: バリアント '{}::{}' の引数形式が不正です",
                                    line, enum_name, variant
                                ));
                            }
                        }
                    } else {
                        self.errors.push(format!(
                            "{}行目: 列挙型 '{}' にバリアント '{}' はありません",
                            line, enum_name, variant
                        ));
                    }
                } else {
                    self.errors
                        .push(format!("{}行目: 未定義の列挙型 '{}'", line, enum_name));
                }
            }
        }
    }

    fn analyze_pattern(&mut self, pattern: &Pattern, expected_type: &Type) {
        match pattern {
            Pattern::IntLiteral(_) | Pattern::Range { .. } => {
                if !expected_type.is_integer() {
                    self.errors.push(format!(
                        "整数パターンは整数型に対してのみ使用できます (型: {:?})",
                        expected_type
                    ));
                }
            }
            Pattern::BoolLiteral(_) => {
                if *expected_type != Type::Bool {
                    self.errors.push(format!(
                        "bool パターンは bool 型に対してのみ使用できます (型: {:?})",
                        expected_type
                    ));
                }
            }
            Pattern::Wildcard | Pattern::Binding(_) => {}
            Pattern::Tuple(patterns) => {
                if let Type::Tuple(types) = expected_type {
                    if patterns.len() != types.len() {
                        self.errors.push(format!(
                            "タプルパターンの要素数が一致しません: {} が期待されましたが {} です",
                            types.len(),
                            patterns.len()
                        ));
                    } else {
                        for (pat, ty) in patterns.iter().zip(types.iter()) {
                            self.analyze_pattern(pat, ty);
                        }
                    }
                } else {
                    self.errors.push(format!(
                        "タプルパターンはタプル型に対してのみ使用できます (型: {:?})",
                        expected_type
                    ));
                }
            }
            Pattern::Struct { name, fields } => {
                if let Type::Named(type_name) = expected_type {
                    if name != type_name {
                        self.errors.push(format!(
                            "構造体パターン '{}' は型 '{:?}' と一致しません",
                            name, expected_type
                        ));
                    } else if let Some(struct_info) = self.ctx.structs.get(name) {
                        for (field_name, field_pat) in fields {
                            if let Some((_, field_type)) =
                                struct_info.fields.iter().find(|(n, _)| n == field_name)
                            {
                                self.analyze_pattern(field_pat, field_type);
                            } else {
                                self.errors.push(format!(
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
                        self.errors.push(format!(
                            "enum パターン '{}' は型 '{:?}' と一致しません",
                            enum_name, expected_type
                        ));
                    } else if let Some(enum_info) = self.ctx.enums.get(enum_name) {
                        if let Some((_, variant_info)) =
                            enum_info.variants.iter().find(|(n, _)| n == variant)
                        {
                            match (variant_info, args) {
                                (VariantInfo::Unit, EnumPatternArgs::Unit) => {}
                                (VariantInfo::Tuple(types), EnumPatternArgs::Tuple(pats)) => {
                                    if pats.len() != types.len() {
                                        self.errors.push(format!(
                                            "バリアント '{}::{}' のパターン引数数が一致しません",
                                            enum_name, variant
                                        ));
                                    } else {
                                        for (pat, ty) in pats.iter().zip(types.iter()) {
                                            self.analyze_pattern(pat, ty);
                                        }
                                    }
                                }
                                (
                                    VariantInfo::Struct(fields),
                                    EnumPatternArgs::Struct(pat_fields),
                                ) => {
                                    for (field_name, field_pat) in pat_fields {
                                        if let Some((_, field_type)) =
                                            fields.iter().find(|(n, _)| n == field_name)
                                        {
                                            self.analyze_pattern(field_pat, field_type);
                                        }
                                    }
                                }
                                _ => {
                                    self.errors.push(format!(
                                        "バリアント '{}::{}' のパターン形式が不正です",
                                        enum_name, variant
                                    ));
                                }
                            }
                        } else {
                            self.errors.push(format!(
                                "列挙型 '{}' にバリアント '{}' はありません",
                                enum_name, variant
                            ));
                        }
                    }
                }
            }
        }
    }

    fn bind_pattern_vars(&mut self, pattern: &Pattern, ty: &Type) {
        let bindings = collect_pattern_bindings(self.ctx, pattern, ty);
        for (name, var_type) in bindings {
            self.define_var(&name, false, var_type, 0);
        }
    }

    fn check_exhaustiveness(&mut self, match_type: &Type, patterns: &[&Pattern]) {
        // Check if there is a wildcard or binding that catches all
        if patterns
            .iter()
            .any(|p| matches!(p, Pattern::Wildcard | Pattern::Binding(_)))
        {
            return;
        }

        match match_type {
            Type::Bool => {
                let has_true = patterns
                    .iter()
                    .any(|p| matches!(p, Pattern::BoolLiteral(true)));
                let has_false = patterns
                    .iter()
                    .any(|p| matches!(p, Pattern::BoolLiteral(false)));
                if !has_true || !has_false {
                    self.errors.push(
                        "match 式が網羅的ではありません: bool 型は true と false の両方が必要です"
                            .to_string(),
                    );
                }
            }
            Type::Named(name) => {
                if let Some(enum_info) = self.ctx.enums.get(name) {
                    let all_variants: Vec<&str> =
                        enum_info.variants.iter().map(|(n, _)| n.as_str()).collect();
                    let covered_variants: Vec<&str> = patterns
                        .iter()
                        .filter_map(|p| {
                            if let Pattern::Enum { variant, .. } = p {
                                Some(variant.as_str())
                            } else {
                                None
                            }
                        })
                        .collect();
                    for v in &all_variants {
                        if !covered_variants.contains(v) {
                            self.errors.push(format!(
                                "match 式が網羅的ではありません: バリアント '{}' がカバーされていません",
                                v
                            ));
                        }
                    }
                }
            }
            _ if match_type.is_integer() => {
                // Integer types require a wildcard/binding pattern
                self.errors.push(
                    "match 式が網羅的ではありません: 整数型にはワイルドカード '_' パターンが必要です"
                        .to_string(),
                );
            }
            _ => {}
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope {
            vars: HashMap::new(),
        });
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_var(&mut self, name: &str, mutable: bool, var_type: Type, line: usize) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.vars.contains_key(name) {
                self.errors.push(format!(
                    "{}行目: 変数 '{}' は同一スコープ内で既に宣言されています",
                    line, name
                ));
            } else {
                scope
                    .vars
                    .insert(name.to_string(), VarInfo { mutable, var_type });
            }
        }
    }

    fn lookup_var(&self, name: &str) -> bool {
        for scope in self.scopes.iter().rev() {
            if scope.vars.contains_key(name) {
                return true;
            }
        }
        false
    }

    fn find_var(&self, name: &str) -> Option<&VarInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.vars.get(name) {
                return Some(info);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::type_context::TypeContext;
    use crate::type_resolver::TypeResolver;

    fn analyze(source: &str) -> Result<(), Vec<String>> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let type_ctx = TypeContext::build(&program).map_err(|e| e)?;
        let type_map = TypeResolver::new(&type_ctx).resolve(&program);
        let mut analyzer = SemanticAnalyzer::new(&type_ctx, &type_map);
        analyzer.analyze(&program)
    }

    fn analyze_errors(source: &str) -> Vec<String> {
        analyze(source).unwrap_err()
    }

    #[test]
    fn valid_simple_function() {
        assert!(analyze("fn main() -> i32 { 0 }").is_ok());
    }

    #[test]
    fn valid_function_with_params() {
        assert!(analyze("fn add(a: i32, b: i32) -> i32 { a + b }").is_ok());
    }

    #[test]
    fn valid_let_and_use() {
        assert!(analyze("fn main() -> i32 { let x: i32 = 42; x }").is_ok());
    }

    #[test]
    fn valid_mutable_reassign() {
        assert!(analyze("fn main() -> i32 { let mut x: i32 = 0; x = 42; x }").is_ok());
    }

    #[test]
    fn valid_forward_reference() {
        assert!(
            analyze(
                "fn main() -> i32 { foo() }
             fn foo() -> i32 { 42 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_print_i32_builtin() {
        assert!(analyze("fn main() -> i32 { print_i32(42); 0 }").is_ok());
    }

    #[test]
    fn undefined_variable() {
        let errors = analyze_errors("fn main() -> i32 { x }");
        assert!(errors.iter().any(|e| e.contains("未定義の変数 'x'")));
    }

    #[test]
    fn undefined_function() {
        let errors = analyze_errors("fn main() -> i32 { foo() }");
        assert!(errors.iter().any(|e| e.contains("未定義の関数 'foo'")));
    }

    #[test]
    fn argument_count_mismatch() {
        let errors = analyze_errors(
            "fn foo(a: i32) -> i32 { a }
             fn main() -> i32 { foo(1, 2) }",
        );
        assert!(
            errors
                .iter()
                .any(|e| e.contains("1 個の引数を取りますが 2 個渡されました"))
        );
    }

    #[test]
    fn immutable_reassign() {
        let errors = analyze_errors("fn main() -> i32 { let x: i32 = 0; x = 1; x }");
        assert!(
            errors
                .iter()
                .any(|e| e.contains("イミュータブル変数 'x' への再代入"))
        );
    }

    #[test]
    fn duplicate_function() {
        let errors = analyze_errors(
            "fn foo() -> i32 { 0 }
             fn foo() -> i32 { 1 }
             fn main() -> i32 { 0 }",
        );
        assert!(
            errors
                .iter()
                .any(|e| e.contains("関数 'foo' は既に定義されています"))
        );
    }

    #[test]
    fn undefined_variable_from_inner_scope() {
        let errors = analyze_errors(
            "fn main() -> i32 {
                if true { let y: i32 = 1; y; }
                y
            }",
        );
        assert!(errors.iter().any(|e| e.contains("未定義の変数 'y'")));
    }

    #[test]
    fn missing_tail_expression() {
        let errors = analyze_errors("fn main() -> i32 { let x: i32 = 0; }");
        assert!(errors.iter().any(|e| e.contains("末尾式がありません")));
    }

    #[test]
    fn if_value_requires_else() {
        let errors = analyze_errors("fn main() -> i32 { if true { 1 } }");
        assert!(errors.iter().any(|e| e.contains("else 分岐が必要です")));
    }

    #[test]
    fn valid_while_loop() {
        assert!(
            analyze(
                "fn main() -> i32 {
                let mut x: i32 = 0;
                while x < 10 {
                    x = x + 1;
                }
                x
            }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_return_statement() {
        assert!(analyze("fn main() -> i32 { return 42; }").is_ok());
    }

    #[test]
    fn valid_if_else_as_value() {
        assert!(analyze("fn main() -> i32 { if true { 1 } else { 0 } }").is_ok());
    }

    #[test]
    fn duplicate_var_in_same_scope() {
        let errors = analyze_errors("fn main() -> i32 { let x: i32 = 1; let x: i32 = 2; x }");
        assert!(
            errors
                .iter()
                .any(|e| e.contains("変数 'x' は同一スコープ内で既に宣言されています"))
        );
    }

    // MS2 tests

    #[test]
    fn type_mismatch_int_bool() {
        let errors = analyze_errors("fn main() -> i32 { if 42 { 1 } else { 0 } }");
        assert!(errors.iter().any(|e| e.contains("bool 型が必要")));
    }

    #[test]
    fn type_mismatch_arithmetic() {
        let errors =
            analyze_errors("fn main() -> i32 { let x: i32 = 1; let y: i64 = 2i64; x + y; 0 }");
        assert!(errors.iter().any(|e| e.contains("同じ型")));
    }

    #[test]
    fn valid_type_inference() {
        assert!(analyze("fn main() -> i32 { let x = 42; x }").is_ok());
    }

    #[test]
    fn valid_type_cast() {
        assert!(analyze("fn main() -> i32 { let x: i64 = 42i64; x as i32 }").is_ok());
    }

    #[test]
    fn invalid_unit_cast() {
        let errors = analyze_errors("fn main() -> i32 { () as i32; 0 }");
        assert!(errors.iter().any(|e| e.contains("キャスト")));
    }

    #[test]
    fn float_modulo_error() {
        let errors =
            analyze_errors("fn main() -> i32 { let x: f64 = 1.0; let y: f64 = 2.0; x % y; 0 }");
        assert!(errors.iter().any(|e| e.contains("整数型")));
    }

    #[test]
    fn break_outside_loop() {
        let errors = analyze_errors("fn main() -> i32 { break; 0 }");
        assert!(errors.iter().any(|e| e.contains("ループ内")));
    }

    #[test]
    fn continue_outside_loop() {
        let errors = analyze_errors("fn main() -> i32 { continue; 0 }");
        assert!(errors.iter().any(|e| e.contains("ループ内")));
    }

    #[test]
    fn valid_unit_return() {
        assert!(analyze("fn foo() { } fn main() -> i32 { foo(); 0 }").is_ok());
    }

    #[test]
    fn valid_bool_type() {
        assert!(analyze("fn main() -> i32 { let b: bool = true; if b { 1 } else { 0 } }").is_ok());
    }

    // 型推論エッジケーステスト

    #[test]
    fn valid_nested_struct_field_access() {
        assert!(
            analyze(
                "struct Inner { value: i32 }
                 struct Outer { inner: Inner }
                 fn main() -> i32 {
                     let o = Outer { inner: Inner { value: 42 } };
                     o.inner.value
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_match_arm_type_consistency() {
        assert!(
            analyze(
                "fn main() -> i32 {
                     let x: i32 = 1;
                     match x {
                         0 => 10,
                         1 => 20,
                         _ => 30,
                     }
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn match_arm_type_mismatch() {
        let errors = analyze_errors(
            "fn main() -> i32 {
                 let x: i32 = 1;
                 match x {
                     0 => 10,
                     1 => true,
                     _ => 30,
                 }
             }",
        );
        assert!(
            errors
                .iter()
                .any(|e| e.contains("match アームの型が一致しません"))
        );
    }

    #[test]
    fn valid_while_break_value() {
        assert!(
            analyze(
                "fn main() -> i32 {
                     let mut i: i32 = 0;
                     while i < 10 {
                         i = i + 1;
                         if i == 5 {
                             break 42;
                         }
                     }
                     0
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_if_else_type_inference() {
        assert!(
            analyze(
                "fn main() -> i32 {
                     let x: i32 = if true { 1 } else { 2 };
                     x
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_if_else_i64_type_inference() {
        assert!(
            analyze(
                "fn main() -> i64 {
                     if false { 100i64 } else { 200i64 }
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_block_tail_expression() {
        assert!(
            analyze(
                "fn main() -> i32 {
                     let x: i32 = {
                         let a: i32 = 1;
                         let b: i32 = 2;
                         a + b
                     };
                     x
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_nested_block_tail_expression() {
        assert!(
            analyze(
                "fn main() -> i32 {
                     {
                         {
                             42
                         }
                     }
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_recursive_function() {
        assert!(
            analyze(
                "fn factorial(n: i32) -> i32 {
                     if n == 0 {
                         1
                     } else {
                         n * factorial(n - 1)
                     }
                 }
                 fn main() -> i32 {
                     factorial(5)
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_mutual_recursion() {
        assert!(
            analyze(
                "fn is_even(n: i32) -> i32 {
                     if n == 0 { 1 } else { is_odd(n - 1) }
                 }
                 fn is_odd(n: i32) -> i32 {
                     if n == 0 { 0 } else { is_even(n - 1) }
                 }
                 fn main() -> i32 {
                     is_even(4)
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_match_enum_type_inference() {
        assert!(
            analyze(
                "enum Option { Some(i32), None }
                 fn main() -> i32 {
                     let opt = Option::Some(42);
                     match opt {
                         Option::Some(v) => v,
                         Option::None => 0,
                     }
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_match_bool_exhaustive() {
        assert!(
            analyze(
                "fn main() -> i32 {
                     let b: bool = true;
                     match b {
                         true => 1,
                         false => 0,
                     }
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn valid_struct_in_function_param() {
        assert!(
            analyze(
                "struct Point { x: i32, y: i32 }
                 fn sum(p: Point) -> i32 {
                     p.x + p.y
                 }
                 fn main() -> i32 {
                     let p = Point { x: 10, y: 20 };
                     sum(p)
                 }"
            )
            .is_ok()
        );
    }

    #[test]
    fn type_mismatch_return_if_else_branches() {
        let errors = analyze_errors(
            "fn main() -> i64 {
                 if true { 1 } else { 2i64 }
             }",
        );
        assert!(
            errors
                .iter()
                .any(|e| e.contains("期待されましたが") || e.contains("戻り値"))
        );
    }

    #[test]
    fn type_mismatch_return_type() {
        let errors = analyze_errors(
            "fn main() -> i32 {
                 true
             }",
        );
        assert!(
            errors
                .iter()
                .any(|e| e.contains("期待されましたが") || e.contains("戻り値"))
        );
    }

    #[test]
    fn valid_tuple_type_inference() {
        assert!(
            analyze(
                "fn main() -> i32 {
                     let t = (1, 2);
                     t.0 + t.1
                 }"
            )
            .is_ok()
        );
    }
}

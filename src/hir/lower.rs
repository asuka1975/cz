use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::hir::types::{TypeContext, VariantInfo, collect_pattern_bindings};
use crate::hir::*;
use crate::scope::ScopeStack;
use crate::syntax::ast::{self, BinOp, UnaryOp};
use crate::syntax::token::{FloatSuffix, IntSuffix};
use std::collections::HashMap;

pub struct Lowering<'a> {
    ctx: &'a TypeContext,
    source: &'a str,
    expr_arena: Arena<HirExpr>,
    stmt_arena: Arena<HirStmt>,
    vars: Vec<VarInfo>,
    func_ids: HashMap<String, FuncId>,
    func_names: Vec<String>,
    scope: ScopeStack<VarId>,
    errors: Vec<String>,
}

impl<'a> Lowering<'a> {
    pub fn new(ctx: &'a TypeContext, source: &'a str) -> Self {
        let mut func_ids = HashMap::new();
        let mut func_names = Vec::new();
        for name in ctx.functions.keys() {
            let id = FuncId(func_names.len() as u32);
            func_ids.insert(name.clone(), id);
            func_names.push(name.clone());
        }

        Self {
            ctx,
            source,
            expr_arena: Arena::new(),
            stmt_arena: Arena::new(),
            vars: Vec::new(),
            func_ids,
            func_names,
            scope: ScopeStack::new(),
            errors: Vec::new(),
        }
    }

    pub fn lower(
        mut self,
        program: &ast::Program,
        ast_exprs: &Arena<ast::Expr>,
        ast_stmts: &Arena<ast::Stmt>,
    ) -> Result<LowerResult, Vec<String>> {
        let struct_order: Vec<String> = program.structs.iter().map(|s| s.name.clone()).collect();
        let enum_order: Vec<String> = program.enums.iter().map(|e| e.name.clone()).collect();

        let mut functions = Vec::new();
        for func_def in &program.functions {
            let hir_func = self.lower_function(func_def, ast_exprs, ast_stmts);
            functions.push(hir_func);
        }

        if self.errors.is_empty() {
            Ok(LowerResult {
                program: HirProgram {
                    functions,
                    struct_order,
                    enum_order,
                },
                expr_arena: self.expr_arena,
                stmt_arena: self.stmt_arena,
                vars: self.vars,
                func_names: self.func_names,
            })
        } else {
            Err(self.errors)
        }
    }

    fn alloc_var(&mut self, name: String, ty: Type, mutable: bool, span: Span) -> VarId {
        let id = VarId(self.vars.len() as u32);
        self.vars.push(VarInfo {
            name,
            ty,
            mutable,
            span,
        });
        id
    }

    fn alloc_expr(&mut self, kind: HirExprKind, ty: Type, span: Span) -> HirExprId {
        self.expr_arena.alloc(HirExpr { kind, ty, span })
    }

    fn alloc_stmt(&mut self, stmt: HirStmt) -> HirStmtId {
        self.stmt_arena.alloc(stmt)
    }

    fn error(&mut self, span: Span, msg: String) {
        let line = crate::diagnostics::Diagnostics::span_to_line(self.source, span);
        self.errors
            .push(format!("意味解析エラー: {}行目: {}", line, msg));
    }

    // --- Function ---

    fn lower_function(
        &mut self,
        func: &ast::FunctionDef,
        exprs: &Arena<ast::Expr>,
        stmts: &Arena<ast::Stmt>,
    ) -> HirFunctionDef {
        let func_id = self.func_ids[&func.name];
        self.scope.push();

        let mut params = Vec::new();
        for param in &func.params {
            let var_id = self.alloc_var(
                param.name.clone(),
                param.param_type.clone(),
                false,
                func.span,
            );
            self.scope.define(param.name.clone(), var_id);
            params.push(var_id);
        }

        let body = self.lower_block(&func.body, exprs, stmts);
        self.scope.pop();

        HirFunctionDef {
            func_id,
            name: func.name.clone(),
            params,
            return_type: func.return_type.clone(),
            body,
            span: func.span,
        }
    }

    // --- Block ---

    fn lower_block(
        &mut self,
        block: &ast::Block,
        exprs: &Arena<ast::Expr>,
        stmts: &Arena<ast::Stmt>,
    ) -> HirBlock {
        let mut hir_stmts = Vec::new();
        for &stmt_id in &block.stmts {
            let hir_stmt = self.lower_stmt(stmts.get(stmt_id), exprs, stmts);
            hir_stmts.push(hir_stmt);
        }
        let hir_expr = block
            .expr
            .map(|eid| self.lower_expr(exprs.get(eid), exprs, stmts));
        HirBlock {
            stmts: hir_stmts,
            expr: hir_expr,
        }
    }

    // --- Statement ---

    fn lower_stmt(
        &mut self,
        stmt: &ast::Stmt,
        exprs: &Arena<ast::Expr>,
        stmts: &Arena<ast::Stmt>,
    ) -> HirStmtId {
        match stmt {
            ast::Stmt::Let {
                name,
                mutable,
                var_type,
                init,
                span,
            } => {
                let hir_init = self.lower_expr(exprs.get(*init), exprs, stmts);
                let resolved_type = if let Some(declared) = var_type {
                    declared.clone()
                } else {
                    self.expr_arena.get(hir_init).ty.clone()
                };
                let var_id = self.alloc_var(name.clone(), resolved_type, *mutable, *span);
                let shadowed = self.scope.define(name.clone(), var_id);
                if shadowed {
                    self.error(
                        *span,
                        format!("変数 '{}' は同じスコープ内で既に定義されています", name),
                    );
                }
                self.alloc_stmt(HirStmt::Let {
                    var: var_id,
                    init: hir_init,
                    span: *span,
                })
            }
            ast::Stmt::Return { value, span } => {
                let hir_val = value.map(|eid| self.lower_expr(exprs.get(eid), exprs, stmts));
                self.alloc_stmt(HirStmt::Return {
                    value: hir_val,
                    span: *span,
                })
            }
            ast::Stmt::Break { label, value, span } => {
                let hir_val = value.map(|eid| self.lower_expr(exprs.get(eid), exprs, stmts));
                self.alloc_stmt(HirStmt::Break {
                    label: label.clone(),
                    value: hir_val,
                    span: *span,
                })
            }
            ast::Stmt::Continue { label, span } => self.alloc_stmt(HirStmt::Continue {
                label: label.clone(),
                span: *span,
            }),
            ast::Stmt::Expr(eid) => {
                let hir_expr = self.lower_expr(exprs.get(*eid), exprs, stmts);
                self.alloc_stmt(HirStmt::Expr(hir_expr))
            }
        }
    }

    // --- Expression ---

    fn lower_expr(
        &mut self,
        expr: &ast::Expr,
        exprs: &Arena<ast::Expr>,
        stmts: &Arena<ast::Stmt>,
    ) -> HirExprId {
        match expr {
            ast::Expr::IntegerLiteral {
                value,
                suffix,
                span,
            } => {
                let ty = match suffix {
                    Some(IntSuffix::I8) => Type::I8,
                    Some(IntSuffix::I16) => Type::I16,
                    Some(IntSuffix::I32) => Type::I32,
                    Some(IntSuffix::I64) => Type::I64,
                    None => Type::I32,
                };
                self.alloc_expr(HirExprKind::IntegerLiteral { value: *value }, ty, *span)
            }
            ast::Expr::FloatLiteral {
                value,
                suffix,
                span,
            } => {
                let ty = match suffix {
                    Some(FloatSuffix::F32) => Type::F32,
                    Some(FloatSuffix::F64) | None => Type::F64,
                };
                self.alloc_expr(HirExprKind::FloatLiteral { value: *value }, ty, *span)
            }
            ast::Expr::BoolLiteral { value, span } => {
                self.alloc_expr(HirExprKind::BoolLiteral(*value), Type::Bool, *span)
            }
            ast::Expr::UnitLiteral { span } => {
                self.alloc_expr(HirExprKind::UnitLiteral, Type::Unit, *span)
            }
            ast::Expr::Identifier { name, span } => {
                if let Some(var_id) = self.scope.lookup(name) {
                    let ty = self.vars[var_id.0 as usize].ty.clone();
                    self.alloc_expr(HirExprKind::Var(var_id), ty, *span)
                } else {
                    self.error(*span, format!("未定義の変数 '{}'", name));
                    self.alloc_expr(HirExprKind::Var(VarId(0)), Type::Error, *span)
                }
            }
            ast::Expr::BinaryOp {
                op,
                left,
                right,
                span,
            } => {
                let hir_left = self.lower_expr(exprs.get(*left), exprs, stmts);
                let hir_right = self.lower_expr(exprs.get(*right), exprs, stmts);
                let ty = match op {
                    BinOp::Eq
                    | BinOp::NotEq
                    | BinOp::Lt
                    | BinOp::Gt
                    | BinOp::LtEq
                    | BinOp::GtEq
                    | BinOp::And
                    | BinOp::Or => Type::Bool,
                    _ => self.expr_arena.get(hir_left).ty.clone(),
                };
                self.alloc_expr(
                    HirExprKind::BinaryOp {
                        op: *op,
                        left: hir_left,
                        right: hir_right,
                    },
                    ty,
                    *span,
                )
            }
            ast::Expr::UnaryOp { op, operand, span } => {
                let hir_operand = self.lower_expr(exprs.get(*operand), exprs, stmts);
                let ty = match op {
                    UnaryOp::Neg => self.expr_arena.get(hir_operand).ty.clone(),
                    UnaryOp::Not => Type::Bool,
                };
                self.alloc_expr(
                    HirExprKind::UnaryOp {
                        op: *op,
                        operand: hir_operand,
                    },
                    ty,
                    *span,
                )
            }
            ast::Expr::Cast {
                expr: inner,
                target_type,
                span,
            } => {
                let hir_inner = self.lower_expr(exprs.get(*inner), exprs, stmts);
                self.alloc_expr(
                    HirExprKind::Cast {
                        expr: hir_inner,
                        target_type: target_type.clone(),
                    },
                    target_type.clone(),
                    *span,
                )
            }
            ast::Expr::Call { name, args, span } => {
                let hir_args: Vec<HirExprId> = args
                    .iter()
                    .map(|aid| self.lower_expr(exprs.get(*aid), exprs, stmts))
                    .collect();
                if let Some(&func_id) = self.func_ids.get(name) {
                    let ret_ty = self.ctx.functions[name].return_type.clone();
                    self.alloc_expr(
                        HirExprKind::Call {
                            func: func_id,
                            args: hir_args,
                        },
                        ret_ty,
                        *span,
                    )
                } else {
                    self.error(*span, format!("未定義の関数 '{}'", name));
                    self.alloc_expr(
                        HirExprKind::Call {
                            func: FuncId(0),
                            args: hir_args,
                        },
                        Type::Error,
                        *span,
                    )
                }
            }
            ast::Expr::Assign { name, value, span } => {
                let hir_value = self.lower_expr(exprs.get(*value), exprs, stmts);
                if let Some(var_id) = self.scope.lookup(name) {
                    self.alloc_expr(
                        HirExprKind::Assign {
                            var: var_id,
                            value: hir_value,
                        },
                        Type::Unit,
                        *span,
                    )
                } else {
                    self.error(*span, format!("未定義の変数 '{}'", name));
                    self.alloc_expr(
                        HirExprKind::Assign {
                            var: VarId(0),
                            value: hir_value,
                        },
                        Type::Unit,
                        *span,
                    )
                }
            }
            ast::Expr::If {
                condition,
                then_block,
                else_block,
                span,
            } => {
                let hir_cond = self.lower_expr(exprs.get(*condition), exprs, stmts);
                self.scope.push();
                let hir_then = self.lower_block(then_block, exprs, stmts);
                self.scope.pop();

                let hir_else = else_block.as_ref().map(|ec| match ec {
                    ast::ElseClause::ElseBlock(block) => {
                        self.scope.push();
                        let hir_block = self.lower_block(block, exprs, stmts);
                        self.scope.pop();
                        HirElseClause::ElseBlock(hir_block)
                    }
                    ast::ElseClause::ElseIf(eid) => {
                        let hir_expr = self.lower_expr(exprs.get(*eid), exprs, stmts);
                        HirElseClause::ElseIf(hir_expr)
                    }
                });

                // Compute type
                let ty = if else_block.is_some() {
                    if let Some(tail) = &hir_then.expr {
                        self.expr_arena.get(*tail).ty.clone()
                    } else {
                        Type::Unit
                    }
                } else {
                    Type::Unit
                };

                self.alloc_expr(
                    HirExprKind::If {
                        condition: hir_cond,
                        then_block: hir_then,
                        else_block: hir_else,
                    },
                    ty,
                    *span,
                )
            }
            ast::Expr::While {
                label,
                condition,
                body,
                span,
            } => {
                let hir_cond = self.lower_expr(exprs.get(*condition), exprs, stmts);
                self.scope.push();
                let hir_body = self.lower_block(body, exprs, stmts);
                self.scope.pop();

                // Find break value type
                let ty = self
                    .find_break_value_type_in_block(&hir_body)
                    .unwrap_or(Type::Unit);

                self.alloc_expr(
                    HirExprKind::While {
                        label: label.clone(),
                        condition: hir_cond,
                        body: hir_body,
                    },
                    ty,
                    *span,
                )
            }
            ast::Expr::Match {
                expr: match_expr,
                arms,
                span,
            } => {
                let hir_match_expr = self.lower_expr(exprs.get(*match_expr), exprs, stmts);
                let match_type = self.expr_arena.get(hir_match_expr).ty.clone();

                let mut hir_arms = Vec::new();
                for arm in arms {
                    self.scope.push();
                    let bindings = collect_pattern_bindings(self.ctx, &arm.pattern, &match_type);
                    for (name, ty) in bindings {
                        let var_id = self.alloc_var(name.clone(), ty, false, *span);
                        self.scope.define(name, var_id);
                    }
                    let hir_body = self.lower_expr(exprs.get(arm.body), exprs, stmts);
                    hir_arms.push(HirMatchArm {
                        pattern: arm.pattern.clone(),
                        body: hir_body,
                    });
                    self.scope.pop();
                }

                let ty = if let Some(first_arm) = hir_arms.first() {
                    self.expr_arena.get(first_arm.body).ty.clone()
                } else {
                    Type::Unit
                };

                self.alloc_expr(
                    HirExprKind::Match {
                        expr: hir_match_expr,
                        arms: hir_arms,
                    },
                    ty,
                    *span,
                )
            }
            ast::Expr::Block { block, span } => {
                self.scope.push();
                let hir_block = self.lower_block(block, exprs, stmts);
                self.scope.pop();

                let ty = if let Some(tail) = &hir_block.expr {
                    self.expr_arena.get(*tail).ty.clone()
                } else {
                    Type::Unit
                };

                self.alloc_expr(HirExprKind::Block(hir_block), ty, *span)
            }
            ast::Expr::FieldAccess {
                expr: inner,
                field,
                span,
            } => {
                let hir_inner = self.lower_expr(exprs.get(*inner), exprs, stmts);
                let base_type = self.expr_arena.get(hir_inner).ty.clone();

                if let Type::Named(ref name) = base_type
                    && let Some(struct_info) = self.ctx.structs.get(name)
                    && let Some(idx) = struct_info
                        .fields
                        .iter()
                        .position(|(fname, _)| fname == field)
                {
                    let field_ty = struct_info.fields[idx].1.clone();
                    return self.alloc_expr(
                        HirExprKind::FieldAccess {
                            expr: hir_inner,
                            struct_name: name.clone(),
                            field_index: idx,
                        },
                        field_ty,
                        *span,
                    );
                }
                if let Type::Named(ref name) = base_type {
                    self.error(
                        *span,
                        format!("構造体 '{}' にフィールド '{}' はありません", name, field),
                    );
                } else {
                    self.error(
                        *span,
                        "フィールドアクセスは構造体型にのみ使用できます".to_string(),
                    );
                }
                self.alloc_expr(
                    HirExprKind::FieldAccess {
                        expr: hir_inner,
                        struct_name: String::new(),
                        field_index: 0,
                    },
                    Type::Error,
                    *span,
                )
            }
            ast::Expr::TupleIndex {
                expr: inner,
                index,
                span,
            } => {
                let hir_inner = self.lower_expr(exprs.get(*inner), exprs, stmts);
                let base_type = self.expr_arena.get(hir_inner).ty.clone();

                let ty = if let Type::Tuple(ref types) = base_type {
                    if (*index as usize) < types.len() {
                        types[*index as usize].clone()
                    } else {
                        Type::Error
                    }
                } else {
                    Type::Error
                };

                self.alloc_expr(
                    HirExprKind::TupleIndex {
                        expr: hir_inner,
                        index: *index,
                    },
                    ty,
                    *span,
                )
            }
            ast::Expr::TupleExpr { elements, span } => {
                let hir_elems: Vec<HirExprId> = elements
                    .iter()
                    .map(|eid| self.lower_expr(exprs.get(*eid), exprs, stmts))
                    .collect();
                let types: Vec<Type> = hir_elems
                    .iter()
                    .map(|&eid| self.expr_arena.get(eid).ty.clone())
                    .collect();
                let ty = Type::Tuple(types);
                self.alloc_expr(HirExprKind::TupleExpr(hir_elems), ty, *span)
            }
            ast::Expr::StructExpr { name, fields, span } => {
                // Reorder fields to match struct definition order
                if let Some(struct_info) = self.ctx.structs.get(name) {
                    let mut ordered_fields = Vec::new();
                    for (def_name, _) in &struct_info.fields {
                        if let Some((_, eid)) = fields.iter().find(|(n, _)| n == def_name) {
                            let hir_expr = self.lower_expr(exprs.get(*eid), exprs, stmts);
                            ordered_fields.push(hir_expr);
                        } else {
                            self.error(
                                *span,
                                format!(
                                    "構造体 '{}' のフィールド '{}' が不足しています",
                                    name, def_name
                                ),
                            );
                            // Missing field - create a dummy
                            ordered_fields.push(self.alloc_expr(
                                HirExprKind::UnitLiteral,
                                Type::Error,
                                *span,
                            ));
                        }
                    }
                    self.alloc_expr(
                        HirExprKind::StructExpr {
                            name: name.clone(),
                            fields: ordered_fields,
                        },
                        Type::Named(name.clone()),
                        *span,
                    )
                } else {
                    // Lower fields anyway for error recovery
                    let hir_fields: Vec<HirExprId> = fields
                        .iter()
                        .map(|(_, eid)| self.lower_expr(exprs.get(*eid), exprs, stmts))
                        .collect();
                    self.alloc_expr(
                        HirExprKind::StructExpr {
                            name: name.clone(),
                            fields: hir_fields,
                        },
                        Type::Error,
                        *span,
                    )
                }
            }
            ast::Expr::EnumExpr {
                enum_name,
                variant,
                args,
                span,
            } => {
                let variant_index = if let Some(enum_info) = self.ctx.enums.get(enum_name) {
                    match enum_info.variants.iter().position(|(n, _)| n == variant) {
                        Some(idx) => idx,
                        None => {
                            self.error(
                                *span,
                                format!(
                                    "列挙型 '{}' にバリアント '{}' はありません",
                                    enum_name, variant
                                ),
                            );
                            0
                        }
                    }
                } else {
                    self.error(*span, format!("未定義の列挙型 '{}'", enum_name));
                    0
                };

                let hir_args = match args {
                    ast::EnumArgs::Unit => HirEnumArgs::Unit,
                    ast::EnumArgs::Tuple(arg_exprs) => {
                        let hir_exprs: Vec<HirExprId> = arg_exprs
                            .iter()
                            .map(|eid| self.lower_expr(exprs.get(*eid), exprs, stmts))
                            .collect();
                        HirEnumArgs::Tuple(hir_exprs)
                    }
                    ast::EnumArgs::Struct(field_exprs) => {
                        // Reorder to match variant definition
                        if let Some(enum_info) = self.ctx.enums.get(enum_name) {
                            if let Some((_, VariantInfo::Struct(def_fields))) =
                                enum_info.variants.iter().find(|(n, _)| n == variant)
                            {
                                let mut ordered = Vec::new();
                                for (def_name, _) in def_fields {
                                    if let Some((_, eid)) =
                                        field_exprs.iter().find(|(n, _)| n == def_name)
                                    {
                                        ordered.push(self.lower_expr(
                                            exprs.get(*eid),
                                            exprs,
                                            stmts,
                                        ));
                                    }
                                }
                                HirEnumArgs::Struct(ordered)
                            } else {
                                let hir: Vec<HirExprId> = field_exprs
                                    .iter()
                                    .map(|(_, eid)| self.lower_expr(exprs.get(*eid), exprs, stmts))
                                    .collect();
                                HirEnumArgs::Struct(hir)
                            }
                        } else {
                            let hir: Vec<HirExprId> = field_exprs
                                .iter()
                                .map(|(_, eid)| self.lower_expr(exprs.get(*eid), exprs, stmts))
                                .collect();
                            HirEnumArgs::Struct(hir)
                        }
                    }
                };

                self.alloc_expr(
                    HirExprKind::EnumExpr {
                        enum_name: enum_name.clone(),
                        variant_index,
                        args: hir_args,
                    },
                    Type::Named(enum_name.clone()),
                    *span,
                )
            }
        }
    }

    // --- break value type detection ---

    fn find_break_value_type_in_block(&self, block: &HirBlock) -> Option<Type> {
        for &stmt_id in &block.stmts {
            let stmt = self.stmt_arena.get(stmt_id);
            match stmt {
                HirStmt::Break {
                    value: Some(expr_id),
                    ..
                } => {
                    return Some(self.expr_arena.get(*expr_id).ty.clone());
                }
                HirStmt::Expr(expr_id) => {
                    if let Some(ty) = self.find_break_value_type_in_expr(*expr_id) {
                        return Some(ty);
                    }
                }
                _ => {}
            }
        }
        if let Some(expr_id) = block.expr
            && let Some(ty) = self.find_break_value_type_in_expr(expr_id)
        {
            return Some(ty);
        }
        None
    }

    fn find_break_value_type_in_expr(&self, expr_id: HirExprId) -> Option<Type> {
        let expr = self.expr_arena.get(expr_id);
        match &expr.kind {
            HirExprKind::If {
                then_block,
                else_block,
                ..
            } => {
                if let Some(ty) = self.find_break_value_type_in_block(then_block) {
                    return Some(ty);
                }
                match else_block {
                    Some(HirElseClause::ElseBlock(block)) => {
                        self.find_break_value_type_in_block(block)
                    }
                    Some(HirElseClause::ElseIf(inner)) => {
                        self.find_break_value_type_in_expr(*inner)
                    }
                    None => None,
                }
            }
            HirExprKind::Block(block) => self.find_break_value_type_in_block(block),
            _ => None,
        }
    }
}

// Need to implement Clone for Pattern to allow cloning in match arms
impl Clone for ast::Pattern {
    fn clone(&self) -> Self {
        match self {
            ast::Pattern::IntLiteral(v) => ast::Pattern::IntLiteral(*v),
            ast::Pattern::BoolLiteral(v) => ast::Pattern::BoolLiteral(*v),
            ast::Pattern::Range { start, end } => ast::Pattern::Range {
                start: *start,
                end: *end,
            },
            ast::Pattern::Wildcard => ast::Pattern::Wildcard,
            ast::Pattern::Binding(s) => ast::Pattern::Binding(s.clone()),
            ast::Pattern::Tuple(pats) => ast::Pattern::Tuple(pats.clone()),
            ast::Pattern::Struct { name, fields } => ast::Pattern::Struct {
                name: name.clone(),
                fields: fields.clone(),
            },
            ast::Pattern::Enum {
                enum_name,
                variant,
                args,
            } => ast::Pattern::Enum {
                enum_name: enum_name.clone(),
                variant: variant.clone(),
                args: args.clone(),
            },
        }
    }
}

impl Clone for ast::EnumPatternArgs {
    fn clone(&self) -> Self {
        match self {
            ast::EnumPatternArgs::Unit => ast::EnumPatternArgs::Unit,
            ast::EnumPatternArgs::Tuple(pats) => ast::EnumPatternArgs::Tuple(pats.clone()),
            ast::EnumPatternArgs::Struct(fields) => ast::EnumPatternArgs::Struct(fields.clone()),
        }
    }
}

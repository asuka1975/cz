use crate::ast::*;
use crate::token::{FloatSuffix, IntSuffix};
use crate::type_context::{TypeContext, TypeMap, collect_pattern_bindings};
use std::collections::HashMap;

pub struct TypeResolver<'a> {
    ctx: &'a TypeContext,
    scopes: Vec<HashMap<String, Type>>,
    type_map: TypeMap,
}

impl<'a> TypeResolver<'a> {
    pub fn new(ctx: &'a TypeContext) -> Self {
        Self {
            ctx,
            scopes: Vec::new(),
            type_map: TypeMap::new(),
        }
    }

    pub fn resolve(mut self, program: &Program) -> TypeMap {
        for func in &program.functions {
            self.resolve_function(func);
        }
        self.type_map
    }

    fn resolve_function(&mut self, func: &FunctionDef) {
        self.push_scope();
        for param in &func.params {
            self.define_var(&param.name, param.param_type.clone());
        }
        self.resolve_block(&func.body);
        self.pop_scope();
    }

    fn resolve_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.resolve_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            self.resolve_expr(expr);
        }
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                name,
                var_type,
                init,
                ..
            } => {
                self.resolve_expr(init);
                let resolved_type = if let Some(declared_type) = var_type {
                    declared_type.clone()
                } else {
                    self.type_map.get(init).clone()
                };
                self.define_var(name, resolved_type);
            }
            Stmt::Return { value, .. } => {
                if let Some(val) = value {
                    self.resolve_expr(val);
                }
            }
            Stmt::Break { value, .. } => {
                if let Some(val) = value {
                    self.resolve_expr(val);
                }
            }
            Stmt::Continue { .. } => {}
            Stmt::Expr(expr) => {
                self.resolve_expr(expr);
            }
        }
    }

    fn resolve_expr(&mut self, expr: &Expr) {
        // Depth-first: resolve children first
        match expr {
            Expr::IntegerLiteral { .. }
            | Expr::FloatLiteral { .. }
            | Expr::BoolLiteral(_)
            | Expr::UnitLiteral
            | Expr::Identifier(_, _) => {}
            Expr::BinaryOp { left, right, .. } => {
                self.resolve_expr(left);
                self.resolve_expr(right);
            }
            Expr::UnaryOp { operand, .. } => {
                self.resolve_expr(operand);
            }
            Expr::Cast { expr: inner, .. } => {
                self.resolve_expr(inner);
            }
            Expr::Call { args, .. } => {
                for arg in args {
                    self.resolve_expr(arg);
                }
            }
            Expr::Assign { value, .. } => {
                self.resolve_expr(value);
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.resolve_expr(condition);
                self.push_scope();
                self.resolve_block(then_block);
                self.pop_scope();
                if let Some(else_clause) = else_block {
                    match else_clause.as_ref() {
                        ElseClause::ElseBlock(block) => {
                            self.push_scope();
                            self.resolve_block(block);
                            self.pop_scope();
                        }
                        ElseClause::ElseIf(expr) => {
                            self.resolve_expr(expr);
                        }
                    }
                }
            }
            Expr::While {
                condition, body, ..
            } => {
                self.resolve_expr(condition);
                self.push_scope();
                self.resolve_block(body);
                self.pop_scope();
            }
            Expr::Match {
                expr: match_expr,
                arms,
            } => {
                self.resolve_expr(match_expr);
                let match_type = self.type_map.get(match_expr).clone();
                for arm in arms {
                    self.push_scope();
                    self.bind_pattern_vars(&arm.pattern, &match_type);
                    self.resolve_expr(&arm.body);
                    self.pop_scope();
                }
            }
            Expr::Block(block) => {
                self.push_scope();
                self.resolve_block(block);
                self.pop_scope();
            }
            Expr::FieldAccess { expr: inner, .. } => {
                self.resolve_expr(inner);
            }
            Expr::TupleIndex { expr: inner, .. } => {
                self.resolve_expr(inner);
            }
            Expr::TupleExpr(elems) => {
                for elem in elems {
                    self.resolve_expr(elem);
                }
            }
            Expr::StructExpr { fields, .. } => {
                for (_, field_expr) in fields {
                    self.resolve_expr(field_expr);
                }
            }
            Expr::EnumExpr { args, .. } => match args {
                EnumArgs::Unit => {}
                EnumArgs::Tuple(exprs) => {
                    for expr in exprs {
                        self.resolve_expr(expr);
                    }
                }
                EnumArgs::Struct(fields) => {
                    for (_, expr) in fields {
                        self.resolve_expr(expr);
                    }
                }
            },
        }

        // Now compute this expression's type
        let ty = self.compute_type(expr);
        self.type_map.set(expr, ty);
    }

    fn compute_type(&self, expr: &Expr) -> Type {
        match expr {
            Expr::IntegerLiteral { suffix, .. } => match suffix {
                Some(IntSuffix::I8) => Type::I8,
                Some(IntSuffix::I16) => Type::I16,
                Some(IntSuffix::I32) => Type::I32,
                Some(IntSuffix::I64) => Type::I64,
                None => Type::I32,
            },
            Expr::FloatLiteral { suffix, .. } => match suffix {
                Some(FloatSuffix::F32) => Type::F32,
                Some(FloatSuffix::F64) => Type::F64,
                None => Type::F64,
            },
            Expr::BoolLiteral(_) => Type::Bool,
            Expr::UnitLiteral => Type::Unit,
            Expr::Identifier(name, _) => {
                if let Some(ty) = self.find_var(name) {
                    ty.clone()
                } else {
                    Type::Error
                }
            }
            Expr::BinaryOp { op, left, .. } => match op {
                BinOp::Eq
                | BinOp::NotEq
                | BinOp::Lt
                | BinOp::Gt
                | BinOp::LtEq
                | BinOp::GtEq
                | BinOp::And
                | BinOp::Or => Type::Bool,
                _ => self.type_map.get(left).clone(),
            },
            Expr::UnaryOp { op, operand } => match op {
                UnaryOp::Neg => self.type_map.get(operand).clone(),
                UnaryOp::Not => Type::Bool,
            },
            Expr::Cast { target_type, .. } => target_type.clone(),
            Expr::Call { name, .. } => {
                if let Some(func_info) = self.ctx.functions.get(name) {
                    func_info.return_type.clone()
                } else {
                    Type::Error
                }
            }
            Expr::Assign { .. } => Type::Unit,
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                if else_block.is_some() {
                    if let Some(tail) = &then_block.expr {
                        self.type_map.get(tail).clone()
                    } else {
                        Type::Unit
                    }
                } else {
                    Type::Unit
                }
            }
            Expr::While { body, .. } => {
                if let Some(ty) = self.find_break_value_type_in_block(body) {
                    ty
                } else {
                    Type::Unit
                }
            }
            Expr::Match { arms, .. } => {
                if let Some(first_arm) = arms.first() {
                    self.type_map.get(&first_arm.body).clone()
                } else {
                    Type::Unit
                }
            }
            Expr::Block(block) => {
                if let Some(tail) = &block.expr {
                    self.type_map.get(tail).clone()
                } else {
                    Type::Unit
                }
            }
            Expr::FieldAccess { expr: inner, field } => {
                let base_type = self.type_map.get(inner);
                if let Type::Named(name) = base_type
                    && let Some(struct_info) = self.ctx.structs.get(name)
                {
                    for (fname, ftype) in &struct_info.fields {
                        if fname == field {
                            return ftype.clone();
                        }
                    }
                }
                Type::Error
            }
            Expr::TupleIndex { expr: inner, index } => {
                let base_type = self.type_map.get(inner);
                if let Type::Tuple(types) = base_type
                    && (*index as usize) < types.len()
                {
                    return types[*index as usize].clone();
                }
                Type::Error
            }
            Expr::TupleExpr(elems) => {
                let types: Vec<Type> = elems.iter().map(|e| self.type_map.get(e).clone()).collect();
                Type::Tuple(types)
            }
            Expr::StructExpr { name, .. } => Type::Named(name.clone()),
            Expr::EnumExpr { enum_name, .. } => Type::Named(enum_name.clone()),
        }
    }

    fn find_break_value_type_in_block(&self, block: &Block) -> Option<Type> {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Break {
                    value: Some(expr), ..
                } => {
                    return Some(self.type_map.get(expr).clone());
                }
                Stmt::Expr(expr) => {
                    if let Some(ty) = self.find_break_value_type_in_expr(expr) {
                        return Some(ty);
                    }
                }
                _ => {}
            }
        }
        if let Some(expr) = &block.expr
            && let Some(ty) = self.find_break_value_type_in_expr(expr)
        {
            return Some(ty);
        }
        None
    }

    fn find_break_value_type_in_expr(&self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::If {
                then_block,
                else_block,
                ..
            } => {
                if let Some(ty) = self.find_break_value_type_in_block(then_block) {
                    return Some(ty);
                }
                if let Some(else_clause) = else_block {
                    match else_clause.as_ref() {
                        ElseClause::ElseBlock(block) => {
                            if let Some(ty) = self.find_break_value_type_in_block(block) {
                                return Some(ty);
                            }
                        }
                        ElseClause::ElseIf(inner) => {
                            if let Some(ty) = self.find_break_value_type_in_expr(inner) {
                                return Some(ty);
                            }
                        }
                    }
                }
                None
            }
            Expr::Block(block) => self.find_break_value_type_in_block(block),
            // Don't recurse into inner while loops (their breaks belong to them)
            _ => None,
        }
    }

    fn bind_pattern_vars(&mut self, pattern: &Pattern, ty: &Type) {
        let bindings = collect_pattern_bindings(self.ctx, pattern, ty);
        for (name, var_type) in bindings {
            self.define_var(&name, var_type);
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define_var(&mut self, name: &str, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), ty);
        }
    }

    fn find_var(&self, name: &str) -> Option<&Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty);
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

    fn resolve_types(source: &str) -> (crate::ast::Program, TypeMap) {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().unwrap();
        let mut parser = Parser::new(tokens);
        let program = parser.parse_program().unwrap();
        let type_ctx = TypeContext::build(&program).unwrap();
        let type_map = TypeResolver::new(&type_ctx).resolve(&program);
        (program, type_map)
    }

    #[test]
    fn integer_literal_type() {
        let (program, type_map) = resolve_types("fn main() -> i32 { 42 }");
        let expr = program.functions[0].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr), &Type::I32);
    }

    #[test]
    fn integer_literal_with_suffix() {
        let (program, type_map) = resolve_types("fn main() -> i64 { 42i64 }");
        let expr = program.functions[0].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr), &Type::I64);
    }

    #[test]
    fn float_literal_type() {
        let (program, type_map) = resolve_types("fn main() -> i32 { let x: f64 = 3.14; 0 }");
        // The init expr of the let statement
        if let Stmt::Let { init, .. } = &program.functions[0].body.stmts[0] {
            assert_eq!(type_map.get(init), &Type::F64);
        } else {
            panic!("expected let statement");
        }
    }

    #[test]
    fn bool_literal_type() {
        let (program, type_map) = resolve_types("fn main() -> i32 { let b: bool = true; 0 }");
        if let Stmt::Let { init, .. } = &program.functions[0].body.stmts[0] {
            assert_eq!(type_map.get(init), &Type::Bool);
        } else {
            panic!("expected let statement");
        }
    }

    #[test]
    fn variable_type_tracking() {
        let (program, type_map) = resolve_types("fn main() -> i64 { let x: i64 = 10i64; x }");
        let expr = program.functions[0].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr.as_ref()), &Type::I64);
    }

    #[test]
    fn binary_op_arithmetic_type() {
        let (program, type_map) =
            resolve_types("fn main() -> i32 { let a: i32 = 1; let b: i32 = 2; a + b }");
        let expr = program.functions[0].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr.as_ref()), &Type::I32);
    }

    #[test]
    fn binary_op_comparison_type() {
        let (program, type_map) =
            resolve_types("fn main() -> i32 { let a: i32 = 1; let b: bool = a == 1; 0 }");
        if let Stmt::Let { init, .. } = &program.functions[0].body.stmts[1] {
            assert_eq!(type_map.get(init), &Type::Bool);
        } else {
            panic!("expected let statement");
        }
    }

    #[test]
    fn function_call_return_type() {
        let (program, type_map) = resolve_types(
            "fn foo() -> i64 { 42i64 }
             fn main() -> i64 { foo() }",
        );
        let expr = program.functions[1].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr.as_ref()), &Type::I64);
    }

    #[test]
    fn struct_type() {
        let (program, type_map) = resolve_types(
            "struct Point { x: i32, y: i32 }
             fn main() -> i32 { let p = Point { x: 1, y: 2 }; p.x }",
        );
        // p.x should be i32
        let expr = program.functions[0].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr.as_ref()), &Type::I32);
    }

    #[test]
    fn tuple_type() {
        let (program, type_map) =
            resolve_types("fn main() -> i32 { let t: (i32, bool) = (1, true); t.0 }");
        let expr = program.functions[0].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr.as_ref()), &Type::I32);
    }

    #[test]
    fn enum_type() {
        let (program, type_map) = resolve_types(
            "enum Color { Red, Green, Blue }
             fn main() -> i32 { let c = Color::Red; 0 }",
        );
        if let Stmt::Let { init, .. } = &program.functions[0].body.stmts[0] {
            assert_eq!(type_map.get(init), &Type::Named("Color".to_string()));
        } else {
            panic!("expected let statement");
        }
    }

    #[test]
    fn cast_type() {
        let (program, type_map) = resolve_types("fn main() -> i64 { let x: i32 = 42; x as i64 }");
        let expr = program.functions[0].body.expr.as_ref().unwrap();
        assert_eq!(type_map.get(expr.as_ref()), &Type::I64);
    }
}

use crate::arena::Arena;
use crate::diagnostics::Span;
use crate::hir::types::{
    EnumInfo, FuncInfo, StructInfo, TypeContext, VariantInfo, collect_pattern_bindings,
    substitute_type_map,
};
use crate::hir::*;
use crate::syntax::ast::{EnumPatternArgs, Pattern};
use std::collections::HashMap;

/// 型引数の組み合わせ (例: [Type::I32, Type::Bool])。
type TypeArgs = Vec<Type>;

/// 単相化結果。
pub struct MonomorphizeResult {
    pub program: HirProgram,
    pub expr_arena: Arena<HirExpr>,
    pub stmt_arena: Arena<HirStmt>,
    pub vars: Vec<VarInfo>,
    pub func_names: Vec<String>,
    pub ctx: TypeContext,
}

/// ジェネリクスを具体化するキー。
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct InstKey {
    name: String,
    type_args: TypeArgs,
}

fn mangle_name(base: &str, type_args: &[Type]) -> String {
    if type_args.is_empty() {
        base.to_string()
    } else {
        let suffix: Vec<String> = type_args.iter().map(type_to_mangled).collect();
        format!("{}__{}", base, suffix.join("__"))
    }
}

fn type_to_mangled(ty: &Type) -> String {
    match ty {
        Type::I8 => "i8".to_string(),
        Type::I16 => "i16".to_string(),
        Type::I32 => "i32".to_string(),
        Type::I64 => "i64".to_string(),
        Type::F32 => "f32".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "unit".to_string(),
        Type::Named(n) => n.clone(),
        Type::Tuple(ts) => {
            let inner: Vec<String> = ts.iter().map(type_to_mangled).collect();
            format!("T{}", inner.join("_"))
        }
        Type::Generic(name, args) => {
            let inner: Vec<String> = args.iter().map(type_to_mangled).collect();
            format!("{}__{}", name, inner.join("__"))
        }
        Type::TypeParam(name) => format!("TP_{}", name),
        Type::Error => "error".to_string(),
    }
}

fn has_unresolved_type_params(ty: &Type, ctx: &TypeContext) -> bool {
    match ty {
        Type::TypeParam(_) => true,
        Type::Named(n) => is_likely_type_param(n, ctx),
        Type::Generic(_, args) => args.iter().any(|a| has_unresolved_type_params(a, ctx)),
        Type::Tuple(types) => types.iter().any(|t| has_unresolved_type_params(t, ctx)),
        _ => false,
    }
}

/// Determine if a Named type is likely a type parameter reference rather than a concrete type.
fn is_likely_type_param(name: &str, ctx: &TypeContext) -> bool {
    // Known types are not type parameters
    if ctx.structs.contains_key(name)
        || ctx.enums.contains_key(name)
        || ctx.type_aliases.contains_key(name)
    {
        return false;
    }
    // Primitive types
    if matches!(
        name,
        "i8" | "i16" | "i32" | "i64" | "f32" | "f64" | "bool" | "unit"
    ) {
        return false;
    }
    // Mangled names (contain "__") are monomorphized types, not type params
    if name.contains("__") {
        // But check if any part after __ is itself a type param (e.g., "Maybe__TP_T")
        return name.contains("TP_");
    }
    // At this point, it's a Named type that isn't known anywhere.
    // Single uppercase letters are almost certainly type params.
    // Multi-char names could be forward references, but in this context they're likely type params.
    true
}

fn parse_mangled_type(s: &str) -> Type {
    match s {
        "i8" => Type::I8,
        "i16" => Type::I16,
        "i32" => Type::I32,
        "i64" => Type::I64,
        "f32" => Type::F32,
        "f64" => Type::F64,
        "bool" => Type::Bool,
        "unit" => Type::Unit,
        other => Type::Named(other.to_string()),
    }
}

/// Parse mangled type args from a suffix string like "i32__bool" or "Pair__i32__bool".
/// Uses the TypeContext to determine how many type params known types take,
/// so that nested mangled names are parsed correctly.
/// E.g., for suffix "Pair__i32__bool" with expected_count=1, we parse it as [Named("Pair__i32__bool")].
/// For suffix "i32__bool" with expected_count=2, we parse it as [I32, Bool].
fn parse_mangled_type_args(suffix: &str, expected_count: usize, ctx: &TypeContext) -> Vec<Type> {
    let parts: Vec<&str> = suffix.split("__").collect();
    if parts.len() == expected_count {
        // Simple case: each part is one type arg
        return parts.iter().map(|p| parse_mangled_type(p)).collect();
    }

    // Complex case: some parts are nested mangled types.
    // Greedy approach: try to consume parts matching known types with their type params.
    let mut result = Vec::new();
    let mut i = 0;
    while i < parts.len() && result.len() < expected_count {
        let part = parts[i];
        // Check if this part is a known generic struct or enum
        let type_param_count = ctx
            .structs
            .get(part)
            .map(|info| info.type_params.len())
            .or_else(|| ctx.enums.get(part).map(|info| info.type_params.len()));

        if let Some(n_params) = type_param_count
            && n_params > 0
            && i + n_params < parts.len()
        {
            // This is a nested generic type - recursively parse its args
            let nested_suffix_parts = &parts[i + 1..];
            let nested_args =
                parse_mangled_type_args(&nested_suffix_parts.join("__"), n_params, ctx);
            let mangled = mangle_name(part, &nested_args);
            result.push(Type::Named(mangled));
            // Skip the parts consumed by this nested type
            // The nested type consumed: 1 (name) + however many parts its args took
            i += 1 + count_mangled_parts(&nested_args);
            continue;
        }
        // Simple type
        result.push(parse_mangled_type(part));
        i += 1;
    }
    result
}

/// Count how many `__`-separated parts a list of type args would consume in a mangled name.
fn count_mangled_parts(type_args: &[Type]) -> usize {
    let mut count = 0;
    for arg in type_args {
        match arg {
            Type::Named(name) => {
                // Check if this is itself a mangled type (contains __)
                if name.contains("__") {
                    count += name.split("__").count();
                } else {
                    count += 1;
                }
            }
            _ => {
                count += 1;
            }
        }
    }
    count
}

/// HIR 式の構造体用データ (borrow 回避のためコピー)。
struct ExprData {
    kind: ExprKindData,
    ty: Type,
    span: Span,
}

#[allow(clippy::enum_variant_names)]
enum ExprKindData {
    IntegerLiteral {
        value: i64,
    },
    FloatLiteral {
        value: f64,
    },
    BoolLiteral(bool),
    UnitLiteral,
    Var(VarId),
    BinaryOp {
        op: crate::syntax::ast::BinOp,
        left: HirExprId,
        right: HirExprId,
    },
    UnaryOp {
        op: crate::syntax::ast::UnaryOp,
        operand: HirExprId,
    },
    Cast {
        expr: HirExprId,
        target_type: Type,
    },
    Call {
        func: FuncId,
        args: Vec<HirExprId>,
    },
    Assign {
        var: VarId,
        value: HirExprId,
    },
    If {
        condition: HirExprId,
        then_block: HirBlock,
        else_block: Option<HirElseClause>,
    },
    While {
        label: Option<String>,
        condition: HirExprId,
        body: HirBlock,
    },
    Match {
        expr: HirExprId,
        arms: Vec<(Pattern, HirExprId)>,
    },
    Block(HirBlock),
    FieldAccess {
        expr: HirExprId,
        struct_name: String,
        field_index: usize,
    },
    TupleIndex {
        expr: HirExprId,
        index: u32,
    },
    TupleExpr(Vec<HirExprId>),
    StructExpr {
        name: String,
        fields: Vec<HirExprId>,
    },
    EnumExpr {
        enum_name: String,
        variant_index: usize,
        args: EnumArgsData,
    },
}

enum EnumArgsData {
    Unit,
    Tuple(Vec<HirExprId>),
    Struct(Vec<HirExprId>),
}

/// HIR 文のデータ (borrow 回避)。
enum StmtData {
    Let {
        var: VarId,
        init: HirExprId,
        span: Span,
    },
    Return {
        value: Option<HirExprId>,
        span: Span,
    },
    Break {
        label: Option<String>,
        value: Option<HirExprId>,
        span: Span,
    },
    Continue {
        label: Option<String>,
        span: Span,
    },
    Expr(HirExprId),
}

fn extract_expr_data(expr: &HirExpr) -> ExprData {
    let kind = match &expr.kind {
        HirExprKind::IntegerLiteral { value } => ExprKindData::IntegerLiteral { value: *value },
        HirExprKind::FloatLiteral { value } => ExprKindData::FloatLiteral { value: *value },
        HirExprKind::BoolLiteral(b) => ExprKindData::BoolLiteral(*b),
        HirExprKind::UnitLiteral => ExprKindData::UnitLiteral,
        HirExprKind::Var(v) => ExprKindData::Var(*v),
        HirExprKind::BinaryOp { op, left, right } => ExprKindData::BinaryOp {
            op: *op,
            left: *left,
            right: *right,
        },
        HirExprKind::UnaryOp { op, operand } => ExprKindData::UnaryOp {
            op: *op,
            operand: *operand,
        },
        HirExprKind::Cast { expr, target_type } => ExprKindData::Cast {
            expr: *expr,
            target_type: target_type.clone(),
        },
        HirExprKind::Call { func, args } => ExprKindData::Call {
            func: *func,
            args: args.clone(),
        },
        HirExprKind::Assign { var, value } => ExprKindData::Assign {
            var: *var,
            value: *value,
        },
        HirExprKind::If {
            condition,
            then_block,
            else_block,
        } => ExprKindData::If {
            condition: *condition,
            then_block: clone_hir_block(then_block),
            else_block: else_block.as_ref().map(|ec| match ec {
                HirElseClause::ElseBlock(b) => HirElseClause::ElseBlock(clone_hir_block(b)),
                HirElseClause::ElseIf(id) => HirElseClause::ElseIf(*id),
            }),
        },
        HirExprKind::While {
            label,
            condition,
            body,
        } => ExprKindData::While {
            label: label.clone(),
            condition: *condition,
            body: clone_hir_block(body),
        },
        HirExprKind::Match { expr, arms } => ExprKindData::Match {
            expr: *expr,
            arms: arms.iter().map(|a| (a.pattern.clone(), a.body)).collect(),
        },
        HirExprKind::Block(block) => ExprKindData::Block(clone_hir_block(block)),
        HirExprKind::FieldAccess {
            expr,
            struct_name,
            field_index,
        } => ExprKindData::FieldAccess {
            expr: *expr,
            struct_name: struct_name.clone(),
            field_index: *field_index,
        },
        HirExprKind::TupleIndex { expr, index } => ExprKindData::TupleIndex {
            expr: *expr,
            index: *index,
        },
        HirExprKind::TupleExpr(elems) => ExprKindData::TupleExpr(elems.clone()),
        HirExprKind::StructExpr { name, fields } => ExprKindData::StructExpr {
            name: name.clone(),
            fields: fields.clone(),
        },
        HirExprKind::EnumExpr {
            enum_name,
            variant_index,
            args,
        } => ExprKindData::EnumExpr {
            enum_name: enum_name.clone(),
            variant_index: *variant_index,
            args: match args {
                HirEnumArgs::Unit => EnumArgsData::Unit,
                HirEnumArgs::Tuple(ids) => EnumArgsData::Tuple(ids.clone()),
                HirEnumArgs::Struct(ids) => EnumArgsData::Struct(ids.clone()),
            },
        },
    };
    ExprData {
        kind,
        ty: expr.ty.clone(),
        span: expr.span,
    }
}

fn extract_stmt_data(stmt: &HirStmt) -> StmtData {
    match stmt {
        HirStmt::Let { var, init, span } => StmtData::Let {
            var: *var,
            init: *init,
            span: *span,
        },
        HirStmt::Return { value, span } => StmtData::Return {
            value: *value,
            span: *span,
        },
        HirStmt::Break { label, value, span } => StmtData::Break {
            label: label.clone(),
            value: *value,
            span: *span,
        },
        HirStmt::Continue { label, span } => StmtData::Continue {
            label: label.clone(),
            span: *span,
        },
        HirStmt::Expr(eid) => StmtData::Expr(*eid),
    }
}

fn clone_hir_block(block: &HirBlock) -> HirBlock {
    HirBlock {
        stmts: block.stmts.clone(),
        expr: block.expr,
    }
}

pub struct Monomorphizer {
    old_exprs: Arena<HirExpr>,
    old_stmts: Arena<HirStmt>,
    old_vars: Vec<VarInfo>,
    old_func_names: Vec<String>,
    ctx: TypeContext,

    new_exprs: Arena<HirExpr>,
    new_stmts: Arena<HirStmt>,
    new_vars: Vec<VarInfo>,
    new_func_names: Vec<String>,
    new_func_ids: HashMap<String, FuncId>,
    new_functions: Vec<HirFunctionDef>,

    generic_func_defs: HashMap<String, HirFunctionDef>,
    instantiated_funcs: HashMap<InstKey, String>,
    instantiated_structs: HashMap<InstKey, String>,
    instantiated_enums: HashMap<InstKey, String>,

    new_struct_order: Vec<String>,
    new_enum_order: Vec<String>,

    var_map: HashMap<u32, VarId>,
    current_type_map: HashMap<String, Type>,
    func_id_map: HashMap<u32, FuncId>,

    errors: Vec<String>,
    source: String,
}

impl Monomorphizer {
    pub fn new(
        expr_arena: Arena<HirExpr>,
        stmt_arena: Arena<HirStmt>,
        vars: Vec<VarInfo>,
        func_names: Vec<String>,
        ctx: TypeContext,
        source: &str,
    ) -> Self {
        Self {
            old_exprs: expr_arena,
            old_stmts: stmt_arena,
            old_vars: vars,
            old_func_names: func_names,
            ctx,
            new_exprs: Arena::new(),
            new_stmts: Arena::new(),
            new_vars: Vec::new(),
            new_func_names: Vec::new(),
            new_func_ids: HashMap::new(),
            new_functions: Vec::new(),
            generic_func_defs: HashMap::new(),
            instantiated_funcs: HashMap::new(),
            instantiated_structs: HashMap::new(),
            instantiated_enums: HashMap::new(),
            new_struct_order: Vec::new(),
            new_enum_order: Vec::new(),
            var_map: HashMap::new(),
            current_type_map: HashMap::new(),
            func_id_map: HashMap::new(),
            errors: Vec::new(),
            source: source.to_string(),
        }
    }

    pub fn run(mut self, program: HirProgram) -> Result<MonomorphizeResult, Vec<String>> {
        // Phase 1: Separate generic and non-generic functions
        let mut non_generic_funcs = Vec::new();
        for func in program.functions {
            let name = &func.name;
            if let Some(info) = self.ctx.functions.get(name)
                && !info.type_params.is_empty()
            {
                self.generic_func_defs.insert(name.clone(), func);
                continue;
            }
            non_generic_funcs.push(func);
        }

        // Phase 2: Register non-generic struct/enum types
        for name in &program.struct_order {
            if let Some(info) = self.ctx.structs.get(name)
                && info.type_params.is_empty()
            {
                self.new_struct_order.push(name.clone());
            }
        }
        for name in &program.enum_order {
            if let Some(info) = self.ctx.enums.get(name)
                && info.type_params.is_empty()
            {
                self.new_enum_order.push(name.clone());
            }
        }

        // Phase 3: Register non-generic function ids
        for (name, info) in &self.ctx.functions {
            if info.type_params.is_empty() {
                let func_id = FuncId(self.new_func_names.len() as u32);
                self.new_func_names.push(name.clone());
                self.new_func_ids.insert(name.clone(), func_id);
            }
        }
        for (i, old_name) in self.old_func_names.iter().enumerate() {
            if let Some(&new_id) = self.new_func_ids.get(old_name) {
                self.func_id_map.insert(i as u32, new_id);
            }
        }

        // Phase 4: Process non-generic functions
        for func in non_generic_funcs {
            self.var_map.clear();
            self.current_type_map.clear();
            let new_func = self.clone_function(&func);
            self.new_functions.push(new_func);
        }

        // Phase 5: Process deferred instantiations (worklist)
        let mut processed = std::collections::HashSet::new();
        loop {
            let pending: Vec<(InstKey, String)> = self
                .instantiated_funcs
                .iter()
                .filter(|(k, _)| !processed.contains(*k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            if pending.is_empty() {
                break;
            }
            for (key, mangled_name) in pending {
                processed.insert(key.clone());
                self.instantiate_generic_function(&key.name, &key.type_args, &mangled_name);
            }
        }

        if !self.errors.is_empty() {
            return Err(self.errors);
        }

        // Build output TypeContext
        let mut new_ctx = TypeContext {
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            type_aliases: HashMap::new(),
        };
        for name in &self.new_struct_order {
            if let Some(info) = self.ctx.structs.get(name) {
                new_ctx.structs.insert(name.clone(), info.clone());
            }
        }
        for name in &self.new_enum_order {
            if let Some(info) = self.ctx.enums.get(name) {
                new_ctx.enums.insert(name.clone(), info.clone());
            }
        }
        for func in &self.new_functions {
            let param_types: Vec<Type> = func
                .params
                .iter()
                .map(|&vid| self.new_vars[vid.0 as usize].ty.clone())
                .collect();
            new_ctx.functions.insert(
                func.name.clone(),
                FuncInfo {
                    name: func.name.clone(),
                    type_params: Vec::new(),
                    param_count: func.params.len(),
                    param_types,
                    return_type: func.return_type.clone(),
                },
            );
        }
        for (name, info) in &self.ctx.functions {
            if !new_ctx.functions.contains_key(name) && info.type_params.is_empty() {
                new_ctx.functions.insert(name.clone(), info.clone());
            }
        }

        Ok(MonomorphizeResult {
            program: HirProgram {
                functions: self.new_functions,
                struct_order: self.new_struct_order,
                enum_order: self.new_enum_order,
            },
            expr_arena: self.new_exprs,
            stmt_arena: self.new_stmts,
            vars: self.new_vars,
            func_names: self.new_func_names,
            ctx: new_ctx,
        })
    }

    // --- Expression type fixup ---

    /// Fix up an expression's type to a concrete type, also updating inner names
    /// (e.g., EnumExpr's enum_name or StructExpr's name).
    fn fixup_expr_type(&mut self, expr_id: HirExprId, concrete_ty: &Type) {
        // Extract info about the expression first to avoid borrow conflicts
        let expr_kind_info = match &self.new_exprs.get(expr_id).kind {
            HirExprKind::EnumExpr { enum_name, .. } => Some(("enum", enum_name.clone())),
            HirExprKind::StructExpr { name, .. } => Some(("struct", name.clone())),
            _ => None,
        };

        // Set the type
        self.new_exprs.get_mut(expr_id).ty = concrete_ty.clone();

        // Fix up inner names and ensure types are instantiated
        if let Type::Named(concrete_name) = concrete_ty
            && let Some((kind, orig_name)) = expr_kind_info
        {
            match kind {
                "enum" => {
                    self.ensure_enum_instantiated(concrete_name, &orig_name);
                    if let HirExprKind::EnumExpr {
                        ref mut enum_name, ..
                    } = self.new_exprs.get_mut(expr_id).kind
                    {
                        *enum_name = concrete_name.clone();
                    }
                }
                "struct" => {
                    self.ensure_struct_instantiated(concrete_name, &orig_name);
                    if let HirExprKind::StructExpr { ref mut name, .. } =
                        self.new_exprs.get_mut(expr_id).kind
                    {
                        *name = concrete_name.clone();
                    }
                }
                _ => {}
            }
        }
    }

    // --- Type resolution ---

    fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::TypeParam(name) | Type::Named(name)
                if self.current_type_map.contains_key(name) =>
            {
                self.current_type_map[name].clone()
            }
            Type::Generic(name, args) => {
                let resolved_args: Vec<Type> = args.iter().map(|a| self.resolve_type(a)).collect();
                if let Some(alias_info) = self.ctx.type_aliases.get(name) {
                    let mut mapping = HashMap::new();
                    for (param, arg) in alias_info.type_params.iter().zip(resolved_args.iter()) {
                        mapping.insert(param.clone(), arg.clone());
                    }
                    let result = substitute_type_map(&alias_info.aliased_type, &mapping);
                    return self.resolve_type(&result);
                }
                if self.ctx.structs.contains_key(name) || self.ctx.enums.contains_key(name) {
                    // Only mangle when all type args are concrete
                    if resolved_args
                        .iter()
                        .any(|a| has_unresolved_type_params(a, &self.ctx))
                    {
                        Type::Generic(name.clone(), resolved_args)
                    } else {
                        let mangled = mangle_name(name, &resolved_args);
                        Type::Named(mangled)
                    }
                } else {
                    Type::Generic(name.clone(), resolved_args)
                }
            }
            Type::Named(name) => {
                if let Some(alias_info) = self.ctx.type_aliases.get(name)
                    && alias_info.type_params.is_empty()
                {
                    return self.resolve_type(&alias_info.aliased_type);
                }
                ty.clone()
            }
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| self.resolve_type(t)).collect()),
            _ => ty.clone(),
        }
    }

    // --- Clone function ---

    fn clone_function(&mut self, func: &HirFunctionDef) -> HirFunctionDef {
        let new_func_id = self.new_func_ids[&func.name];
        let mut new_params = Vec::new();
        for &old_var_id in &func.params {
            let old_var = &self.old_vars[old_var_id.0 as usize];
            let new_ty = self.resolve_type(&old_var.ty);
            let new_var_id = VarId(self.new_vars.len() as u32);
            self.new_vars.push(VarInfo {
                name: old_var.name.clone(),
                ty: new_ty,
                mutable: old_var.mutable,
                span: old_var.span,
            });
            self.var_map.insert(old_var_id.0, new_var_id);
            new_params.push(new_var_id);
        }
        let new_body = self.clone_block(&func.body);
        let new_return_type = self.resolve_type(&func.return_type);
        HirFunctionDef {
            func_id: new_func_id,
            name: func.name.clone(),
            params: new_params,
            return_type: new_return_type,
            body: new_body,
            span: func.span,
        }
    }

    fn clone_block(&mut self, block: &HirBlock) -> HirBlock {
        let stmt_ids: Vec<HirStmtId> = block.stmts.clone();
        let expr_id = block.expr;
        let mut new_stmts = Vec::new();
        for old_sid in stmt_ids {
            new_stmts.push(self.clone_stmt(old_sid));
        }
        let new_expr = expr_id.map(|eid| self.clone_expr(eid));
        HirBlock {
            stmts: new_stmts,
            expr: new_expr,
        }
    }

    fn clone_stmt(&mut self, old_stmt_id: HirStmtId) -> HirStmtId {
        // Extract data to avoid borrow conflict
        let data = extract_stmt_data(self.old_stmts.get(old_stmt_id));
        let new_stmt = match data {
            StmtData::Let {
                var: old_var_id,
                init,
                span,
            } => {
                let new_init = self.clone_expr(init);
                // Extract old_var data upfront to avoid borrow conflict
                let old_var_ty = self.old_vars[old_var_id.0 as usize].ty.clone();
                let old_var_name = self.old_vars[old_var_id.0 as usize].name.clone();
                let old_var_mutable = self.old_vars[old_var_id.0 as usize].mutable;
                let old_var_span = self.old_vars[old_var_id.0 as usize].span;

                let resolved_var_ty = self.resolve_type(&old_var_ty);
                let init_ty = self.new_exprs.get(new_init).ty.clone();

                // Determine the final type:
                // - If the variable type is resolved, use it (and propagate to init if init is unresolved)
                // - If the variable type is unresolved but init is resolved, use init's type
                let new_ty = if has_unresolved_type_params(&resolved_var_ty, &self.ctx) {
                    if !has_unresolved_type_params(&init_ty, &self.ctx) {
                        // Init has concrete type - use it (e.g., call to generic function returned concrete type)
                        init_ty.clone()
                    } else {
                        // Both unresolved - keep var type as-is
                        resolved_var_ty
                    }
                } else {
                    // Var type is concrete - if init is unresolved, propagate var type to init
                    if has_unresolved_type_params(&init_ty, &self.ctx) {
                        self.fixup_expr_type(new_init, &resolved_var_ty);
                    }
                    resolved_var_ty
                };

                // Check for unresolved type parameters (e.g., let x = Maybe::None)
                if has_unresolved_type_params(&new_ty, &self.ctx) {
                    self.error(
                        span,
                        format!(
                            "変数 '{}' の型パラメータを推論できません。型注釈を追加してください",
                            old_var_name
                        ),
                    );
                }

                let new_var_id = VarId(self.new_vars.len() as u32);
                self.new_vars.push(VarInfo {
                    name: old_var_name,
                    ty: new_ty,
                    mutable: old_var_mutable,
                    span: old_var_span,
                });
                self.var_map.insert(old_var_id.0, new_var_id);
                HirStmt::Let {
                    var: new_var_id,
                    init: new_init,
                    span,
                }
            }
            StmtData::Return { value, span } => {
                let new_value = value.map(|eid| self.clone_expr(eid));
                HirStmt::Return {
                    value: new_value,
                    span,
                }
            }
            StmtData::Break { label, value, span } => {
                let new_value = value.map(|eid| self.clone_expr(eid));
                HirStmt::Break {
                    label,
                    value: new_value,
                    span,
                }
            }
            StmtData::Continue { label, span } => HirStmt::Continue { label, span },
            StmtData::Expr(eid) => {
                let new_eid = self.clone_expr(eid);
                HirStmt::Expr(new_eid)
            }
        };
        self.new_stmts.alloc(new_stmt)
    }

    fn clone_expr(&mut self, old_expr_id: HirExprId) -> HirExprId {
        // Extract data to avoid borrow conflict
        let data = extract_expr_data(self.old_exprs.get(old_expr_id));
        let resolved_ty = self.resolve_type(&data.ty);
        let span = data.span;

        let new_kind = match data.kind {
            ExprKindData::IntegerLiteral { value } => HirExprKind::IntegerLiteral { value },
            ExprKindData::FloatLiteral { value } => HirExprKind::FloatLiteral { value },
            ExprKindData::BoolLiteral(b) => HirExprKind::BoolLiteral(b),
            ExprKindData::UnitLiteral => HirExprKind::UnitLiteral,
            ExprKindData::Var(var_id) => {
                let new_var_id = self.var_map.get(&var_id.0).copied().unwrap_or(var_id);
                // Use the new variable's type, which may have been updated (e.g., from init expression)
                let var_ty = self.new_vars[new_var_id.0 as usize].ty.clone();
                let new_id = self.new_exprs.alloc(HirExpr {
                    kind: HirExprKind::Var(new_var_id),
                    ty: var_ty,
                    span,
                });
                return new_id;
            }
            ExprKindData::BinaryOp { op, left, right } => {
                let new_left = self.clone_expr(left);
                let new_right = self.clone_expr(right);
                HirExprKind::BinaryOp {
                    op,
                    left: new_left,
                    right: new_right,
                }
            }
            ExprKindData::UnaryOp { op, operand } => {
                let new_operand = self.clone_expr(operand);
                HirExprKind::UnaryOp {
                    op,
                    operand: new_operand,
                }
            }
            ExprKindData::Cast { expr, target_type } => {
                let new_inner = self.clone_expr(expr);
                let new_target = self.resolve_type(&target_type);
                HirExprKind::Cast {
                    expr: new_inner,
                    target_type: new_target,
                }
            }
            ExprKindData::Call { func, args } => {
                let new_args: Vec<HirExprId> = args.iter().map(|&a| self.clone_expr(a)).collect();
                let (new_func_id, maybe_ret_ty, maybe_param_types) =
                    self.resolve_call(func, &new_args, span);
                if let Some(ret_ty) = maybe_ret_ty {
                    // Fix up argument types: if any arg has unresolved type but we know the concrete param type
                    if let Some(ref param_types) = maybe_param_types {
                        for (i, &arg_id) in new_args.iter().enumerate() {
                            if i < param_types.len() {
                                let arg_ty = self.new_exprs.get(arg_id).ty.clone();
                                if has_unresolved_type_params(&arg_ty, &self.ctx) {
                                    self.fixup_expr_type(arg_id, &param_types[i]);
                                }
                            }
                        }
                    }
                    let new_id = self.new_exprs.alloc(HirExpr {
                        kind: HirExprKind::Call {
                            func: new_func_id,
                            args: new_args,
                        },
                        ty: ret_ty,
                        span,
                    });
                    return new_id;
                }
                HirExprKind::Call {
                    func: new_func_id,
                    args: new_args,
                }
            }
            ExprKindData::Assign { var, value } => {
                let new_value = self.clone_expr(value);
                let new_var_id = self.var_map.get(&var.0).copied().unwrap_or(var);
                HirExprKind::Assign {
                    var: new_var_id,
                    value: new_value,
                }
            }
            ExprKindData::If {
                condition,
                then_block,
                else_block,
            } => {
                let new_cond = self.clone_expr(condition);
                let new_then = self.clone_block(&then_block);
                let new_else = else_block.map(|ec| match ec {
                    HirElseClause::ElseBlock(block) => {
                        HirElseClause::ElseBlock(self.clone_block(&block))
                    }
                    HirElseClause::ElseIf(eid) => HirElseClause::ElseIf(self.clone_expr(eid)),
                });
                HirExprKind::If {
                    condition: new_cond,
                    then_block: new_then,
                    else_block: new_else,
                }
            }
            ExprKindData::While {
                label,
                condition,
                body,
            } => {
                let new_cond = self.clone_expr(condition);
                let new_body = self.clone_block(&body);
                HirExprKind::While {
                    label,
                    condition: new_cond,
                    body: new_body,
                }
            }
            ExprKindData::Match { expr, arms } => {
                let new_match_expr = self.clone_expr(expr);
                let match_type = self.new_exprs.get(new_match_expr).ty.clone();
                // Ensure the match type's enum (if monomorphized) is instantiated
                // so that collect_pattern_bindings can look up variant field types.
                if let Type::Named(ref match_name) = match_type {
                    self.ensure_monomorphized_type_exists(match_name);
                }
                let mut new_arms = Vec::new();
                for (pattern, body_id) in arms {
                    // First rewrite the pattern to use monomorphized names
                    let new_pattern = self.rewrite_pattern_with_type(&pattern, &match_type);
                    // Collect bindings using the rewritten pattern and monomorphized type
                    let bindings = collect_pattern_bindings(&self.ctx, &new_pattern, &match_type);
                    for (bname, bty) in bindings {
                        let resolved_ty = self.resolve_type(&bty);
                        let new_var_id = VarId(self.new_vars.len() as u32);
                        self.new_vars.push(VarInfo {
                            name: bname.clone(),
                            ty: resolved_ty,
                            mutable: false,
                            span,
                        });
                        // Map old var IDs: find matching old var by name
                        for (idx, old_var) in self.old_vars.iter().enumerate() {
                            if old_var.name == bname && !self.var_map.contains_key(&(idx as u32)) {
                                self.var_map.insert(idx as u32, new_var_id);
                                break;
                            }
                        }
                    }
                    let new_body = self.clone_expr(body_id);
                    new_arms.push(HirMatchArm {
                        pattern: new_pattern,
                        body: new_body,
                    });
                }
                // Use the first arm's body type as the match result type
                let match_result_ty = if !new_arms.is_empty() {
                    self.new_exprs.get(new_arms[0].body).ty.clone()
                } else {
                    resolved_ty.clone()
                };
                let new_id = self.new_exprs.alloc(HirExpr {
                    kind: HirExprKind::Match {
                        expr: new_match_expr,
                        arms: new_arms,
                    },
                    ty: match_result_ty,
                    span,
                });
                return new_id;
            }
            ExprKindData::Block(block) => HirExprKind::Block(self.clone_block(&block)),
            ExprKindData::FieldAccess {
                expr,
                struct_name,
                field_index,
            } => {
                let new_inner = self.clone_expr(expr);
                let inner_ty = self.new_exprs.get(new_inner).ty.clone();
                let resolved_name = match &inner_ty {
                    Type::Named(n) => n.clone(),
                    _ => struct_name,
                };
                // Look up the field type from the (possibly monomorphized) struct
                let field_ty = if let Some(struct_info) = self.ctx.structs.get(&resolved_name) {
                    if field_index < struct_info.fields.len() {
                        struct_info.fields[field_index].1.clone()
                    } else {
                        resolved_ty.clone()
                    }
                } else {
                    resolved_ty.clone()
                };
                let new_id = self.new_exprs.alloc(HirExpr {
                    kind: HirExprKind::FieldAccess {
                        expr: new_inner,
                        struct_name: resolved_name,
                        field_index,
                    },
                    ty: field_ty,
                    span,
                });
                return new_id;
            }
            ExprKindData::TupleIndex { expr, index } => {
                let new_inner = self.clone_expr(expr);
                HirExprKind::TupleIndex {
                    expr: new_inner,
                    index,
                }
            }
            ExprKindData::TupleExpr(elems) => {
                let new_elems: Vec<HirExprId> =
                    elems.iter().map(|&eid| self.clone_expr(eid)).collect();
                HirExprKind::TupleExpr(new_elems)
            }
            ExprKindData::StructExpr { name, fields } => {
                let new_fields: Vec<HirExprId> =
                    fields.iter().map(|&fid| self.clone_expr(fid)).collect();
                // Try to re-infer type if still has unresolved params
                let final_ty = if has_unresolved_type_params(&resolved_ty, &self.ctx) {
                    if let Some(struct_info) = self.ctx.structs.get(&name).cloned() {
                        if !struct_info.type_params.is_empty() {
                            let mut mapping = HashMap::new();
                            for (i, (_, def_ty)) in struct_info.fields.iter().enumerate() {
                                if i < new_fields.len() {
                                    let field_ty = &self.new_exprs.get(new_fields[i]).ty;
                                    self.unify_types(
                                        def_ty,
                                        field_ty,
                                        &struct_info.type_params,
                                        &mut mapping,
                                    );
                                }
                            }
                            let inferred_args: Vec<Type> = struct_info
                                .type_params
                                .iter()
                                .map(|p| {
                                    mapping
                                        .get(p)
                                        .cloned()
                                        .unwrap_or(Type::TypeParam(p.clone()))
                                })
                                .collect();
                            let mangled = mangle_name(&name, &inferred_args);
                            Type::Named(mangled)
                        } else {
                            resolved_ty.clone()
                        }
                    } else {
                        resolved_ty.clone()
                    }
                } else {
                    resolved_ty.clone()
                };
                let resolved_name = match &final_ty {
                    Type::Named(n) => n.clone(),
                    _ => name.clone(),
                };
                self.ensure_struct_instantiated(&resolved_name, &name);
                let new_id = self.new_exprs.alloc(HirExpr {
                    kind: HirExprKind::StructExpr {
                        name: resolved_name,
                        fields: new_fields,
                    },
                    ty: final_ty,
                    span,
                });
                return new_id;
            }
            ExprKindData::EnumExpr {
                enum_name,
                variant_index,
                args,
            } => {
                let new_args = match args {
                    EnumArgsData::Unit => HirEnumArgs::Unit,
                    EnumArgsData::Tuple(eids) => {
                        HirEnumArgs::Tuple(eids.iter().map(|&eid| self.clone_expr(eid)).collect())
                    }
                    EnumArgsData::Struct(eids) => {
                        HirEnumArgs::Struct(eids.iter().map(|&eid| self.clone_expr(eid)).collect())
                    }
                };
                // Try to re-infer type if still has unresolved params
                let final_ty = if has_unresolved_type_params(&resolved_ty, &self.ctx) {
                    if let Some(enum_info) = self.ctx.enums.get(&enum_name).cloned() {
                        if !enum_info.type_params.is_empty() {
                            let mut mapping = HashMap::new();
                            let variant_info = &enum_info.variants[variant_index].1;
                            match (variant_info, &new_args) {
                                (VariantInfo::Tuple(types), HirEnumArgs::Tuple(hir_exprs)) => {
                                    for (def_ty, &hir_eid) in types.iter().zip(hir_exprs.iter()) {
                                        let arg_ty = &self.new_exprs.get(hir_eid).ty;
                                        self.unify_types(
                                            def_ty,
                                            arg_ty,
                                            &enum_info.type_params,
                                            &mut mapping,
                                        );
                                    }
                                }
                                (VariantInfo::Struct(fields), HirEnumArgs::Struct(hir_exprs)) => {
                                    for ((_, def_ty), &hir_eid) in
                                        fields.iter().zip(hir_exprs.iter())
                                    {
                                        let arg_ty = &self.new_exprs.get(hir_eid).ty;
                                        self.unify_types(
                                            def_ty,
                                            arg_ty,
                                            &enum_info.type_params,
                                            &mut mapping,
                                        );
                                    }
                                }
                                _ => {}
                            }
                            let inferred_args: Vec<Type> = enum_info
                                .type_params
                                .iter()
                                .map(|p| {
                                    mapping
                                        .get(p)
                                        .cloned()
                                        .unwrap_or(Type::TypeParam(p.clone()))
                                })
                                .collect();
                            if inferred_args
                                .iter()
                                .any(|t| matches!(t, Type::TypeParam(_)))
                            {
                                // Still unresolved - keep as Generic for later resolution
                                resolved_ty.clone()
                            } else {
                                let mangled = mangle_name(&enum_name, &inferred_args);
                                Type::Named(mangled)
                            }
                        } else {
                            resolved_ty.clone()
                        }
                    } else {
                        resolved_ty.clone()
                    }
                } else {
                    resolved_ty.clone()
                };
                let resolved_name = match &final_ty {
                    Type::Named(n) => n.clone(),
                    _ => enum_name.clone(),
                };
                if !has_unresolved_type_params(&final_ty, &self.ctx) {
                    self.ensure_enum_instantiated(&resolved_name, &enum_name);
                }
                let new_id = self.new_exprs.alloc(HirExpr {
                    kind: HirExprKind::EnumExpr {
                        enum_name: resolved_name,
                        variant_index,
                        args: new_args,
                    },
                    ty: final_ty,
                    span,
                });
                return new_id;
            }
        };

        self.new_exprs.alloc(HirExpr {
            kind: new_kind,
            ty: resolved_ty,
            span,
        })
    }

    // --- Pattern rewriting ---

    /// Rewrite pattern using the match expression's type to resolve monomorphized names.
    fn rewrite_pattern_with_type(&self, pattern: &Pattern, match_ty: &Type) -> Pattern {
        match pattern {
            Pattern::Enum {
                enum_name,
                variant,
                args,
            } => {
                // Determine the resolved enum name from the match type
                let resolved_name = match match_ty {
                    Type::Named(n) => {
                        // If the match type is a monomorphized name like "Maybe__i32",
                        // and the pattern refers to the original "Maybe", use the monomorphized name
                        if n.starts_with(&format!("{enum_name}__")) || n == enum_name {
                            n.clone()
                        } else {
                            enum_name.clone()
                        }
                    }
                    _ => enum_name.clone(),
                };
                // Resolve subpattern types from the enum's variant info
                let new_args = match args {
                    EnumPatternArgs::Unit => EnumPatternArgs::Unit,
                    EnumPatternArgs::Tuple(pats) => {
                        // Get variant field types for sub-pattern rewriting
                        let sub_types = self.get_enum_variant_tuple_types(&resolved_name, variant);
                        EnumPatternArgs::Tuple(
                            pats.iter()
                                .enumerate()
                                .map(|(i, p)| {
                                    let sub_ty = sub_types
                                        .as_ref()
                                        .and_then(|ts| ts.get(i))
                                        .cloned()
                                        .unwrap_or(Type::Error);
                                    self.rewrite_pattern_with_type(p, &sub_ty)
                                })
                                .collect(),
                        )
                    }
                    EnumPatternArgs::Struct(fields) => EnumPatternArgs::Struct(
                        fields
                            .iter()
                            .map(|(n, p)| {
                                (n.clone(), self.rewrite_pattern_with_type(p, &Type::Error))
                            })
                            .collect(),
                    ),
                };
                Pattern::Enum {
                    enum_name: resolved_name,
                    variant: variant.clone(),
                    args: new_args,
                }
            }
            Pattern::Struct { name, fields } => {
                let resolved_name = match match_ty {
                    Type::Named(n) => {
                        if n.starts_with(&format!("{name}__")) || n == name {
                            n.clone()
                        } else {
                            name.clone()
                        }
                    }
                    _ => name.clone(),
                };
                let new_fields: Vec<(String, Pattern)> = fields
                    .iter()
                    .map(|(fname, pat)| {
                        (
                            fname.clone(),
                            self.rewrite_pattern_with_type(pat, &Type::Error),
                        )
                    })
                    .collect();
                Pattern::Struct {
                    name: resolved_name,
                    fields: new_fields,
                }
            }
            Pattern::Tuple(pats) => {
                let sub_types = if let Type::Tuple(types) = match_ty {
                    types.clone()
                } else {
                    Vec::new()
                };
                Pattern::Tuple(
                    pats.iter()
                        .enumerate()
                        .map(|(i, p)| {
                            let sub_ty = sub_types.get(i).cloned().unwrap_or(Type::Error);
                            self.rewrite_pattern_with_type(p, &sub_ty)
                        })
                        .collect(),
                )
            }
            other => other.clone(),
        }
    }

    /// Get tuple variant field types for a (possibly monomorphized) enum.
    fn get_enum_variant_tuple_types(
        &self,
        enum_name: &str,
        variant_name: &str,
    ) -> Option<Vec<Type>> {
        if let Some(enum_info) = self.ctx.enums.get(enum_name) {
            for (vname, vinfo) in &enum_info.variants {
                if vname == variant_name
                    && let VariantInfo::Tuple(types) = vinfo
                {
                    return Some(types.clone());
                }
            }
        }
        None
    }

    // --- Resolve function calls ---

    /// Returns (new_func_id, resolved_return_type, concrete_param_types).
    fn resolve_call(
        &mut self,
        old_func_id: FuncId,
        new_args: &[HirExprId],
        span: Span,
    ) -> (FuncId, Option<Type>, Option<Vec<Type>>) {
        let old_name = self.old_func_names[old_func_id.0 as usize].clone();

        if let Some(func_info) = self.ctx.functions.get(&old_name).cloned()
            && !func_info.type_params.is_empty()
        {
            let type_args =
                self.infer_type_args(&func_info.type_params, &func_info.param_types, new_args);

            if type_args.iter().any(|t| matches!(t, Type::TypeParam(_))) {
                self.error(
                    span,
                    format!(
                        "ジェネリック関数 '{}' の型パラメータを推論できません",
                        old_name
                    ),
                );
                return (old_func_id, None, None);
            }

            // Compute the concrete return type and param types
            let mut mapping = HashMap::new();
            for (param, arg) in func_info.type_params.iter().zip(type_args.iter()) {
                mapping.insert(param.clone(), arg.clone());
            }
            let concrete_return = substitute_type_map(&func_info.return_type, &mapping);
            let concrete_params: Vec<Type> = func_info
                .param_types
                .iter()
                .map(|pt| substitute_type_map(pt, &mapping))
                .collect();
            let (resolved_return, resolved_params) = {
                let old_map = self.current_type_map.clone();
                self.current_type_map.extend(mapping);
                let ret = self.resolve_type(&concrete_return);
                let params: Vec<Type> = concrete_params
                    .iter()
                    .map(|p| self.resolve_type(p))
                    .collect();
                self.current_type_map = old_map;
                (ret, params)
            };

            let mangled = mangle_name(&old_name, &type_args);
            let key = InstKey {
                name: old_name.clone(),
                type_args,
            };
            if !self.instantiated_funcs.contains_key(&key) {
                let func_id = FuncId(self.new_func_names.len() as u32);
                self.new_func_names.push(mangled.clone());
                self.new_func_ids.insert(mangled.clone(), func_id);
                self.instantiated_funcs.insert(key, mangled.clone());
            }
            return (
                self.new_func_ids[&mangled],
                Some(resolved_return),
                Some(resolved_params),
            );
        }

        (
            self.func_id_map
                .get(&old_func_id.0)
                .copied()
                .unwrap_or(old_func_id),
            None,
            None,
        )
    }

    // --- Type inference ---

    fn infer_type_args(
        &self,
        type_params: &[String],
        param_types: &[Type],
        actual_args: &[HirExprId],
    ) -> Vec<Type> {
        let mut mapping: HashMap<String, Type> = HashMap::new();
        for (param_ty, &arg_id) in param_types.iter().zip(actual_args.iter()) {
            let arg_ty = self.new_exprs.get(arg_id).ty.clone();
            self.unify_types(param_ty, &arg_ty, type_params, &mut mapping);
        }
        type_params
            .iter()
            .map(|p| {
                mapping
                    .get(p)
                    .cloned()
                    .unwrap_or(Type::TypeParam(p.clone()))
            })
            .collect()
    }

    fn unify_types(
        &self,
        pattern: &Type,
        actual: &Type,
        type_params: &[String],
        mapping: &mut HashMap<String, Type>,
    ) {
        // Skip unification if the actual type is unresolved (TypeParam or unknown Named)
        let actual_is_unresolved = has_unresolved_type_params(actual, &self.ctx);

        match pattern {
            Type::TypeParam(name) => {
                if type_params.contains(name) && !actual_is_unresolved {
                    // Insert or upgrade: prefer concrete types over unresolved ones
                    match mapping.entry(name.clone()) {
                        std::collections::hash_map::Entry::Vacant(e) => {
                            e.insert(actual.clone());
                        }
                        std::collections::hash_map::Entry::Occupied(mut e) => {
                            // Overwrite if current value is unresolved but new is concrete
                            if has_unresolved_type_params(e.get(), &self.ctx) {
                                e.insert(actual.clone());
                            }
                        }
                    }
                }
            }
            Type::Named(name) if type_params.contains(name) => {
                if !actual_is_unresolved {
                    match mapping.entry(name.clone()) {
                        std::collections::hash_map::Entry::Vacant(e) => {
                            e.insert(actual.clone());
                        }
                        std::collections::hash_map::Entry::Occupied(mut e) => {
                            if has_unresolved_type_params(e.get(), &self.ctx) {
                                e.insert(actual.clone());
                            }
                        }
                    }
                }
            }
            Type::Generic(name, args) => match actual {
                Type::Named(actual_name) => {
                    if let Some(suffix) = actual_name.strip_prefix(&format!("{name}__")) {
                        let parsed_args = parse_mangled_type_args(suffix, args.len(), &self.ctx);
                        for (arg, concrete) in args.iter().zip(parsed_args.iter()) {
                            self.unify_types(arg, concrete, type_params, mapping);
                        }
                    }
                }
                Type::Generic(actual_name, actual_args) if actual_name == name => {
                    for (p, a) in args.iter().zip(actual_args.iter()) {
                        self.unify_types(p, a, type_params, mapping);
                    }
                }
                _ => {}
            },
            Type::Tuple(pat_types) => {
                if let Type::Tuple(act_types) = actual {
                    for (p, a) in pat_types.iter().zip(act_types.iter()) {
                        self.unify_types(p, a, type_params, mapping);
                    }
                }
            }
            _ => {}
        }
    }

    // --- Struct/Enum instantiation ---

    /// For a mangled name like "Maybe__i32", find the original type name ("Maybe")
    /// and instantiate it if it hasn't been already. Also recursively ensures
    /// any nested types are instantiated.
    fn ensure_monomorphized_type_exists(&mut self, mangled_name: &str) {
        // Already registered?
        if self.ctx.enums.contains_key(mangled_name) || self.ctx.structs.contains_key(mangled_name)
        {
            return;
        }
        // Try to find the original type: scan known generic enums and structs
        for (original_name, info) in self.ctx.enums.clone() {
            if !info.type_params.is_empty()
                && mangled_name.starts_with(&format!("{original_name}__"))
            {
                self.ensure_enum_instantiated(mangled_name, &original_name);
                return;
            }
        }
        for (original_name, info) in self.ctx.structs.clone() {
            if !info.type_params.is_empty()
                && mangled_name.starts_with(&format!("{original_name}__"))
            {
                self.ensure_struct_instantiated(mangled_name, &original_name);
                return;
            }
        }
    }

    /// Ensure that all types referenced within a Type are instantiated.
    fn ensure_nested_types_instantiated(&mut self, ty: &Type) {
        match ty {
            Type::Named(name) => {
                if name.contains("__") {
                    self.ensure_monomorphized_type_exists(name);
                }
            }
            Type::Generic(_, args) => {
                for arg in args {
                    self.ensure_nested_types_instantiated(arg);
                }
            }
            Type::Tuple(types) => {
                for t in types {
                    self.ensure_nested_types_instantiated(t);
                }
            }
            _ => {}
        }
    }

    fn ensure_struct_instantiated_with_args(
        &mut self,
        resolved_name: &str,
        original_name: &str,
        type_args: &[Type],
    ) {
        if self.new_struct_order.contains(&resolved_name.to_string()) {
            return;
        }
        if let Some(struct_info) = self.ctx.structs.get(original_name).cloned()
            && !struct_info.type_params.is_empty()
        {
            let key = InstKey {
                name: original_name.to_string(),
                type_args: type_args.to_vec(),
            };
            if self.instantiated_structs.contains_key(&key) {
                return;
            }
            self.instantiated_structs
                .insert(key, resolved_name.to_string());

            let mut mapping = HashMap::new();
            for (param, arg) in struct_info.type_params.iter().zip(type_args.iter()) {
                mapping.insert(param.clone(), arg.clone());
            }
            let new_fields: Vec<(String, Type)> = struct_info
                .fields
                .iter()
                .map(|(name, ty)| {
                    let resolved = substitute_type_map(ty, &mapping);
                    let old_map = self.current_type_map.clone();
                    self.current_type_map.extend(mapping.clone());
                    let final_ty = self.resolve_type(&resolved);
                    self.current_type_map = old_map;
                    (name.clone(), final_ty)
                })
                .collect();
            // Collect field types for nested instantiation
            let field_types: Vec<Type> = new_fields.iter().map(|(_, ty)| ty.clone()).collect();
            let new_info = StructInfo {
                type_params: Vec::new(),
                fields: new_fields,
            };
            self.ctx.structs.insert(resolved_name.to_string(), new_info);
            self.new_struct_order.push(resolved_name.to_string());
            // Ensure any nested types (e.g., Pair__i32__bool inside Maybe's variant) are also instantiated
            for ft in &field_types {
                self.ensure_nested_types_instantiated(ft);
            }
        }
    }

    fn ensure_struct_instantiated(&mut self, resolved_name: &str, original_name: &str) {
        if self.new_struct_order.contains(&resolved_name.to_string()) {
            return;
        }
        if let Some(struct_info) = self.ctx.structs.get(original_name).cloned()
            && !struct_info.type_params.is_empty()
            && let Some(suffix) = resolved_name.strip_prefix(&format!("{original_name}__"))
        {
            let expected_count = struct_info.type_params.len();
            let type_args = parse_mangled_type_args(suffix, expected_count, &self.ctx);
            self.ensure_struct_instantiated_with_args(resolved_name, original_name, &type_args);
        }
    }

    fn ensure_enum_instantiated(&mut self, resolved_name: &str, original_name: &str) {
        if self.new_enum_order.contains(&resolved_name.to_string()) {
            return;
        }
        if let Some(enum_info) = self.ctx.enums.get(original_name).cloned()
            && !enum_info.type_params.is_empty()
            && let Some(suffix) = resolved_name.strip_prefix(&format!("{original_name}__"))
        {
            let expected_count = enum_info.type_params.len();
            let type_args = parse_mangled_type_args(suffix, expected_count, &self.ctx);
            self.ensure_enum_instantiated_with_args(resolved_name, original_name, &type_args);
        }
    }

    fn ensure_enum_instantiated_with_args(
        &mut self,
        resolved_name: &str,
        original_name: &str,
        type_args: &[Type],
    ) {
        if self.new_enum_order.contains(&resolved_name.to_string()) {
            return;
        }
        if let Some(enum_info) = self.ctx.enums.get(original_name).cloned()
            && !enum_info.type_params.is_empty()
        {
            let key = InstKey {
                name: original_name.to_string(),
                type_args: type_args.to_vec(),
            };
            if self.instantiated_enums.contains_key(&key) {
                return;
            }
            self.instantiated_enums
                .insert(key, resolved_name.to_string());

            let mut mapping = HashMap::new();
            for (param, arg) in enum_info.type_params.iter().zip(type_args.iter()) {
                mapping.insert(param.clone(), arg.clone());
            }
            let new_variants: Vec<(String, VariantInfo)> = enum_info
                .variants
                .iter()
                .map(|(vname, vinfo)| {
                    let new_vinfo = match vinfo {
                        VariantInfo::Unit => VariantInfo::Unit,
                        VariantInfo::Tuple(types) => {
                            let new_types: Vec<Type> = types
                                .iter()
                                .map(|t| {
                                    let resolved = substitute_type_map(t, &mapping);
                                    let old_map = self.current_type_map.clone();
                                    self.current_type_map.extend(mapping.clone());
                                    let final_ty = self.resolve_type(&resolved);
                                    self.current_type_map = old_map;
                                    final_ty
                                })
                                .collect();
                            VariantInfo::Tuple(new_types)
                        }
                        VariantInfo::Struct(fields) => {
                            let new_fields: Vec<(String, Type)> = fields
                                .iter()
                                .map(|(fname, fty)| {
                                    let resolved = substitute_type_map(fty, &mapping);
                                    let old_map = self.current_type_map.clone();
                                    self.current_type_map.extend(mapping.clone());
                                    let final_ty = self.resolve_type(&resolved);
                                    self.current_type_map = old_map;
                                    (fname.clone(), final_ty)
                                })
                                .collect();
                            VariantInfo::Struct(new_fields)
                        }
                    };
                    (vname.clone(), new_vinfo)
                })
                .collect();
            // Collect all variant types for nested instantiation
            let mut variant_types = Vec::new();
            for (_, vinfo) in &new_variants {
                match vinfo {
                    VariantInfo::Tuple(types) => variant_types.extend(types.clone()),
                    VariantInfo::Struct(fields) => {
                        variant_types.extend(fields.iter().map(|(_, ty)| ty.clone()));
                    }
                    VariantInfo::Unit => {}
                }
            }
            let new_info = EnumInfo {
                type_params: Vec::new(),
                variants: new_variants,
            };
            self.ctx.enums.insert(resolved_name.to_string(), new_info);
            self.new_enum_order.push(resolved_name.to_string());
            // Ensure any nested types are also instantiated
            for vt in &variant_types {
                self.ensure_nested_types_instantiated(vt);
            }
        }
    }

    // --- Generic function instantiation ---

    fn instantiate_generic_function(
        &mut self,
        original_name: &str,
        type_args: &[Type],
        mangled_name: &str,
    ) {
        let func_info = match self.ctx.functions.get(original_name) {
            Some(info) => info.clone(),
            None => return,
        };

        // Clone out the func_def to avoid borrow issues
        let func_def_params: Vec<VarId>;
        let func_def_body: HirBlock;
        let func_def_return_type: Type;
        let func_def_span: Span;
        {
            let func_def = match self.generic_func_defs.get(original_name) {
                Some(def) => def,
                None => return,
            };
            func_def_params = func_def.params.clone();
            func_def_body = clone_hir_block(&func_def.body);
            func_def_return_type = func_def.return_type.clone();
            func_def_span = func_def.span;
        }

        let mut mapping = HashMap::new();
        for (param, arg) in func_info.type_params.iter().zip(type_args.iter()) {
            mapping.insert(param.clone(), arg.clone());
        }

        let old_type_map = self.current_type_map.clone();
        let old_var_map = self.var_map.clone();
        let old_func_id_map = self.func_id_map.clone();

        self.current_type_map = mapping;
        self.var_map.clear();

        // Rebuild func_id_map for all known functions
        let new_func_ids_snapshot = self.new_func_ids.clone();
        for (name, &new_fid) in &new_func_ids_snapshot {
            for (i, fname) in self.old_func_names.iter().enumerate() {
                if fname == name {
                    self.func_id_map.insert(i as u32, new_fid);
                }
            }
        }

        let new_func_id = self.new_func_ids[mangled_name];

        let mut new_params = Vec::new();
        for &old_var_id in &func_def_params {
            let old_var = &self.old_vars[old_var_id.0 as usize];
            let new_ty = self.resolve_type(&old_var.ty);
            let new_var_id = VarId(self.new_vars.len() as u32);
            self.new_vars.push(VarInfo {
                name: old_var.name.clone(),
                ty: new_ty,
                mutable: old_var.mutable,
                span: old_var.span,
            });
            self.var_map.insert(old_var_id.0, new_var_id);
            new_params.push(new_var_id);
        }

        let new_body = self.clone_block(&func_def_body);
        let new_return_type = self.resolve_type(&func_def_return_type);

        let new_func = HirFunctionDef {
            func_id: new_func_id,
            name: mangled_name.to_string(),
            params: new_params,
            return_type: new_return_type,
            body: new_body,
            span: func_def_span,
        };
        self.new_functions.push(new_func);

        self.current_type_map = old_type_map;
        self.var_map = old_var_map;
        self.func_id_map = old_func_id_map;
    }

    fn error(&mut self, span: Span, msg: String) {
        let line = crate::diagnostics::Diagnostics::span_to_line(&self.source, span);
        self.errors
            .push(format!("単相化エラー: {}行目: {}", line, msg));
    }
}

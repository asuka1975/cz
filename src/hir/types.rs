use crate::diagnostics::Diagnostics;
use crate::syntax::ast::{self, VariantKind};
use std::collections::HashMap;

/// Cz の型 (AST の Type と同一定義を HIR でも使う)
pub use ast::Type;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct FuncInfo {
    pub name: String,
    pub type_params: Vec<String>,
    pub param_count: usize,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Clone, Debug)]
pub struct StructInfo {
    pub type_params: Vec<String>,
    pub fields: Vec<(String, Type)>,
}

#[derive(Clone, Debug)]
pub struct EnumInfo {
    pub type_params: Vec<String>,
    pub variants: Vec<(String, VariantInfo)>,
}

#[derive(Clone, Debug)]
pub enum VariantInfo {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<(String, Type)>),
}

/// 型エイリアスの情報。
#[derive(Clone, Debug)]
pub struct TypeAliasInfo {
    pub type_params: Vec<String>,
    pub aliased_type: Type,
}

/// 型定義のレジストリ。構造体・列挙型・関数シグネチャを保持する。
pub struct TypeContext {
    pub functions: HashMap<String, FuncInfo>,
    pub structs: HashMap<String, StructInfo>,
    pub enums: HashMap<String, EnumInfo>,
    pub type_aliases: HashMap<String, TypeAliasInfo>,
}

impl TypeContext {
    pub fn build(program: &ast::Program, source: &str) -> Result<TypeContext, Vec<String>> {
        let mut functions = HashMap::new();
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();
        let mut type_aliases = HashMap::new();
        let mut errors = Vec::new();

        // Built-in print functions
        for (name, param_type) in [
            ("print_i8", Type::I8),
            ("print_i16", Type::I16),
            ("print_i32", Type::I32),
            ("print_i64", Type::I64),
            ("print_f32", Type::F32),
            ("print_f64", Type::F64),
            ("print_bool", Type::Bool),
        ] {
            functions.insert(
                name.to_string(),
                FuncInfo {
                    name: name.to_string(),
                    type_params: Vec::new(),
                    param_count: 1,
                    param_types: vec![param_type],
                    return_type: Type::Unit,
                },
            );
        }

        // Register type aliases
        for alias in &program.type_aliases {
            let line = Diagnostics::span_to_line(source, alias.span);
            if type_aliases.contains_key(&alias.name) {
                errors.push(format!(
                    "{}行目: 型エイリアス '{}' は既に定義されています",
                    line, alias.name
                ));
            } else {
                type_aliases.insert(
                    alias.name.clone(),
                    TypeAliasInfo {
                        type_params: alias.type_params.clone(),
                        aliased_type: alias.aliased_type.clone(),
                    },
                );
            }
        }

        // Detect cyclic type aliases
        for name in type_aliases.keys() {
            let mut visited = std::collections::HashSet::new();
            let mut current = name.clone();
            loop {
                if !visited.insert(current.clone()) {
                    let alias = &program
                        .type_aliases
                        .iter()
                        .find(|a| a.name == *name)
                        .unwrap();
                    let line = Diagnostics::span_to_line(source, alias.span);
                    errors.push(format!(
                        "{}行目: 型エイリアス '{}' は循環定義です",
                        line, name
                    ));
                    break;
                }
                if let Some(info) = type_aliases.get(&current) {
                    if let Type::Named(ref next) = info.aliased_type {
                        current = next.clone();
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
        }

        // Register structs
        for s in &program.structs {
            let line = Diagnostics::span_to_line(source, s.span);
            if structs.contains_key(&s.name) {
                errors.push(format!(
                    "{}行目: 構造体 '{}' は既に定義されています",
                    line, s.name
                ));
            } else {
                let fields = s
                    .fields
                    .iter()
                    .map(|f| (f.name.clone(), f.field_type.clone()))
                    .collect();
                structs.insert(
                    s.name.clone(),
                    StructInfo {
                        type_params: s.type_params.clone(),
                        fields,
                    },
                );
            }
        }

        // Register enums
        for e in &program.enums {
            let line = Diagnostics::span_to_line(source, e.span);
            if enums.contains_key(&e.name) || structs.contains_key(&e.name) {
                errors.push(format!(
                    "{}行目: 列挙型 '{}' は既に定義されています",
                    line, e.name
                ));
            } else {
                let variants = e
                    .variants
                    .iter()
                    .map(|v| {
                        let info = match &v.kind {
                            VariantKind::Unit => VariantInfo::Unit,
                            VariantKind::Tuple(types) => VariantInfo::Tuple(types.clone()),
                            VariantKind::Struct(fields) => VariantInfo::Struct(
                                fields
                                    .iter()
                                    .map(|f| (f.name.clone(), f.field_type.clone()))
                                    .collect(),
                            ),
                        };
                        (v.name.clone(), info)
                    })
                    .collect();
                enums.insert(
                    e.name.clone(),
                    EnumInfo {
                        type_params: e.type_params.clone(),
                        variants,
                    },
                );
            }
        }

        // Register functions
        for func in &program.functions {
            let line = Diagnostics::span_to_line(source, func.span);
            if functions.contains_key(&func.name) {
                errors.push(format!(
                    "{}行目: 関数 '{}' は既に定義されています",
                    line, func.name
                ));
            } else {
                functions.insert(
                    func.name.clone(),
                    FuncInfo {
                        name: func.name.clone(),
                        type_params: func.type_params.clone(),
                        param_count: func.params.len(),
                        param_types: func.params.iter().map(|p| p.param_type.clone()).collect(),
                        return_type: func.return_type.clone(),
                    },
                );
            }
        }

        // Validate type parameters in function signatures
        for func in &program.functions {
            let line = Diagnostics::span_to_line(source, func.span);
            let type_params = &func.type_params;
            // Check param types for undeclared type params
            for param in &func.params {
                check_undeclared_type_params(
                    &param.param_type,
                    type_params,
                    &structs,
                    &enums,
                    &type_aliases,
                    line,
                    &func.name,
                    &mut errors,
                );
            }
            // Check return type
            check_undeclared_type_params(
                &func.return_type,
                type_params,
                &structs,
                &enums,
                &type_aliases,
                line,
                &func.name,
                &mut errors,
            );
        }

        // Validate type argument counts in struct/enum type_args usage
        for func in &program.functions {
            let line = Diagnostics::span_to_line(source, func.span);
            for param in &func.params {
                check_type_arg_counts(&param.param_type, &structs, &enums, line, &mut errors);
            }
            check_type_arg_counts(&func.return_type, &structs, &enums, line, &mut errors);
        }

        if errors.is_empty() {
            Ok(TypeContext {
                functions,
                structs,
                enums,
                type_aliases,
            })
        } else {
            Err(errors)
        }
    }

    /// 型エイリアスを再帰的に解決する。
    pub fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Named(name) => {
                if let Some(alias_info) = self.type_aliases.get(name) {
                    if alias_info.type_params.is_empty() {
                        self.resolve_type(&alias_info.aliased_type)
                    } else {
                        ty.clone()
                    }
                } else {
                    ty.clone()
                }
            }
            Type::Generic(name, args) => {
                if let Some(alias_info) = self.type_aliases.get(name) {
                    // ジェネリック型エイリアスの展開
                    let mut resolved = alias_info.aliased_type.clone();
                    for (param, arg) in alias_info.type_params.iter().zip(args.iter()) {
                        resolved = substitute_type(&resolved, param, arg);
                    }
                    self.resolve_type(&resolved)
                } else {
                    let resolved_args: Vec<Type> =
                        args.iter().map(|a| self.resolve_type(a)).collect();
                    Type::Generic(name.clone(), resolved_args)
                }
            }
            Type::Tuple(types) => Type::Tuple(types.iter().map(|t| self.resolve_type(t)).collect()),
            _ => ty.clone(),
        }
    }

    /// 型名から Named 型を解決する。
    #[allow(dead_code)]
    pub fn resolve_named_type<'a>(&self, name: &'a str) -> Option<&'a str> {
        if self.structs.contains_key(name) || self.enums.contains_key(name) {
            Some(name)
        } else {
            None
        }
    }

    #[allow(dead_code)]
    pub fn get_builtin_func_id(&self, name: &str) -> bool {
        matches!(
            name,
            "print_i8"
                | "print_i16"
                | "print_i32"
                | "print_i64"
                | "print_f32"
                | "print_f64"
                | "print_bool"
        )
    }
}

/// Check if a type references undeclared type parameters.
#[allow(clippy::too_many_arguments)]
fn check_undeclared_type_params(
    ty: &Type,
    declared_params: &[String],
    structs: &HashMap<String, StructInfo>,
    enums: &HashMap<String, EnumInfo>,
    type_aliases: &HashMap<String, TypeAliasInfo>,
    line: usize,
    func_name: &str,
    errors: &mut Vec<String>,
) {
    match ty {
        Type::Named(name) => {
            // Skip if it's a known type, primitive, or declared type param
            if declared_params.contains(name)
                || structs.contains_key(name)
                || enums.contains_key(name)
                || type_aliases.contains_key(name)
                || matches!(
                    name.as_str(),
                    "i8" | "i16" | "i32" | "i64" | "f32" | "f64" | "bool" | "unit"
                )
            {
                return;
            }
            errors.push(format!(
                "{}行目: 関数 '{}' で未宣言の型パラメータ '{}' が使われています",
                line, func_name, name
            ));
        }
        Type::TypeParam(name) => {
            if !declared_params.contains(name) {
                errors.push(format!(
                    "{}行目: 関数 '{}' で未宣言の型パラメータ '{}' が使われています",
                    line, func_name, name
                ));
            }
        }
        Type::Generic(_, args) => {
            for arg in args {
                check_undeclared_type_params(
                    arg,
                    declared_params,
                    structs,
                    enums,
                    type_aliases,
                    line,
                    func_name,
                    errors,
                );
            }
        }
        Type::Tuple(types) => {
            for t in types {
                check_undeclared_type_params(
                    t,
                    declared_params,
                    structs,
                    enums,
                    type_aliases,
                    line,
                    func_name,
                    errors,
                );
            }
        }
        _ => {}
    }
}

/// Check if Generic types use the correct number of type arguments.
fn check_type_arg_counts(
    ty: &Type,
    structs: &HashMap<String, StructInfo>,
    enums: &HashMap<String, EnumInfo>,
    line: usize,
    errors: &mut Vec<String>,
) {
    if let Type::Generic(name, args) = ty {
        if let Some(info) = structs.get(name)
            && args.len() != info.type_params.len()
        {
            errors.push(format!(
                "{}行目: 構造体 '{}' は {}個の型引数が必要ですが、{}個指定されています",
                line,
                name,
                info.type_params.len(),
                args.len()
            ));
        }
        if let Some(info) = enums.get(name)
            && args.len() != info.type_params.len()
        {
            errors.push(format!(
                "{}行目: 列挙型 '{}' は {}個の型引数が必要ですが、{}個指定されています",
                line,
                name,
                info.type_params.len(),
                args.len()
            ));
        }
        for arg in args {
            check_type_arg_counts(arg, structs, enums, line, errors);
        }
    }
    if let Type::Tuple(types) = ty {
        for t in types {
            check_type_arg_counts(t, structs, enums, line, errors);
        }
    }
}

/// 型内の型パラメータを具体型で置換する。
pub fn substitute_type(ty: &Type, param: &str, replacement: &Type) -> Type {
    match ty {
        Type::TypeParam(name) if name == param => replacement.clone(),
        Type::Named(name) if name == param => replacement.clone(),
        Type::Tuple(types) => Type::Tuple(
            types
                .iter()
                .map(|t| substitute_type(t, param, replacement))
                .collect(),
        ),
        Type::Generic(name, args) => Type::Generic(
            name.clone(),
            args.iter()
                .map(|a| substitute_type(a, param, replacement))
                .collect(),
        ),
        _ => ty.clone(),
    }
}

/// 型パラメータ→具体型のマッピングで型を一括置換する。
pub fn substitute_type_map(ty: &Type, mapping: &HashMap<String, Type>) -> Type {
    match ty {
        Type::TypeParam(name) | Type::Named(name) if mapping.contains_key(name) => {
            mapping[name].clone()
        }
        Type::Tuple(types) => Type::Tuple(
            types
                .iter()
                .map(|t| substitute_type_map(t, mapping))
                .collect(),
        ),
        Type::Generic(name, args) => {
            let resolved_args: Vec<Type> = args
                .iter()
                .map(|a| substitute_type_map(a, mapping))
                .collect();
            Type::Generic(name.clone(), resolved_args)
        }
        _ => ty.clone(),
    }
}

/// パターンからバインディングを収集する共有ユーティリティ。
pub fn collect_pattern_bindings(
    ctx: &TypeContext,
    pattern: &ast::Pattern,
    ty: &Type,
) -> Vec<(String, Type)> {
    let mut bindings = Vec::new();
    collect_bindings_inner(ctx, pattern, ty, &mut bindings);
    bindings
}

fn collect_bindings_inner(
    ctx: &TypeContext,
    pattern: &ast::Pattern,
    ty: &Type,
    bindings: &mut Vec<(String, Type)>,
) {
    match pattern {
        ast::Pattern::Binding(name) => {
            bindings.push((name.clone(), ty.clone()));
        }
        ast::Pattern::Tuple(patterns) => {
            if let Type::Tuple(types) = ty {
                for (pat, t) in patterns.iter().zip(types.iter()) {
                    collect_bindings_inner(ctx, pat, t, bindings);
                }
            }
        }
        ast::Pattern::Struct { name, fields } => {
            if let Some(struct_info) = ctx.structs.get(name) {
                let struct_fields = struct_info.fields.clone();
                for (field_name, field_pat) in fields {
                    if let Some((_, field_type)) =
                        struct_fields.iter().find(|(n, _)| n == field_name)
                    {
                        collect_bindings_inner(ctx, field_pat, field_type, bindings);
                    }
                }
            }
        }
        ast::Pattern::Enum {
            enum_name,
            variant,
            args,
        } => {
            if let Some(enum_info) = ctx.enums.get(enum_name) {
                // Build type param mapping if match type is Generic
                let type_mapping: HashMap<String, Type> = if let Type::Generic(_, type_args) = ty {
                    enum_info
                        .type_params
                        .iter()
                        .cloned()
                        .zip(type_args.iter().cloned())
                        .collect()
                } else {
                    HashMap::new()
                };

                let variants = enum_info.variants.clone();
                if let Some((_, variant_info)) = variants.iter().find(|(n, _)| n == variant) {
                    match (variant_info, args) {
                        (VariantInfo::Tuple(types), ast::EnumPatternArgs::Tuple(pats)) => {
                            for (pat, t) in pats.iter().zip(types.iter()) {
                                let resolved_t = if type_mapping.is_empty() {
                                    t.clone()
                                } else {
                                    substitute_type_map(t, &type_mapping)
                                };
                                collect_bindings_inner(ctx, pat, &resolved_t, bindings);
                            }
                        }
                        (VariantInfo::Struct(fields), ast::EnumPatternArgs::Struct(pat_fields)) => {
                            for (field_name, field_pat) in pat_fields {
                                if let Some((_, field_type)) =
                                    fields.iter().find(|(n, _)| n == field_name)
                                {
                                    let resolved_ft = if type_mapping.is_empty() {
                                        field_type.clone()
                                    } else {
                                        substitute_type_map(field_type, &type_mapping)
                                    };
                                    collect_bindings_inner(ctx, field_pat, &resolved_ft, bindings);
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
        _ => {}
    }
}

use crate::diagnostics::Diagnostics;
use crate::syntax::ast::{self, VariantKind};
use std::collections::HashMap;

/// Cz の型 (AST の Type と同一定義を HIR でも使う)
pub use ast::Type;

#[allow(dead_code)]
#[derive(Clone, Debug)]
pub struct FuncInfo {
    pub name: String,
    pub param_count: usize,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Clone, Debug)]
pub struct StructInfo {
    pub fields: Vec<(String, Type)>,
}

#[derive(Clone, Debug)]
pub struct EnumInfo {
    pub variants: Vec<(String, VariantInfo)>,
}

#[derive(Clone, Debug)]
pub enum VariantInfo {
    Unit,
    Tuple(Vec<Type>),
    Struct(Vec<(String, Type)>),
}

/// 型定義のレジストリ。構造体・列挙型・関数シグネチャを保持する。
pub struct TypeContext {
    pub functions: HashMap<String, FuncInfo>,
    pub structs: HashMap<String, StructInfo>,
    pub enums: HashMap<String, EnumInfo>,
}

impl TypeContext {
    pub fn build(program: &ast::Program, source: &str) -> Result<TypeContext, Vec<String>> {
        let mut functions = HashMap::new();
        let mut structs = HashMap::new();
        let mut enums = HashMap::new();
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
                    param_count: 1,
                    param_types: vec![param_type],
                    return_type: Type::Unit,
                },
            );
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
                structs.insert(s.name.clone(), StructInfo { fields });
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
                enums.insert(e.name.clone(), EnumInfo { variants });
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
                        param_count: func.params.len(),
                        param_types: func.params.iter().map(|p| p.param_type.clone()).collect(),
                        return_type: func.return_type.clone(),
                    },
                );
            }
        }

        if errors.is_empty() {
            Ok(TypeContext {
                functions,
                structs,
                enums,
            })
        } else {
            Err(errors)
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
                let variants = enum_info.variants.clone();
                if let Some((_, variant_info)) = variants.iter().find(|(n, _)| n == variant) {
                    match (variant_info, args) {
                        (VariantInfo::Tuple(types), ast::EnumPatternArgs::Tuple(pats)) => {
                            for (pat, t) in pats.iter().zip(types.iter()) {
                                collect_bindings_inner(ctx, pat, t, bindings);
                            }
                        }
                        (VariantInfo::Struct(fields), ast::EnumPatternArgs::Struct(pat_fields)) => {
                            for (field_name, field_pat) in pat_fields {
                                if let Some((_, field_type)) =
                                    fields.iter().find(|(n, _)| n == field_name)
                                {
                                    collect_bindings_inner(ctx, field_pat, field_type, bindings);
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

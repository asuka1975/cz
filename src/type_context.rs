use crate::ast::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct FuncInfo {
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

pub struct TypeContext {
    pub functions: HashMap<String, FuncInfo>,
    pub structs: HashMap<String, StructInfo>,
    pub enums: HashMap<String, EnumInfo>,
}

impl TypeContext {
    pub fn build(program: &Program) -> Result<TypeContext, Vec<String>> {
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
                    param_count: 1,
                    param_types: vec![param_type],
                    return_type: Type::Unit,
                },
            );
        }

        // Register structs
        for s in &program.structs {
            if structs.contains_key(&s.name) {
                errors.push(format!(
                    "{}行目: 構造体 '{}' は既に定義されています",
                    s.line, s.name
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
            if enums.contains_key(&e.name) || structs.contains_key(&e.name) {
                errors.push(format!(
                    "{}行目: 列挙型 '{}' は既に定義されています",
                    e.line, e.name
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
            if functions.contains_key(&func.name) {
                errors.push(format!(
                    "{}行目: 関数 '{}' は既に定義されています",
                    func.line, func.name
                ));
            } else {
                functions.insert(
                    func.name.clone(),
                    FuncInfo {
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
}

/// パターンから変数バインディングを収集し、コールバックで適用する共有ユーティリティ。
///
/// `type_resolver::bind_pattern_vars` と `semantic::bind_pattern_vars` の
/// 共通ロジックを抽出したもの。バインディングを一旦 Vec に収集してから
/// コールバックを呼ぶことで、借用の問題を回避する。
pub fn collect_pattern_bindings(
    ctx: &TypeContext,
    pattern: &Pattern,
    ty: &Type,
) -> Vec<(String, Type)> {
    let mut bindings = Vec::new();
    collect_pattern_bindings_inner(ctx, pattern, ty, &mut bindings);
    bindings
}

fn collect_pattern_bindings_inner(
    ctx: &TypeContext,
    pattern: &Pattern,
    ty: &Type,
    bindings: &mut Vec<(String, Type)>,
) {
    match pattern {
        Pattern::Binding(name) => {
            bindings.push((name.clone(), ty.clone()));
        }
        Pattern::Tuple(patterns) => {
            if let Type::Tuple(types) = ty {
                for (pat, t) in patterns.iter().zip(types.iter()) {
                    collect_pattern_bindings_inner(ctx, pat, t, bindings);
                }
            }
        }
        Pattern::Struct { name, fields } => {
            if let Some(struct_info) = ctx.structs.get(name) {
                let struct_fields = struct_info.fields.clone();
                for (field_name, field_pat) in fields {
                    if let Some((_, field_type)) =
                        struct_fields.iter().find(|(n, _)| n == field_name)
                    {
                        collect_pattern_bindings_inner(ctx, field_pat, field_type, bindings);
                    }
                }
            }
        }
        Pattern::Enum {
            enum_name,
            variant,
            args,
        } => {
            if let Some(enum_info) = ctx.enums.get(enum_name) {
                let variants = enum_info.variants.clone();
                if let Some((_, variant_info)) = variants.iter().find(|(n, _)| n == variant) {
                    match (variant_info, args) {
                        (VariantInfo::Tuple(types), EnumPatternArgs::Tuple(pats)) => {
                            for (pat, t) in pats.iter().zip(types.iter()) {
                                collect_pattern_bindings_inner(ctx, pat, t, bindings);
                            }
                        }
                        (VariantInfo::Struct(fields), EnumPatternArgs::Struct(pat_fields)) => {
                            for (field_name, field_pat) in pat_fields {
                                if let Some((_, field_type)) =
                                    fields.iter().find(|(n, _)| n == field_name)
                                {
                                    collect_pattern_bindings_inner(
                                        ctx, field_pat, field_type, bindings,
                                    );
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

pub type ExprId = usize;

pub struct TypeMap {
    types: HashMap<ExprId, Type>,
}

impl TypeMap {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn set(&mut self, expr: &Expr, ty: Type) {
        self.types.insert(expr as *const Expr as usize, ty);
    }

    pub fn get(&self, expr: &Expr) -> &Type {
        self.types
            .get(&(expr as *const Expr as usize))
            .expect("type not resolved")
    }
}

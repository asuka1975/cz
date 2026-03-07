use crate::hir::types::Type;
use crate::syntax::ast::{FieldDef, VariantKind};

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::BasicTypeEnum;

use super::CodeGenError;

/// Cz の Type から LLVM の BasicTypeEnum への変換。
pub(crate) fn llvm_type<'ctx>(
    context: &'ctx Context,
    module: &Module<'ctx>,
    ty: &Type,
) -> Result<BasicTypeEnum<'ctx>, CodeGenError> {
    match ty {
        Type::I8 => Ok(context.i8_type().into()),
        Type::I16 => Ok(context.i16_type().into()),
        Type::I32 => Ok(context.i32_type().into()),
        Type::I64 => Ok(context.i64_type().into()),
        Type::F32 => Ok(context.f32_type().into()),
        Type::F64 => Ok(context.f64_type().into()),
        Type::Bool => Ok(context.bool_type().into()),
        Type::Unit => Ok(context.struct_type(&[], false).into()),
        Type::Tuple(types) => {
            let mut field_types: Vec<BasicTypeEnum> = Vec::new();
            for t in types {
                field_types.push(llvm_type(context, module, t)?);
            }
            Ok(context.struct_type(&field_types, false).into())
        }
        Type::Named(name) => {
            if let Some(struct_type) = module.get_struct_type(&format!("cz.{}", name)) {
                Ok(struct_type.into())
            } else {
                Err(CodeGenError::UndefinedType(name.clone()))
            }
        }
        Type::Error => unreachable!("Type::Error reached codegen"),
    }
}

/// 型のバイトサイズを計算する (enum ペイロードサイズ計算用)。
pub(crate) fn type_size(
    ty: &Type,
    struct_types: &std::collections::HashMap<String, Vec<(String, Type)>>,
    enum_types: &std::collections::HashMap<String, Vec<(String, VariantKind)>>,
) -> u64 {
    match ty {
        Type::I8 | Type::Bool => 1,
        Type::I16 => 2,
        Type::I32 | Type::F32 => 4,
        Type::I64 | Type::F64 => 8,
        Type::Unit => 0,
        Type::Tuple(types) => types
            .iter()
            .map(|t| type_size(t, struct_types, enum_types))
            .sum(),
        Type::Named(name) => {
            if let Some(fields) = struct_types.get(name) {
                fields
                    .iter()
                    .map(|(_, t)| type_size(t, struct_types, enum_types))
                    .sum()
            } else if let Some(variants) = enum_types.get(name) {
                let max_payload: u64 = variants
                    .iter()
                    .map(|(_, kind)| match kind {
                        VariantKind::Unit => 0,
                        VariantKind::Tuple(types) => types
                            .iter()
                            .map(|t| type_size(t, struct_types, enum_types))
                            .sum(),
                        VariantKind::Struct(fields) => fields
                            .iter()
                            .map(|f| type_size(&f.field_type, struct_types, enum_types))
                            .sum(),
                    })
                    .max()
                    .unwrap_or(0);
                4 + max_payload
            } else {
                0
            }
        }
        Type::Error => unreachable!("Type::Error reached codegen"),
    }
}

/// 整数型のビット幅を返す。
pub(crate) fn int_bit_width(ty: &Type) -> u32 {
    match ty {
        Type::I8 => 8,
        Type::I16 => 16,
        Type::I32 => 32,
        Type::I64 => 64,
        _ => 32,
    }
}

/// 浮動小数点型のビット幅を返す。
pub(crate) fn float_bit_width(ty: &Type) -> u32 {
    match ty {
        Type::F32 => 32,
        Type::F64 => 64,
        _ => 64,
    }
}

/// enum バリアントの構造体フィールドからオフセットを計算する。
pub(crate) fn enum_field_offset(
    fields: &[FieldDef],
    target: &str,
    struct_types: &std::collections::HashMap<String, Vec<(String, Type)>>,
    enum_types: &std::collections::HashMap<String, Vec<(String, VariantKind)>>,
) -> Option<(u64, Type)> {
    let mut offset: u64 = 0;
    for f in fields {
        if f.name == target {
            return Some((offset, f.field_type.clone()));
        }
        offset += type_size(&f.field_type, struct_types, enum_types);
    }
    None
}

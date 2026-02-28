use crate::hir::VarId;
use crate::hir::types::Type;
use crate::syntax::ast::{EnumPatternArgs, Pattern, VariantKind};

use inkwell::IntPredicate;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicValueEnum, IntValue, PointerValue};

use super::CodeGen;
use super::types::{enum_field_offset, type_size};

impl<'ctx> CodeGen<'ctx> {
    /// パターンがマッチするか検査する条件値 (i1) を生成する。
    pub(crate) fn emit_pattern_test(
        &mut self,
        pattern: &Pattern,
        val: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> IntValue<'ctx> {
        let true_val = self.context.bool_type().const_int(1, false);
        match pattern {
            Pattern::Wildcard | Pattern::Binding(_) => true_val,
            Pattern::IntLiteral(n) => {
                let int_val = val.into_int_value();
                let const_val = int_val.get_type().const_int(*n as u64, true);
                self.builder
                    .build_int_compare(IntPredicate::EQ, int_val, const_val, "pat.int")
                    .unwrap()
            }
            Pattern::BoolLiteral(b) => {
                let bool_val = val.into_int_value();
                let const_val = self
                    .context
                    .bool_type()
                    .const_int(if *b { 1 } else { 0 }, false);
                self.builder
                    .build_int_compare(IntPredicate::EQ, bool_val, const_val, "pat.bool")
                    .unwrap()
            }
            Pattern::Range { start, end } => {
                let int_val = val.into_int_value();
                let start_val = int_val.get_type().const_int(*start as u64, true);
                let end_val = int_val.get_type().const_int(*end as u64, true);
                let ge = self
                    .builder
                    .build_int_compare(IntPredicate::SGE, int_val, start_val, "range.ge")
                    .unwrap();
                let le = self
                    .builder
                    .build_int_compare(IntPredicate::SLE, int_val, end_val, "range.le")
                    .unwrap();
                self.builder.build_and(ge, le, "range.and").unwrap()
            }
            Pattern::Tuple(patterns) => {
                if let Type::Tuple(types) = ty {
                    let struct_val = val.into_struct_value();
                    let mut result = true_val;
                    for (i, (pat, elem_ty)) in patterns.iter().zip(types.iter()).enumerate() {
                        let elem = self
                            .builder
                            .build_extract_value(struct_val, i as u32, "tuple.elem")
                            .unwrap();
                        let test = self.emit_pattern_test(pat, elem, elem_ty);
                        result = self.builder.build_and(result, test, "pat.and").unwrap();
                    }
                    result
                } else {
                    true_val
                }
            }
            Pattern::Struct { name, fields } => {
                if let Some(struct_info) = self.struct_types.get(name).cloned() {
                    let struct_val = val.into_struct_value();
                    let mut result = true_val;
                    for (field_name, field_pat) in fields {
                        if let Some((idx, (_, field_type))) = struct_info
                            .iter()
                            .enumerate()
                            .find(|(_, (n, _))| n == field_name)
                        {
                            let field_val = self
                                .builder
                                .build_extract_value(struct_val, idx as u32, "struct.field")
                                .unwrap();
                            let test = self.emit_pattern_test(field_pat, field_val, field_type);
                            result = self.builder.build_and(result, test, "pat.and").unwrap();
                        }
                    }
                    result
                } else {
                    true_val
                }
            }
            Pattern::Enum {
                enum_name,
                variant,
                args,
            } => {
                if let Some(variants) = self.enum_types.get(enum_name).cloned() {
                    let variant_idx = variants.iter().position(|(n, _)| n == variant).unwrap();

                    let enum_llvm_type = self.llvm_type(&Type::Named(enum_name.clone())).unwrap();
                    let enum_addr = self
                        .builder
                        .build_alloca(enum_llvm_type, "enum.tmp")
                        .unwrap();
                    self.builder.build_store(enum_addr, val).unwrap();

                    let tag = self.get_enum_tag(enum_llvm_type, enum_addr);
                    let i32_type = self.context.i32_type();
                    let expected_tag = i32_type.const_int(variant_idx as u64, false);
                    let tag_match = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, tag, expected_tag, "tag.cmp")
                        .unwrap();

                    let variant_kind = &variants[variant_idx].1;
                    match (variant_kind, args) {
                        (VariantKind::Unit, _) => tag_match,
                        (VariantKind::Tuple(types), EnumPatternArgs::Tuple(pats)) => {
                            let payload_ptr = self.get_enum_payload_ptr(enum_llvm_type, enum_addr);
                            let mut result = tag_match;
                            let mut offset: u64 = 0;
                            for (pat, field_type) in pats.iter().zip(types.iter()) {
                                let field_llvm_type = self.llvm_type(field_type).unwrap();
                                let field_ptr = self.get_enum_field_ptr(payload_ptr, offset);
                                let field_val = self
                                    .builder
                                    .build_load(field_llvm_type, field_ptr, "field.val")
                                    .unwrap();
                                let test = self.emit_pattern_test(pat, field_val, field_type);
                                result = self.builder.build_and(result, test, "pat.and").unwrap();
                                offset +=
                                    type_size(field_type, &self.struct_types, &self.enum_types);
                            }
                            result
                        }
                        (VariantKind::Struct(fields), EnumPatternArgs::Struct(pat_fields)) => {
                            let payload_ptr = self.get_enum_payload_ptr(enum_llvm_type, enum_addr);
                            let mut result = tag_match;
                            for (pat_field_name, field_pat) in pat_fields {
                                if let Some((offset, field_type)) = enum_field_offset(
                                    fields,
                                    pat_field_name,
                                    &self.struct_types,
                                    &self.enum_types,
                                ) {
                                    let field_llvm_type = self.llvm_type(&field_type).unwrap();
                                    let field_ptr = self.get_enum_field_ptr(payload_ptr, offset);
                                    let field_val = self
                                        .builder
                                        .build_load(field_llvm_type, field_ptr, "field.val")
                                        .unwrap();
                                    let test =
                                        self.emit_pattern_test(field_pat, field_val, &field_type);
                                    result =
                                        self.builder.build_and(result, test, "pat.and").unwrap();
                                }
                            }
                            result
                        }
                        _ => tag_match,
                    }
                } else {
                    true_val
                }
            }
        }
    }

    /// パターンマッチのバインディングを生成する。
    /// lowering が `collect_pattern_bindings` と同じ順序で VarId を割り当てているため、
    /// 同じ順序でパターンを走査し、binding_var_ids イテレータから VarId を取得する。
    pub(crate) fn emit_pattern_bindings(
        &mut self,
        pattern: &Pattern,
        val: BasicValueEnum<'ctx>,
        ty: &Type,
        binding_var_ids: &mut std::slice::Iter<'_, VarId>,
    ) {
        match pattern {
            Pattern::Binding(_) => {
                if let Some(&var_id) = binding_var_ids.next() {
                    let llvm_ty = self.llvm_type(ty).unwrap();
                    let var_name = &self.vars[var_id.0 as usize].name;
                    let alloca = self.builder.build_alloca(llvm_ty, var_name).unwrap();
                    self.builder.build_store(alloca, val).unwrap();
                    self.var_ptrs[var_id.0 as usize] = Some(alloca);
                }
            }
            Pattern::Tuple(patterns) => {
                if let Type::Tuple(types) = ty {
                    let struct_val = val.into_struct_value();
                    for (i, (pat, elem_ty)) in patterns.iter().zip(types.iter()).enumerate() {
                        let elem = self
                            .builder
                            .build_extract_value(struct_val, i as u32, "tuple.elem")
                            .unwrap();
                        self.emit_pattern_bindings(pat, elem, elem_ty, binding_var_ids);
                    }
                }
            }
            Pattern::Struct { name, fields } => {
                if let Some(struct_info) = self.struct_types.get(name).cloned() {
                    let struct_val = val.into_struct_value();
                    for (field_name, field_pat) in fields {
                        if let Some((idx, (_, field_type))) = struct_info
                            .iter()
                            .enumerate()
                            .find(|(_, (n, _))| n == field_name)
                        {
                            let field_val = self
                                .builder
                                .build_extract_value(struct_val, idx as u32, "struct.field")
                                .unwrap();
                            self.emit_pattern_bindings(
                                field_pat,
                                field_val,
                                field_type,
                                binding_var_ids,
                            );
                        }
                    }
                }
            }
            Pattern::Enum {
                enum_name,
                variant,
                args,
            } => {
                if let Some(variants) = self.enum_types.get(enum_name).cloned() {
                    let variant_idx = variants.iter().position(|(n, _)| n == variant).unwrap();
                    let variant_kind = &variants[variant_idx].1;

                    let enum_llvm_type = self.llvm_type(&Type::Named(enum_name.clone())).unwrap();
                    let enum_addr = self
                        .builder
                        .build_alloca(enum_llvm_type, "enum.bind")
                        .unwrap();
                    self.builder.build_store(enum_addr, val).unwrap();

                    match (variant_kind, args) {
                        (VariantKind::Tuple(types), EnumPatternArgs::Tuple(pats)) => {
                            let payload_ptr = self.get_enum_payload_ptr(enum_llvm_type, enum_addr);
                            let mut offset: u64 = 0;
                            for (pat, field_type) in pats.iter().zip(types.iter()) {
                                let field_llvm_type = self.llvm_type(field_type).unwrap();
                                let field_ptr = self.get_enum_field_ptr(payload_ptr, offset);
                                let field_val = self
                                    .builder
                                    .build_load(field_llvm_type, field_ptr, "field.val")
                                    .unwrap();
                                self.emit_pattern_bindings(
                                    pat,
                                    field_val,
                                    field_type,
                                    binding_var_ids,
                                );
                                offset +=
                                    type_size(field_type, &self.struct_types, &self.enum_types);
                            }
                        }
                        (VariantKind::Struct(def_fields), EnumPatternArgs::Struct(pat_fields)) => {
                            let payload_ptr = self.get_enum_payload_ptr(enum_llvm_type, enum_addr);
                            for (pat_field_name, field_pat) in pat_fields {
                                if let Some((offset, field_type)) = enum_field_offset(
                                    def_fields,
                                    pat_field_name,
                                    &self.struct_types,
                                    &self.enum_types,
                                ) {
                                    let field_llvm_type = self.llvm_type(&field_type).unwrap();
                                    let field_ptr = self.get_enum_field_ptr(payload_ptr, offset);
                                    let field_val = self
                                        .builder
                                        .build_load(field_llvm_type, field_ptr, "field.val")
                                        .unwrap();
                                    self.emit_pattern_bindings(
                                        field_pat,
                                        field_val,
                                        &field_type,
                                        binding_var_ids,
                                    );
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }

    // --- Enum helpers ---

    /// enum 値が格納されたアドレスからタグ値 (i32) を読み出す。
    pub(crate) fn get_enum_tag(
        &self,
        enum_llvm_type: BasicTypeEnum<'ctx>,
        enum_addr: PointerValue<'ctx>,
    ) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        let tag_ptr = self
            .builder
            .build_struct_gep(enum_llvm_type, enum_addr, 0, "tag.ptr")
            .unwrap();
        self.builder
            .build_load(i32_type, tag_ptr, "tag")
            .unwrap()
            .into_int_value()
    }

    /// enum 値が格納されたアドレスからペイロード領域のポインタを取得する。
    pub(crate) fn get_enum_payload_ptr(
        &self,
        enum_llvm_type: BasicTypeEnum<'ctx>,
        enum_addr: PointerValue<'ctx>,
    ) -> PointerValue<'ctx> {
        self.builder
            .build_struct_gep(enum_llvm_type, enum_addr, 1, "payload.ptr")
            .unwrap()
    }

    /// ペイロードポインタにバイトオフセットを加算してフィールドポインタを取得する。
    pub(crate) fn get_enum_field_ptr(
        &self,
        payload_ptr: PointerValue<'ctx>,
        offset: u64,
    ) -> PointerValue<'ctx> {
        unsafe {
            self.builder
                .build_gep(
                    self.context.i8_type(),
                    payload_ptr,
                    &[self.context.i64_type().const_int(offset, false)],
                    "field.ptr",
                )
                .unwrap()
        }
    }
}

/// パターンからバインディング変数の VarId を collect_pattern_bindings と同じ
/// 走査順序で収集する。lowering が同じ順序で VarId を割り当てているため、
/// この関数で得た名前リストに対応する VarId を vars テーブルから引ける。
pub(crate) fn collect_binding_names_from_pattern(pattern: &Pattern) -> Vec<String> {
    let mut names = Vec::new();
    collect_binding_names_inner(pattern, &mut names);
    names
}

fn collect_binding_names_inner(pattern: &Pattern, names: &mut Vec<String>) {
    match pattern {
        Pattern::Binding(name) => {
            names.push(name.clone());
        }
        Pattern::Tuple(patterns) => {
            for pat in patterns {
                collect_binding_names_inner(pat, names);
            }
        }
        Pattern::Struct { fields, .. } => {
            for (_field_name, field_pat) in fields {
                collect_binding_names_inner(field_pat, names);
            }
        }
        Pattern::Enum { args, .. } => match args {
            EnumPatternArgs::Tuple(pats) => {
                for pat in pats {
                    collect_binding_names_inner(pat, names);
                }
            }
            EnumPatternArgs::Struct(fields) => {
                for (_field_name, field_pat) in fields {
                    collect_binding_names_inner(field_pat, names);
                }
            }
            EnumPatternArgs::Unit => {}
        },
        _ => {}
    }
}

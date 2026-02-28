pub mod expr;
pub mod pattern;
pub mod stmt;
pub mod types;

use crate::arena::Arena;
use crate::hir::types::{Type, TypeContext, VariantInfo};
use crate::hir::*;
use crate::syntax::ast::{FieldDef, VariantKind};

use inkwell::AddressSpace;
use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use std::collections::HashMap;
use std::path::Path;

// --- Error type ---

#[derive(Debug)]
pub enum CodeGenError {
    UndefinedType(String),
    UndefinedVariable(String),
    UndefinedFunction(String),
    #[allow(dead_code)]
    FieldNotFound {
        struct_name: String,
        field: String,
    },
    #[allow(dead_code)]
    NotAStructType(String),
    LabelNotFound(String),
}

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UndefinedType(name) => write!(f, "codegen: 未定義の型: {}", name),
            Self::UndefinedVariable(name) => write!(f, "codegen: 未定義の変数: {}", name),
            Self::UndefinedFunction(name) => write!(f, "codegen: 未定義の関数: {}", name),
            Self::FieldNotFound { struct_name, field } => {
                write!(
                    f,
                    "codegen: 構造体 '{}' にフィールド '{}' が見つかりません",
                    struct_name, field
                )
            }
            Self::NotAStructType(ty) => {
                write!(f, "codegen: 構造体でない型へのフィールドアクセス: {}", ty)
            }
            Self::LabelNotFound(label) => {
                write!(f, "codegen: ラベル '{}' が見つかりません", label)
            }
        }
    }
}

// --- Loop tracking ---

pub(crate) struct LoopInfo<'ctx> {
    break_bb: inkwell::basic_block::BasicBlock<'ctx>,
    cond_bb: inkwell::basic_block::BasicBlock<'ctx>,
    result_addr: Option<PointerValue<'ctx>>,
    label: Option<String>,
}

// --- Main CodeGen struct ---

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    hir_exprs: Arena<HirExpr>,
    hir_stmts: Arena<HirStmt>,
    vars: Vec<VarInfo>,
    func_names: Vec<String>,
    ctx: TypeContext,
    var_ptrs: Vec<Option<PointerValue<'ctx>>>,
    func_values: Vec<Option<FunctionValue<'ctx>>>,
    printf_fn: FunctionValue<'ctx>,
    exit_fn: FunctionValue<'ctx>,
    div_zero_msg: PointerValue<'ctx>,
    struct_types: HashMap<String, Vec<(String, Type)>>,
    enum_types: HashMap<String, Vec<(String, VariantKind)>>,
    loop_stack: Vec<LoopInfo<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(
        context: &'ctx Context,
        hir_exprs: Arena<HirExpr>,
        hir_stmts: Arena<HirStmt>,
        vars: Vec<VarInfo>,
        func_names: Vec<String>,
        ctx: TypeContext,
    ) -> Self {
        let module = context.create_module("czc");
        let builder = context.create_builder();

        let i32_type = context.i32_type();
        let ptr_type = context.ptr_type(AddressSpace::default());
        let void_type = context.void_type();

        // Declare external functions
        let printf_type = i32_type.fn_type(&[ptr_type.into()], true);
        let printf_fn = module.add_function("printf", printf_type, Some(Linkage::External));

        let exit_type = void_type.fn_type(&[i32_type.into()], false);
        let exit_fn = module.add_function("exit", exit_type, Some(Linkage::External));

        // Global string constants
        let div_zero_str = context.const_string(b"runtime error: division by zero\n", true);
        let div_zero_global = module.add_global(div_zero_str.get_type(), None, ".div_zero_msg");
        div_zero_global.set_initializer(&div_zero_str);
        div_zero_global.set_constant(true);
        div_zero_global.set_linkage(Linkage::Private);

        let var_count = vars.len();
        let func_count = func_names.len();

        Self {
            context,
            module,
            builder,
            hir_exprs,
            hir_stmts,
            vars,
            func_names,
            ctx,
            var_ptrs: vec![None; var_count],
            func_values: vec![None; func_count],
            printf_fn,
            exit_fn,
            div_zero_msg: div_zero_global.as_pointer_value(),
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            loop_stack: Vec::new(),
        }
    }

    pub fn generate(&mut self, program: &HirProgram) -> Result<(), CodeGenError> {
        // Register struct types
        for name in &program.struct_order {
            if let Some(struct_info) = self.ctx.structs.get(name) {
                let mut field_types: Vec<BasicTypeEnum> = Vec::new();
                for (_, ty) in &struct_info.fields {
                    field_types.push(self.llvm_type(ty)?);
                }
                let struct_type = self.context.opaque_struct_type(&format!("cz.{}", name));
                struct_type.set_body(&field_types, false);
                self.struct_types.insert(
                    name.clone(),
                    struct_info
                        .fields
                        .iter()
                        .map(|(n, t)| (n.clone(), t.clone()))
                        .collect(),
                );
            }
        }

        // Register enum types
        for name in &program.enum_order {
            if let Some(enum_info) = self.ctx.enums.get(name) {
                let mut max_payload_size: u64 = 0;
                for (_, variant_info) in &enum_info.variants {
                    let payload_size = match variant_info {
                        VariantInfo::Unit => 0,
                        VariantInfo::Tuple(types) => types
                            .iter()
                            .map(|t| types::type_size(t, &self.struct_types, &self.enum_types))
                            .sum::<u64>(),
                        VariantInfo::Struct(fields) => fields
                            .iter()
                            .map(|(_, t)| types::type_size(t, &self.struct_types, &self.enum_types))
                            .sum::<u64>(),
                    };
                    if payload_size > max_payload_size {
                        max_payload_size = payload_size;
                    }
                }
                let i32_type = self.context.i32_type();
                let payload_type = self.context.i8_type().array_type(max_payload_size as u32);
                let enum_struct = self.context.opaque_struct_type(&format!("cz.{}", name));
                enum_struct.set_body(&[i32_type.into(), payload_type.into()], false);
                self.enum_types.insert(
                    name.clone(),
                    enum_info
                        .variants
                        .iter()
                        .map(|(vname, vinfo)| {
                            let kind = match vinfo {
                                VariantInfo::Unit => VariantKind::Unit,
                                VariantInfo::Tuple(types) => VariantKind::Tuple(types.clone()),
                                VariantInfo::Struct(fields) => VariantKind::Struct(
                                    fields
                                        .iter()
                                        .map(|(n, t)| FieldDef {
                                            name: n.clone(),
                                            field_type: t.clone(),
                                        })
                                        .collect(),
                                ),
                            };
                            (vname.clone(), kind)
                        })
                        .collect(),
                );
            }
        }

        // Generate built-in print functions
        self.generate_print_functions();

        // Pass 1: Register all function signatures
        for func in &program.functions {
            let c_name = Self::mangle(&func.name);
            if self.module.get_function(&c_name).is_none() {
                let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::new();
                for &param_var_id in &func.params {
                    let param_ty = &self.vars[param_var_id.0 as usize].ty;
                    param_types.push(self.llvm_type(param_ty)?.into());
                }
                let fn_value = if func.return_type == Type::Unit {
                    let fn_type = self.context.void_type().fn_type(&param_types, false);
                    self.module.add_function(&c_name, fn_type, None)
                } else {
                    let ret_type = self.llvm_type(&func.return_type)?;
                    let fn_type = ret_type.fn_type(&param_types, false);
                    self.module.add_function(&c_name, fn_type, None)
                };
                self.func_values[func.func_id.0 as usize] = Some(fn_value);
            }
        }

        // Pass 2: Generate function bodies
        for func in &program.functions {
            self.emit_function(func)?;
        }

        Ok(())
    }

    fn generate_print_functions(&mut self) {
        let i32_type = self.context.i32_type();

        let fmt_pairs: Vec<(&str, &str, &[u8], Type)> = vec![
            ("cz_print_i8", ".fmt_i8", b"%d\n", Type::I8),
            ("cz_print_i16", ".fmt_i16", b"%d\n", Type::I16),
            ("cz_print_i32", ".fmt_i32", b"%d\n", Type::I32),
            ("cz_print_i64", ".fmt_i64", b"%ld\n", Type::I64),
            ("cz_print_f32", ".fmt_f32", b"%g\n", Type::F32),
            ("cz_print_f64", ".fmt_f64", b"%g\n", Type::F64),
        ];

        for (fn_name, fmt_name, fmt_bytes, param_type) in &fmt_pairs {
            let fmt_str = self.context.const_string(fmt_bytes, true);
            let fmt_global = self.module.add_global(fmt_str.get_type(), None, fmt_name);
            fmt_global.set_initializer(&fmt_str);
            fmt_global.set_constant(true);
            fmt_global.set_linkage(Linkage::Private);

            let llvm_param_type = self.llvm_type(param_type).unwrap();
            let fn_type = self
                .context
                .void_type()
                .fn_type(&[llvm_param_type.into()], false);
            let func = self.module.add_function(fn_name, fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let param_val = func.get_nth_param(0).unwrap();

            let print_val: BasicMetadataValueEnum = match param_type {
                Type::I8 | Type::I16 => {
                    let ext = self
                        .builder
                        .build_int_s_extend(param_val.into_int_value(), i32_type, "ext")
                        .unwrap();
                    ext.into()
                }
                Type::F32 => {
                    let ext = self
                        .builder
                        .build_float_ext(
                            param_val.into_float_value(),
                            self.context.f64_type(),
                            "ext",
                        )
                        .unwrap();
                    ext.into()
                }
                _ => param_val.into(),
            };

            self.builder
                .build_call(
                    self.printf_fn,
                    &[fmt_global.as_pointer_value().into(), print_val],
                    "printf_call",
                )
                .unwrap();
            self.builder.build_return(None).unwrap();

            // Register the print function's FuncId if it has one
            if let Some(pos) = self.func_names.iter().position(|n| {
                n == &fn_name[3..] // "cz_print_i32" -> "print_i32"
            }) {
                self.func_values[pos] = Some(func);
            }
        }

        // print_bool: outputs "true\n" or "false\n"
        let true_str = self.context.const_string(b"true\n", true);
        let true_global = self
            .module
            .add_global(true_str.get_type(), None, ".true_str");
        true_global.set_initializer(&true_str);
        true_global.set_constant(true);
        true_global.set_linkage(Linkage::Private);

        let false_str = self.context.const_string(b"false\n", true);
        let false_global = self
            .module
            .add_global(false_str.get_type(), None, ".false_str");
        false_global.set_initializer(&false_str);
        false_global.set_constant(true);
        false_global.set_linkage(Linkage::Private);

        let i1_type = self.context.bool_type();
        let fn_type = self.context.void_type().fn_type(&[i1_type.into()], false);
        let print_bool_fn = self.module.add_function("cz_print_bool", fn_type, None);
        let entry = self.context.append_basic_block(print_bool_fn, "entry");
        let then_bb = self.context.append_basic_block(print_bool_fn, "then");
        let else_bb = self.context.append_basic_block(print_bool_fn, "else");
        let merge_bb = self.context.append_basic_block(print_bool_fn, "merge");

        self.builder.position_at_end(entry);
        let bool_val = print_bool_fn.get_nth_param(0).unwrap().into_int_value();
        self.builder
            .build_conditional_branch(bool_val, then_bb, else_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        self.builder
            .build_call(
                self.printf_fn,
                &[true_global.as_pointer_value().into()],
                "printf_call",
            )
            .unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(else_bb);
        self.builder
            .build_call(
                self.printf_fn,
                &[false_global.as_pointer_value().into()],
                "printf_call",
            )
            .unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(merge_bb);
        self.builder.build_return(None).unwrap();

        // Register print_bool's FuncId
        if let Some(pos) = self.func_names.iter().position(|n| n == "print_bool") {
            self.func_values[pos] = Some(print_bool_fn);
        }
    }

    fn emit_function(&mut self, func: &HirFunctionDef) -> Result<(), CodeGenError> {
        let fn_value = self.func_values[func.func_id.0 as usize].unwrap();

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        // Alloca and store parameters
        for (i, &param_var_id) in func.params.iter().enumerate() {
            let param_val = fn_value.get_nth_param(i as u32).unwrap();
            let var_info = &self.vars[param_var_id.0 as usize];
            let llvm_ty = self.llvm_type(&var_info.ty)?;
            let alloca = self.builder.build_alloca(llvm_ty, &var_info.name).unwrap();
            self.builder.build_store(alloca, param_val).unwrap();
            self.var_ptrs[param_var_id.0 as usize] = Some(alloca);
        }

        // Clone block data to avoid borrow conflicts
        let body_stmts = func.body.stmts.clone();
        let body_expr = func.body.expr;
        let return_type = func.return_type.clone();
        let body = HirBlock {
            stmts: body_stmts,
            expr: body_expr,
        };

        self.emit_block_as_return(&body, &return_type)?;
        Ok(())
    }

    // --- Public API ---

    pub fn print_to_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn write_object_file(&self, path: &Path) -> Result<(), String> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("ターゲット初期化に失敗: {}", e))?;

        let triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&triple)
            .map_err(|e| format!("ターゲット取得に失敗: {}", e.to_string()))?;

        let target_machine = target
            .create_target_machine(
                &triple,
                "generic",
                "",
                OptimizationLevel::None,
                RelocMode::Default,
                CodeModel::Default,
            )
            .ok_or("ターゲットマシン作成に失敗".to_string())?;

        target_machine
            .write_to_file(&self.module, FileType::Object, path)
            .map_err(|e| format!("オブジェクトファイル書き出しに失敗: {}", e.to_string()))
    }

    // --- Helpers ---

    fn mangle(name: &str) -> String {
        if name == "main" {
            "main".to_string()
        } else {
            format!("cz_{}", name)
        }
    }

    pub(crate) fn llvm_type(&self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, CodeGenError> {
        types::llvm_type(self.context, &self.module, ty)
    }

    pub(crate) fn unit_value(&self) -> BasicValueEnum<'ctx> {
        self.context
            .struct_type(&[], false)
            .const_named_struct(&[])
            .into()
    }

    pub(crate) fn current_function(&self) -> FunctionValue<'ctx> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
    }

    /// match arm のパターンバインディングに対応する VarId を検索する。
    /// lowering が collect_pattern_bindings と同じ順序で VarId を割り当てているため、
    /// 同じ名前と型をもつ VarId を vars テーブルから検索する。
    ///
    /// 検索は、まだ alloca が割り当てられていない (var_ptrs が None の) VarId を対象とし、
    /// lowering と同じ順序で割り当てられた想定で後方から逆検索しない。
    /// collect_pattern_bindings の結果順序で、vars テーブルから該当 VarId を見つける。
    pub(crate) fn find_match_binding_var_ids(
        &self,
        binding_names: &[String],
        _match_type: &Type,
    ) -> Vec<VarId> {
        let mut result = Vec::new();
        let mut used_indices = std::collections::HashSet::new();
        for name in binding_names {
            // Find the first VarId with this name that hasn't been claimed yet
            for (idx, var_info) in self.vars.iter().enumerate() {
                if var_info.name == *name
                    && self.var_ptrs[idx].is_none()
                    && !used_indices.contains(&idx)
                {
                    result.push(VarId(idx as u32));
                    used_indices.insert(idx);
                    break;
                }
            }
        }
        result
    }
}

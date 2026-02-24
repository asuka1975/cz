use crate::ast::*;
use crate::token::{FloatSuffix, IntSuffix};
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
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

struct LoopInfo<'ctx> {
    break_bb: inkwell::basic_block::BasicBlock<'ctx>,
    cond_bb: inkwell::basic_block::BasicBlock<'ctx>,
    result_addr: Option<PointerValue<'ctx>>,
    label: Option<String>,
}

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    vars: Vec<HashMap<String, (PointerValue<'ctx>, Type)>>,
    printf_fn: FunctionValue<'ctx>,
    exit_fn: FunctionValue<'ctx>,
    div_zero_msg: PointerValue<'ctx>,
    struct_types: HashMap<String, Vec<(String, Type)>>,
    enum_types: HashMap<String, Vec<(String, VariantKind)>>,
    loop_stack: Vec<LoopInfo<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
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

        Self {
            context,
            module,
            builder,
            vars: Vec::new(),
            printf_fn,
            exit_fn,
            div_zero_msg: div_zero_global.as_pointer_value(),
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            loop_stack: Vec::new(),
        }
    }

    pub fn generate(&mut self, program: &Program) {
        // Register struct types
        for s in &program.structs {
            let field_types: Vec<BasicTypeEnum> = s
                .fields
                .iter()
                .map(|f| self.llvm_type(&f.field_type))
                .collect();
            let struct_type = self.context.opaque_struct_type(&format!("cz.{}", s.name));
            struct_type.set_body(&field_types, false);
            self.struct_types.insert(
                s.name.clone(),
                s.fields
                    .iter()
                    .map(|f| (f.name.clone(), f.field_type.clone()))
                    .collect(),
            );
        }

        // Register enum types
        for e in &program.enums {
            let mut max_payload_size: u64 = 0;
            for v in &e.variants {
                let payload_size = match &v.kind {
                    VariantKind::Unit => 0,
                    VariantKind::Tuple(types) => {
                        types.iter().map(|t| self.type_size(t)).sum::<u64>()
                    }
                    VariantKind::Struct(fields) => fields
                        .iter()
                        .map(|f| self.type_size(&f.field_type))
                        .sum::<u64>(),
                };
                if payload_size > max_payload_size {
                    max_payload_size = payload_size;
                }
            }
            // enum = { i32 tag, [N x i8] payload }
            let i32_type = self.context.i32_type();
            let payload_type = self.context.i8_type().array_type(max_payload_size as u32);
            let enum_struct = self.context.opaque_struct_type(&format!("cz.{}", e.name));
            enum_struct.set_body(&[i32_type.into(), payload_type.into()], false);
            self.enum_types.insert(
                e.name.clone(),
                e.variants
                    .iter()
                    .map(|v| (v.name.clone(), v.kind.clone()))
                    .collect(),
            );
        }

        // Generate built-in print functions
        self.generate_print_functions();

        // Pass 1: Register all function signatures
        for func in &program.functions {
            let c_name = Self::mangle(&func.name);
            if self.module.get_function(&c_name).is_none() {
                let param_types: Vec<BasicMetadataTypeEnum> = func
                    .params
                    .iter()
                    .map(|p| self.llvm_type(&p.param_type).into())
                    .collect();
                if func.return_type == Type::Unit {
                    let fn_type = self.context.void_type().fn_type(&param_types, false);
                    self.module.add_function(&c_name, fn_type, None);
                } else {
                    let ret_type = self.llvm_type(&func.return_type);
                    let fn_type = ret_type.fn_type(&param_types, false);
                    self.module.add_function(&c_name, fn_type, None);
                }
            }
        }

        // Pass 2: Generate function bodies
        for func in &program.functions {
            self.emit_function(func);
        }
    }

    fn generate_print_functions(&mut self) {
        let i32_type = self.context.i32_type();

        // Format strings
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

            let llvm_param_type = self.llvm_type(param_type);
            let fn_type = self
                .context
                .void_type()
                .fn_type(&[llvm_param_type.into()], false);
            let func = self.module.add_function(fn_name, fn_type, None);
            let entry = self.context.append_basic_block(func, "entry");
            self.builder.position_at_end(entry);

            let param_val = func.get_nth_param(0).unwrap();

            // For i8/i16, extend to i32 for printf; for f32, extend to f64
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
    }

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

    fn llvm_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::I8 => self.context.i8_type().into(),
            Type::I16 => self.context.i16_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I64 => self.context.i64_type().into(),
            Type::F32 => self.context.f32_type().into(),
            Type::F64 => self.context.f64_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Unit => self.context.struct_type(&[], false).into(),
            Type::Tuple(types) => {
                let field_types: Vec<BasicTypeEnum> =
                    types.iter().map(|t| self.llvm_type(t)).collect();
                self.context.struct_type(&field_types, false).into()
            }
            Type::Named(name) => {
                if let Some(struct_type) = self.module.get_struct_type(&format!("cz.{}", name)) {
                    struct_type.into()
                } else {
                    panic!("codegen: undefined type: {}", name);
                }
            }
        }
    }

    fn type_size(&self, ty: &Type) -> u64 {
        match ty {
            Type::I8 | Type::Bool => 1,
            Type::I16 => 2,
            Type::I32 | Type::F32 => 4,
            Type::I64 | Type::F64 => 8,
            Type::Unit => 0,
            Type::Tuple(types) => types.iter().map(|t| self.type_size(t)).sum(),
            Type::Named(name) => {
                if let Some(fields) = self.struct_types.get(name) {
                    fields.iter().map(|(_, t)| self.type_size(t)).sum()
                } else if self.enum_types.contains_key(name) {
                    // enum size: tag(4) + payload
                    let variants = self.enum_types.get(name).unwrap();
                    let max_payload: u64 = variants
                        .iter()
                        .map(|(_, kind)| match kind {
                            VariantKind::Unit => 0,
                            VariantKind::Tuple(types) => {
                                types.iter().map(|t| self.type_size(t)).sum()
                            }
                            VariantKind::Struct(fields) => {
                                fields.iter().map(|f| self.type_size(&f.field_type)).sum()
                            }
                        })
                        .max()
                        .unwrap_or(0);
                    4 + max_payload
                } else {
                    0
                }
            }
        }
    }

    fn infer_type(&self, expr: &Expr) -> Type {
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
                for scope in self.vars.iter().rev() {
                    if let Some((_, ty)) = scope.get(name) {
                        return ty.clone();
                    }
                }
                Type::I32
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
                _ => self.infer_type(left),
            },
            Expr::UnaryOp { op, operand } => match op {
                UnaryOp::Neg => self.infer_type(operand),
                UnaryOp::Not => Type::Bool,
            },
            Expr::Cast { target_type, .. } => target_type.clone(),
            Expr::Call { name, .. } => {
                let c_name = Self::mangle(name);
                if let Some(func) = self.module.get_function(&c_name) {
                    if func.get_type().get_return_type().is_none() {
                        Type::Unit
                    } else {
                        // Try to match return type
                        self.llvm_type_to_type(func.get_type().get_return_type().unwrap())
                    }
                } else {
                    Type::I32
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
                        self.infer_type(tail)
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
                if let Some(first) = arms.first() {
                    self.infer_type(&first.body)
                } else {
                    Type::Unit
                }
            }
            Expr::Block(block) => {
                if let Some(tail) = &block.expr {
                    self.infer_type(tail)
                } else {
                    Type::Unit
                }
            }
            Expr::FieldAccess { expr, field } => {
                let base = self.infer_type(expr);
                if let Type::Named(name) = &base
                    && let Some(fields) = self.struct_types.get(name)
                {
                    for (fname, ftype) in fields {
                        if fname == field {
                            return ftype.clone();
                        }
                    }
                }
                Type::I32
            }
            Expr::TupleIndex { expr, index } => {
                let base = self.infer_type(expr);
                if let Type::Tuple(types) = &base
                    && (*index as usize) < types.len()
                {
                    return types[*index as usize].clone();
                }
                Type::I32
            }
            Expr::TupleExpr(elems) => {
                Type::Tuple(elems.iter().map(|e| self.infer_type(e)).collect())
            }
            Expr::StructExpr { name, .. } => Type::Named(name.clone()),
            Expr::EnumExpr { enum_name, .. } => Type::Named(enum_name.clone()),
        }
    }

    fn llvm_type_to_type(&self, ty: BasicTypeEnum<'ctx>) -> Type {
        if ty.is_int_type() {
            let width = ty.into_int_type().get_bit_width();
            match width {
                1 => Type::Bool,
                8 => Type::I8,
                16 => Type::I16,
                32 => Type::I32,
                64 => Type::I64,
                _ => Type::I32,
            }
        } else if ty.is_float_type() {
            // F32 or F64
            Type::F64
        } else {
            Type::I32
        }
    }

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    fn define_var(&mut self, name: &str, ptr: PointerValue<'ctx>, ty: Type) {
        if let Some(scope) = self.vars.last_mut() {
            scope.insert(name.to_string(), (ptr, ty));
        }
    }

    fn lookup_var(&self, name: &str) -> (PointerValue<'ctx>, Type) {
        for scope in self.vars.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return entry.clone();
            }
        }
        panic!("codegen: undefined variable: {}", name);
    }

    fn current_function(&self) -> FunctionValue<'ctx> {
        self.builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap()
    }

    fn is_float_type(ty: &Type) -> bool {
        matches!(ty, Type::F32 | Type::F64)
    }

    fn is_integer_type(ty: &Type) -> bool {
        matches!(ty, Type::I8 | Type::I16 | Type::I32 | Type::I64)
    }

    // --- Function/Block Emission ---

    fn emit_function(&mut self, func: &FunctionDef) {
        self.vars.clear();

        let c_name = Self::mangle(&func.name);
        let fn_value = self.module.get_function(&c_name).unwrap();

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        self.push_scope();

        for (i, param) in func.params.iter().enumerate() {
            let param_val = fn_value.get_nth_param(i as u32).unwrap();
            let llvm_ty = self.llvm_type(&param.param_type);
            let alloca = self.builder.build_alloca(llvm_ty, &param.name).unwrap();
            self.builder.build_store(alloca, param_val).unwrap();
            self.define_var(&param.name, alloca, param.param_type.clone());
        }

        self.emit_block_as_return(&func.body, &func.return_type);
        self.pop_scope();
    }

    fn emit_block_as_return(&mut self, block: &Block, return_type: &Type) {
        for stmt in &block.stmts {
            self.emit_stmt(stmt);
            // Check if we already emitted a terminator (return/break/continue)
            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_some()
            {
                // Rest of statements are dead code; still need to handle properly
                // Create a dead block to continue emitting into
                let current_fn = self.current_function();
                let dead_bb = self.context.append_basic_block(current_fn, "dead");
                self.builder.position_at_end(dead_bb);
            }
        }
        if let Some(expr) = &block.expr {
            if *return_type == Type::Unit {
                self.emit_expr(expr);
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    self.builder.build_return(None).unwrap();
                }
            } else {
                let val = self.emit_expr(expr);
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    self.builder.build_return(Some(&val)).unwrap();
                }
            }
        }
        let current_block = self.builder.get_insert_block().unwrap();
        if current_block.get_terminator().is_none() {
            if *return_type == Type::Unit {
                self.builder.build_return(None).unwrap();
            } else {
                self.builder.build_unreachable().unwrap();
            }
        }
    }

    fn emit_block_value(&mut self, block: &Block) -> BasicValueEnum<'ctx> {
        self.push_scope();
        for stmt in &block.stmts {
            self.emit_stmt(stmt);
            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_some()
            {
                let current_fn = self.current_function();
                let dead_bb = self.context.append_basic_block(current_fn, "dead");
                self.builder.position_at_end(dead_bb);
            }
        }
        let val = if let Some(expr) = &block.expr {
            self.emit_expr(expr)
        } else {
            self.unit_value()
        };
        self.pop_scope();
        val
    }

    fn unit_value(&self) -> BasicValueEnum<'ctx> {
        self.context
            .struct_type(&[], false)
            .const_named_struct(&[])
            .into()
    }

    // --- Statement Emission ---

    fn emit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                name,
                var_type,
                init,
                ..
            } => {
                let val = self.emit_expr(init);
                let ty = if let Some(declared_type) = var_type {
                    declared_type.clone()
                } else {
                    self.infer_type(init)
                };
                let llvm_ty = self.llvm_type(&ty);
                let alloca = self.builder.build_alloca(llvm_ty, name).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.define_var(name, alloca, ty);
            }
            Stmt::Return { value, .. } => {
                if let Some(val_expr) = value {
                    let val = self.emit_expr(val_expr);
                    self.builder.build_return(Some(&val)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
                let current_fn = self.current_function();
                let after_bb = self.context.append_basic_block(current_fn, "after.ret");
                self.builder.position_at_end(after_bb);
            }
            Stmt::Break { label, value, .. } => {
                let loop_info_idx = if let Some(label_name) = label {
                    self.loop_stack
                        .iter()
                        .rposition(|l| l.label.as_deref() == Some(label_name))
                        .expect("codegen: label not found")
                } else {
                    self.loop_stack.len() - 1
                };

                if let Some(val_expr) = value {
                    let val = self.emit_expr(val_expr);
                    if let Some(result_addr) = self.loop_stack[loop_info_idx].result_addr {
                        self.builder.build_store(result_addr, val).unwrap();
                    }
                }

                let break_bb = self.loop_stack[loop_info_idx].break_bb;
                self.builder.build_unconditional_branch(break_bb).unwrap();

                let current_fn = self.current_function();
                let after_bb = self.context.append_basic_block(current_fn, "after.break");
                self.builder.position_at_end(after_bb);
            }
            Stmt::Continue { label, .. } => {
                let loop_info_idx = if let Some(label_name) = label {
                    self.loop_stack
                        .iter()
                        .rposition(|l| l.label.as_deref() == Some(label_name))
                        .expect("codegen: label not found")
                } else {
                    self.loop_stack.len() - 1
                };

                let cond_bb = self.loop_stack[loop_info_idx].cond_bb;
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                let current_fn = self.current_function();
                let after_bb = self
                    .context
                    .append_basic_block(current_fn, "after.continue");
                self.builder.position_at_end(after_bb);
            }
            Stmt::Expr(expr) => {
                self.emit_expr(expr);
            }
        }
    }

    // --- Expression Emission ---

    fn emit_expr(&mut self, expr: &Expr) -> BasicValueEnum<'ctx> {
        match expr {
            Expr::IntegerLiteral { value, suffix } => {
                let ty = match suffix {
                    Some(IntSuffix::I8) => self.context.i8_type(),
                    Some(IntSuffix::I16) => self.context.i16_type(),
                    Some(IntSuffix::I32) => self.context.i32_type(),
                    Some(IntSuffix::I64) => self.context.i64_type(),
                    None => self.context.i32_type(),
                };
                ty.const_int(*value as u64, true).into()
            }
            Expr::FloatLiteral { value, suffix } => {
                let ty = match suffix {
                    Some(FloatSuffix::F32) => self.context.f32_type(),
                    Some(FloatSuffix::F64) => self.context.f64_type(),
                    None => self.context.f64_type(),
                };
                ty.const_float(*value).into()
            }
            Expr::BoolLiteral(b) => self
                .context
                .bool_type()
                .const_int(if *b { 1 } else { 0 }, false)
                .into(),
            Expr::UnitLiteral => self.unit_value(),
            Expr::Identifier(name, _) => {
                let (addr, ty) = self.lookup_var(name);
                let llvm_ty = self.llvm_type(&ty);
                self.builder.build_load(llvm_ty, addr, name).unwrap()
            }
            Expr::BinaryOp { op, left, right } => match op {
                BinOp::And => self.emit_and(left, right),
                BinOp::Or => self.emit_or(left, right),
                BinOp::Div | BinOp::Mod => self.emit_div_mod(left, right, *op),
                _ => {
                    let lval = self.emit_expr(left);
                    let rval = self.emit_expr(right);
                    let ty = self.infer_type(left);
                    self.emit_binop(*op, lval, rval, &ty)
                }
            },
            Expr::UnaryOp { op, operand } => {
                let val = self.emit_expr(operand);
                let ty = self.infer_type(operand);
                match op {
                    UnaryOp::Neg => {
                        if Self::is_float_type(&ty) {
                            self.builder
                                .build_float_neg(val.into_float_value(), "fneg")
                                .unwrap()
                                .into()
                        } else {
                            self.builder
                                .build_int_neg(val.into_int_value(), "neg")
                                .unwrap()
                                .into()
                        }
                    }
                    UnaryOp::Not => {
                        let bool_val = val.into_int_value();
                        self.builder.build_not(bool_val, "not").unwrap().into()
                    }
                }
            }
            Expr::Cast {
                expr: inner,
                target_type,
            } => {
                let val = self.emit_expr(inner);
                let source_type = self.infer_type(inner);
                self.emit_cast(val, &source_type, target_type)
            }
            Expr::Call { name, args, .. } => {
                let arg_vals: Vec<BasicMetadataValueEnum> =
                    args.iter().map(|a| self.emit_expr(a).into()).collect();
                let c_name = Self::mangle(name);
                let callee = self
                    .module
                    .get_function(&c_name)
                    .unwrap_or_else(|| panic!("codegen: undefined function: {}", name));

                let call_val = self.builder.build_call(callee, &arg_vals, "call").unwrap();

                // Check if function returns void
                if callee.get_type().get_return_type().is_none() {
                    self.unit_value()
                } else {
                    call_val.try_as_basic_value().unwrap_basic()
                }
            }
            Expr::Assign { name, value, .. } => {
                let val = self.emit_expr(value);
                let (addr, _) = self.lookup_var(name);
                self.builder.build_store(addr, val).unwrap();
                self.unit_value()
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => match else_block {
                Some(else_clause) => self.emit_if_expr(condition, then_block, else_clause),
                None => self.emit_if_stmt(condition, then_block),
            },
            Expr::While {
                label,
                condition,
                body,
            } => self.emit_while(label, condition, body),
            Expr::Match {
                expr: match_expr,
                arms,
            } => self.emit_match(match_expr, arms),
            Expr::Block(block) => self.emit_block_value(block),
            Expr::FieldAccess { expr: inner, field } => {
                let val = self.emit_expr(inner);
                let base_type = self.infer_type(inner);
                if let Type::Named(name) = &base_type {
                    if let Some(fields) = self.struct_types.get(name) {
                        let idx = fields
                            .iter()
                            .position(|(n, _)| n == field)
                            .expect("codegen: field not found");
                        self.builder
                            .build_extract_value(val.into_struct_value(), idx as u32, field)
                            .unwrap()
                    } else {
                        panic!("codegen: not a struct type");
                    }
                } else {
                    panic!("codegen: field access on non-struct");
                }
            }
            Expr::TupleIndex { expr: inner, index } => {
                let val = self.emit_expr(inner);
                self.builder
                    .build_extract_value(val.into_struct_value(), *index, "tuple.idx")
                    .unwrap()
            }
            Expr::TupleExpr(elems) => {
                let types: Vec<BasicTypeEnum> = elems
                    .iter()
                    .map(|e| {
                        let ty = self.infer_type(e);
                        self.llvm_type(&ty)
                    })
                    .collect();
                let tuple_type = self.context.struct_type(&types, false);
                let mut tuple_val = tuple_type.get_undef();
                for (i, elem) in elems.iter().enumerate() {
                    let val = self.emit_expr(elem);
                    tuple_val = self
                        .builder
                        .build_insert_value(tuple_val, val, i as u32, "tuple.insert")
                        .unwrap()
                        .into_struct_value();
                }
                tuple_val.into()
            }
            Expr::StructExpr { name, fields, .. } => {
                let struct_fields = self.struct_types.get(name).unwrap().clone();
                let struct_type = self
                    .module
                    .get_struct_type(&format!("cz.{}", name))
                    .unwrap();
                let mut struct_val = struct_type.get_undef();
                for (field_name, field_expr) in fields {
                    let val = self.emit_expr(field_expr);
                    let idx = struct_fields
                        .iter()
                        .position(|(n, _)| n == field_name)
                        .expect("codegen: struct field not found");
                    struct_val = self
                        .builder
                        .build_insert_value(struct_val, val, idx as u32, "struct.insert")
                        .unwrap()
                        .into_struct_value();
                }
                struct_val.into()
            }
            Expr::EnumExpr {
                enum_name,
                variant,
                args,
                ..
            } => self.emit_enum_construction(enum_name, variant, args),
        }
    }

    fn emit_binop(
        &mut self,
        op: BinOp,
        lval: BasicValueEnum<'ctx>,
        rval: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> BasicValueEnum<'ctx> {
        if Self::is_float_type(ty) {
            let lf = lval.into_float_value();
            let rf = rval.into_float_value();
            match op {
                BinOp::Add => self.builder.build_float_add(lf, rf, "fadd").unwrap().into(),
                BinOp::Sub => self.builder.build_float_sub(lf, rf, "fsub").unwrap().into(),
                BinOp::Mul => self.builder.build_float_mul(lf, rf, "fmul").unwrap().into(),
                BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                    let pred = match op {
                        BinOp::Eq => FloatPredicate::OEQ,
                        BinOp::NotEq => FloatPredicate::ONE,
                        BinOp::Lt => FloatPredicate::OLT,
                        BinOp::Gt => FloatPredicate::OGT,
                        BinOp::LtEq => FloatPredicate::OLE,
                        BinOp::GtEq => FloatPredicate::OGE,
                        _ => unreachable!(),
                    };
                    self.builder
                        .build_float_compare(pred, lf, rf, "fcmp")
                        .unwrap()
                        .into()
                }
                _ => unreachable!(),
            }
        } else {
            let li = lval.into_int_value();
            let ri = rval.into_int_value();
            match op {
                BinOp::Add => self.builder.build_int_add(li, ri, "add").unwrap().into(),
                BinOp::Sub => self.builder.build_int_sub(li, ri, "sub").unwrap().into(),
                BinOp::Mul => self.builder.build_int_mul(li, ri, "mul").unwrap().into(),
                BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::LtEq | BinOp::GtEq => {
                    let pred = match op {
                        BinOp::Eq => IntPredicate::EQ,
                        BinOp::NotEq => IntPredicate::NE,
                        BinOp::Lt => IntPredicate::SLT,
                        BinOp::Gt => IntPredicate::SGT,
                        BinOp::LtEq => IntPredicate::SLE,
                        BinOp::GtEq => IntPredicate::SGE,
                        _ => unreachable!(),
                    };
                    self.builder
                        .build_int_compare(pred, li, ri, "cmp")
                        .unwrap()
                        .into()
                }
                _ => unreachable!(),
            }
        }
    }

    fn emit_and(&mut self, left: &Expr, right: &Expr) -> BasicValueEnum<'ctx> {
        let i1_type = self.context.bool_type();
        let current_fn = self.current_function();

        let result_addr = self.builder.build_alloca(i1_type, "and.result").unwrap();

        let lval = self.emit_expr(left).into_int_value();

        let rhs_bb = self.context.append_basic_block(current_fn, "and.rhs");
        let false_bb = self.context.append_basic_block(current_fn, "and.false");
        let merge_bb = self.context.append_basic_block(current_fn, "and.merge");

        self.builder
            .build_conditional_branch(lval, rhs_bb, false_bb)
            .unwrap();

        self.builder.position_at_end(rhs_bb);
        let rval = self.emit_expr(right).into_int_value();
        self.builder.build_store(result_addr, rval).unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(false_bb);
        let false_val = i1_type.const_int(0, false);
        self.builder.build_store(result_addr, false_val).unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(merge_bb);
        self.builder
            .build_load(i1_type, result_addr, "and.val")
            .unwrap()
    }

    fn emit_or(&mut self, left: &Expr, right: &Expr) -> BasicValueEnum<'ctx> {
        let i1_type = self.context.bool_type();
        let current_fn = self.current_function();

        let result_addr = self.builder.build_alloca(i1_type, "or.result").unwrap();

        let lval = self.emit_expr(left).into_int_value();

        let true_bb = self.context.append_basic_block(current_fn, "or.true");
        let rhs_bb = self.context.append_basic_block(current_fn, "or.rhs");
        let merge_bb = self.context.append_basic_block(current_fn, "or.merge");

        self.builder
            .build_conditional_branch(lval, true_bb, rhs_bb)
            .unwrap();

        self.builder.position_at_end(true_bb);
        let true_val = i1_type.const_int(1, false);
        self.builder.build_store(result_addr, true_val).unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(rhs_bb);
        let rval = self.emit_expr(right).into_int_value();
        self.builder.build_store(result_addr, rval).unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(merge_bb);
        self.builder
            .build_load(i1_type, result_addr, "or.val")
            .unwrap()
    }

    fn emit_div_mod(&mut self, left: &Expr, right: &Expr, op: BinOp) -> BasicValueEnum<'ctx> {
        let ty = self.infer_type(left);

        if Self::is_float_type(&ty) {
            let lval = self.emit_expr(left).into_float_value();
            let rval = self.emit_expr(right).into_float_value();
            // No division-by-zero check for floats (produces inf/nan)
            self.builder
                .build_float_div(lval, rval, "fdiv")
                .unwrap()
                .into()
        } else {
            let lval = self.emit_expr(left).into_int_value();
            let rval = self.emit_expr(right).into_int_value();

            let zero = rval.get_type().const_int(0, false);
            let current_fn = self.current_function();

            let is_zero = self
                .builder
                .build_int_compare(IntPredicate::EQ, rval, zero, "div.zero")
                .unwrap();

            let trap_bb = self.context.append_basic_block(current_fn, "div.trap");
            let ok_bb = self.context.append_basic_block(current_fn, "div.ok");

            self.builder
                .build_conditional_branch(is_zero, trap_bb, ok_bb)
                .unwrap();

            self.builder.position_at_end(trap_bb);
            self.builder
                .build_call(self.printf_fn, &[self.div_zero_msg.into()], "trap.printf")
                .unwrap();
            let i32_type = self.context.i32_type();
            self.builder
                .build_call(self.exit_fn, &[i32_type.const_int(1, false).into()], "")
                .unwrap();
            self.builder.build_unreachable().unwrap();

            self.builder.position_at_end(ok_bb);
            match op {
                BinOp::Div => self
                    .builder
                    .build_int_signed_div(lval, rval, "sdiv")
                    .unwrap()
                    .into(),
                BinOp::Mod => self
                    .builder
                    .build_int_signed_rem(lval, rval, "srem")
                    .unwrap()
                    .into(),
                _ => unreachable!(),
            }
        }
    }

    fn emit_cast(
        &mut self,
        val: BasicValueEnum<'ctx>,
        source: &Type,
        target: &Type,
    ) -> BasicValueEnum<'ctx> {
        if source == target {
            return val;
        }

        match (source, target) {
            // Integer -> Integer
            (s, t) if Self::is_integer_type(s) && Self::is_integer_type(t) => {
                let src_bits = self.int_bit_width(s);
                let dst_bits = self.int_bit_width(t);
                let dst_type = self.llvm_type(t).into_int_type();
                let int_val = val.into_int_value();
                if dst_bits > src_bits {
                    self.builder
                        .build_int_s_extend(int_val, dst_type, "sext")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_int_truncate(int_val, dst_type, "trunc")
                        .unwrap()
                        .into()
                }
            }
            // Integer -> Float
            (s, t) if Self::is_integer_type(s) && Self::is_float_type(t) => {
                let dst_type = self.llvm_type(t).into_float_type();
                self.builder
                    .build_signed_int_to_float(val.into_int_value(), dst_type, "sitofp")
                    .unwrap()
                    .into()
            }
            // Float -> Integer
            (s, t) if Self::is_float_type(s) && Self::is_integer_type(t) => {
                let dst_type = self.llvm_type(t).into_int_type();
                self.builder
                    .build_float_to_signed_int(val.into_float_value(), dst_type, "fptosi")
                    .unwrap()
                    .into()
            }
            // Float -> Float
            (s, t) if Self::is_float_type(s) && Self::is_float_type(t) => {
                let dst_type = self.llvm_type(t).into_float_type();
                let fval = val.into_float_value();
                if self.float_bit_width(t) > self.float_bit_width(s) {
                    self.builder
                        .build_float_ext(fval, dst_type, "fpext")
                        .unwrap()
                        .into()
                } else {
                    self.builder
                        .build_float_trunc(fval, dst_type, "fptrunc")
                        .unwrap()
                        .into()
                }
            }
            // Bool -> Integer
            (Type::Bool, t) if Self::is_integer_type(t) => {
                let dst_type = self.llvm_type(t).into_int_type();
                self.builder
                    .build_int_z_extend(val.into_int_value(), dst_type, "zext")
                    .unwrap()
                    .into()
            }
            // Integer -> Bool
            (s, Type::Bool) if Self::is_integer_type(s) => {
                let zero = val.into_int_value().get_type().const_int(0, false);
                self.builder
                    .build_int_compare(IntPredicate::NE, val.into_int_value(), zero, "tobool")
                    .unwrap()
                    .into()
            }
            // Bool -> Float
            (Type::Bool, t) if Self::is_float_type(t) => {
                let i32_val = self
                    .builder
                    .build_int_z_extend(val.into_int_value(), self.context.i32_type(), "bool2int")
                    .unwrap();
                let dst_type = self.llvm_type(t).into_float_type();
                self.builder
                    .build_signed_int_to_float(i32_val, dst_type, "sitofp")
                    .unwrap()
                    .into()
            }
            // Float -> Bool
            (s, Type::Bool) if Self::is_float_type(s) => {
                let zero = val.into_float_value().get_type().const_float(0.0);
                self.builder
                    .build_float_compare(
                        FloatPredicate::ONE,
                        val.into_float_value(),
                        zero,
                        "tobool",
                    )
                    .unwrap()
                    .into()
            }
            _ => val, // fallback
        }
    }

    fn int_bit_width(&self, ty: &Type) -> u32 {
        match ty {
            Type::I8 => 8,
            Type::I16 => 16,
            Type::I32 => 32,
            Type::I64 => 64,
            _ => 32,
        }
    }

    fn float_bit_width(&self, ty: &Type) -> u32 {
        match ty {
            Type::F32 => 32,
            Type::F64 => 64,
            _ => 64,
        }
    }

    fn emit_if_expr(
        &mut self,
        condition: &Expr,
        then_block: &Block,
        else_clause: &ElseClause,
    ) -> BasicValueEnum<'ctx> {
        let result_type = if let Some(tail) = &then_block.expr {
            self.infer_type(tail)
        } else {
            Type::Unit
        };
        let llvm_result_type = self.llvm_type(&result_type);

        let current_fn = self.current_function();
        let result_addr = self
            .builder
            .build_alloca(llvm_result_type, "if.result")
            .unwrap();

        let cond_val = self.emit_expr(condition).into_int_value();

        let then_bb = self.context.append_basic_block(current_fn, "if.then");
        let else_bb = self.context.append_basic_block(current_fn, "if.else");
        let merge_bb = self.context.append_basic_block(current_fn, "if.merge");

        self.builder
            .build_conditional_branch(cond_val, then_bb, else_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        let then_val = self.emit_block_value(then_block);
        let then_terminated = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_some();
        if !then_terminated {
            self.builder.build_store(result_addr, then_val).unwrap();
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        self.builder.position_at_end(else_bb);
        let else_val = match else_clause {
            ElseClause::ElseBlock(block) => self.emit_block_value(block),
            ElseClause::ElseIf(if_expr) => self.emit_expr(if_expr),
        };
        let else_terminated = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_some();
        if !else_terminated {
            self.builder.build_store(result_addr, else_val).unwrap();
            self.builder.build_unconditional_branch(merge_bb).unwrap();
        }

        self.builder.position_at_end(merge_bb);
        self.builder
            .build_load(llvm_result_type, result_addr, "if.val")
            .unwrap()
    }

    fn emit_if_stmt(&mut self, condition: &Expr, then_block: &Block) -> BasicValueEnum<'ctx> {
        let current_fn = self.current_function();

        let cond_val = self.emit_expr(condition).into_int_value();

        let then_bb = self.context.append_basic_block(current_fn, "if.then");
        let end_bb = self.context.append_basic_block(current_fn, "if.end");

        self.builder
            .build_conditional_branch(cond_val, then_bb, end_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        self.push_scope();
        for s in &then_block.stmts {
            self.emit_stmt(s);
        }
        if let Some(tail) = &then_block.expr {
            self.emit_expr(tail);
        }
        self.pop_scope();
        let then_terminated = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_some();
        if !then_terminated {
            self.builder.build_unconditional_branch(end_bb).unwrap();
        }

        self.builder.position_at_end(end_bb);
        self.unit_value()
    }

    fn emit_while(
        &mut self,
        label: &Option<String>,
        condition: &Expr,
        body: &Block,
    ) -> BasicValueEnum<'ctx> {
        let current_fn = self.current_function();
        let cond_bb = self.context.append_basic_block(current_fn, "while.cond");
        let body_bb = self.context.append_basic_block(current_fn, "while.body");
        let end_bb = self.context.append_basic_block(current_fn, "while.end");

        // Check if while loop has break with value
        let break_value_type = self.find_break_value_type_in_block(body);
        let result_addr = break_value_type.as_ref().map(|ty| {
            let llvm_ty = self.llvm_type(ty);
            self.builder.build_alloca(llvm_ty, "while.result").unwrap()
        });

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(cond_bb);
        let cond_val = self.emit_expr(condition).into_int_value();
        self.builder
            .build_conditional_branch(cond_val, body_bb, end_bb)
            .unwrap();

        self.builder.position_at_end(body_bb);
        self.loop_stack.push(LoopInfo {
            break_bb: end_bb,
            cond_bb,
            result_addr,
            label: label.clone(),
        });
        self.push_scope();
        for s in &body.stmts {
            self.emit_stmt(s);
            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_some()
            {
                let dead_bb = self.context.append_basic_block(current_fn, "dead");
                self.builder.position_at_end(dead_bb);
            }
        }
        if let Some(tail) = &body.expr {
            self.emit_expr(tail);
        }
        self.pop_scope();
        self.loop_stack.pop();
        let body_terminated = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_some();
        if !body_terminated {
            self.builder.build_unconditional_branch(cond_bb).unwrap();
        }

        self.builder.position_at_end(end_bb);
        if let (Some(addr), Some(ty)) = (result_addr, &break_value_type) {
            let llvm_ty = self.llvm_type(ty);
            self.builder.build_load(llvm_ty, addr, "while.val").unwrap()
        } else {
            self.unit_value()
        }
    }

    fn emit_match(&mut self, match_expr: &Expr, arms: &[MatchArm]) -> BasicValueEnum<'ctx> {
        let match_val = self.emit_expr(match_expr);
        let match_type = self.infer_type(match_expr);

        // Determine result type
        let result_type = if let Some(first_arm) = arms.first() {
            self.infer_type(&first_arm.body)
        } else {
            Type::Unit
        };
        let llvm_result_type = self.llvm_type(&result_type);

        let current_fn = self.current_function();
        let result_addr = self
            .builder
            .build_alloca(llvm_result_type, "match.result")
            .unwrap();
        let merge_bb = self.context.append_basic_block(current_fn, "match.merge");

        // Store match value in alloca so we can use it in patterns
        let match_llvm_type = self.llvm_type(&match_type);
        let match_addr = self
            .builder
            .build_alloca(match_llvm_type, "match.val")
            .unwrap();
        self.builder.build_store(match_addr, match_val).unwrap();

        for (i, arm) in arms.iter().enumerate() {
            let test_bb = self
                .context
                .append_basic_block(current_fn, &format!("match.test.{}", i));
            let body_bb = self
                .context
                .append_basic_block(current_fn, &format!("match.arm.{}", i));
            let next_bb = if i + 1 < arms.len() {
                self.context
                    .append_basic_block(current_fn, &format!("match.next.{}", i))
            } else {
                merge_bb
            };

            self.builder.build_unconditional_branch(test_bb).unwrap();
            self.builder.position_at_end(test_bb);

            let match_val_loaded = self
                .builder
                .build_load(match_llvm_type, match_addr, "match.loaded")
                .unwrap();

            let test_result = self.emit_pattern_test(&arm.pattern, match_val_loaded, &match_type);
            self.builder
                .build_conditional_branch(test_result, body_bb, next_bb)
                .unwrap();

            self.builder.position_at_end(body_bb);
            self.push_scope();
            self.emit_pattern_bindings(&arm.pattern, match_val_loaded, &match_type);
            let arm_val = self.emit_expr(&arm.body);
            let arm_terminated = self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_some();
            if !arm_terminated {
                self.builder.build_store(result_addr, arm_val).unwrap();
                self.builder.build_unconditional_branch(merge_bb).unwrap();
            }
            self.pop_scope();

            if i + 1 < arms.len() {
                self.builder.position_at_end(next_bb);
            }
        }

        // If we fall through (no match), unreachable (should be caught by exhaustiveness)
        let current_block = self.builder.get_insert_block().unwrap();
        if current_block != merge_bb && current_block.get_terminator().is_none() {
            self.builder.build_unreachable().unwrap();
        }

        self.builder.position_at_end(merge_bb);
        self.builder
            .build_load(llvm_result_type, result_addr, "match.result")
            .unwrap()
    }

    fn emit_pattern_test(
        &mut self,
        pattern: &Pattern,
        val: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> inkwell::values::IntValue<'ctx> {
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

                    // Load tag
                    let enum_llvm_type = self.llvm_type(&Type::Named(enum_name.clone()));
                    let enum_addr = self
                        .builder
                        .build_alloca(enum_llvm_type, "enum.tmp")
                        .unwrap();
                    self.builder.build_store(enum_addr, val).unwrap();

                    let i32_type = self.context.i32_type();
                    let tag_ptr = self
                        .builder
                        .build_struct_gep(enum_llvm_type, enum_addr, 0, "tag.ptr")
                        .unwrap();
                    let tag = self
                        .builder
                        .build_load(i32_type, tag_ptr, "tag")
                        .unwrap()
                        .into_int_value();
                    let expected_tag = i32_type.const_int(variant_idx as u64, false);
                    let tag_match = self
                        .builder
                        .build_int_compare(IntPredicate::EQ, tag, expected_tag, "tag.cmp")
                        .unwrap();

                    // Also test payload patterns
                    let variant_kind = &variants[variant_idx].1;
                    match (variant_kind, args) {
                        (VariantKind::Unit, _) => tag_match,
                        (VariantKind::Tuple(types), EnumPatternArgs::Tuple(pats)) => {
                            let payload_ptr = self
                                .builder
                                .build_struct_gep(enum_llvm_type, enum_addr, 1, "payload.ptr")
                                .unwrap();
                            let mut result = tag_match;
                            let mut offset: u64 = 0;
                            for (pat, field_type) in pats.iter().zip(types.iter()) {
                                let field_llvm_type = self.llvm_type(field_type);
                                let field_ptr_type = self.context.ptr_type(AddressSpace::default());
                                let field_ptr = unsafe {
                                    self.builder
                                        .build_gep(
                                            self.context.i8_type(),
                                            payload_ptr,
                                            &[self.context.i64_type().const_int(offset, false)],
                                            "field.ptr",
                                        )
                                        .unwrap()
                                };
                                let _ = field_ptr_type;
                                let field_val = self
                                    .builder
                                    .build_load(field_llvm_type, field_ptr, "field.val")
                                    .unwrap();
                                let test = self.emit_pattern_test(pat, field_val, field_type);
                                result = self.builder.build_and(result, test, "pat.and").unwrap();
                                offset += self.type_size(field_type);
                            }
                            result
                        }
                        (VariantKind::Struct(fields), EnumPatternArgs::Struct(pat_fields)) => {
                            let payload_ptr = self
                                .builder
                                .build_struct_gep(enum_llvm_type, enum_addr, 1, "payload.ptr")
                                .unwrap();
                            let mut result = tag_match;
                            for (pat_field_name, field_pat) in pat_fields {
                                if let Some(offset_and_type) =
                                    self.enum_field_offset(fields, pat_field_name)
                                {
                                    let (offset, field_type) = offset_and_type;
                                    let field_llvm_type = self.llvm_type(&field_type);
                                    let field_ptr = unsafe {
                                        self.builder
                                            .build_gep(
                                                self.context.i8_type(),
                                                payload_ptr,
                                                &[self.context.i64_type().const_int(offset, false)],
                                                "field.ptr",
                                            )
                                            .unwrap()
                                    };
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

    fn emit_pattern_bindings(&mut self, pattern: &Pattern, val: BasicValueEnum<'ctx>, ty: &Type) {
        match pattern {
            Pattern::Binding(name) => {
                let llvm_ty = self.llvm_type(ty);
                let alloca = self.builder.build_alloca(llvm_ty, name).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.define_var(name, alloca, ty.clone());
            }
            Pattern::Tuple(patterns) => {
                if let Type::Tuple(types) = ty {
                    let struct_val = val.into_struct_value();
                    for (i, (pat, elem_ty)) in patterns.iter().zip(types.iter()).enumerate() {
                        let elem = self
                            .builder
                            .build_extract_value(struct_val, i as u32, "tuple.elem")
                            .unwrap();
                        self.emit_pattern_bindings(pat, elem, elem_ty);
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
                            self.emit_pattern_bindings(field_pat, field_val, field_type);
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

                    let enum_llvm_type = self.llvm_type(&Type::Named(enum_name.clone()));
                    let enum_addr = self
                        .builder
                        .build_alloca(enum_llvm_type, "enum.bind")
                        .unwrap();
                    self.builder.build_store(enum_addr, val).unwrap();

                    match (variant_kind, args) {
                        (VariantKind::Tuple(types), EnumPatternArgs::Tuple(pats)) => {
                            let payload_ptr = self
                                .builder
                                .build_struct_gep(enum_llvm_type, enum_addr, 1, "payload.ptr")
                                .unwrap();
                            let mut offset: u64 = 0;
                            for (pat, field_type) in pats.iter().zip(types.iter()) {
                                let field_llvm_type = self.llvm_type(field_type);
                                let field_ptr = unsafe {
                                    self.builder
                                        .build_gep(
                                            self.context.i8_type(),
                                            payload_ptr,
                                            &[self.context.i64_type().const_int(offset, false)],
                                            "field.ptr",
                                        )
                                        .unwrap()
                                };
                                let field_val = self
                                    .builder
                                    .build_load(field_llvm_type, field_ptr, "field.val")
                                    .unwrap();
                                self.emit_pattern_bindings(pat, field_val, field_type);
                                offset += self.type_size(field_type);
                            }
                        }
                        (VariantKind::Struct(fields), EnumPatternArgs::Struct(pat_fields)) => {
                            let payload_ptr = self
                                .builder
                                .build_struct_gep(enum_llvm_type, enum_addr, 1, "payload.ptr")
                                .unwrap();
                            for (pat_field_name, field_pat) in pat_fields {
                                if let Some((offset, field_type)) =
                                    self.enum_field_offset(fields, pat_field_name)
                                {
                                    let field_llvm_type = self.llvm_type(&field_type);
                                    let field_ptr = unsafe {
                                        self.builder
                                            .build_gep(
                                                self.context.i8_type(),
                                                payload_ptr,
                                                &[self.context.i64_type().const_int(offset, false)],
                                                "field.ptr",
                                            )
                                            .unwrap()
                                    };
                                    let field_val = self
                                        .builder
                                        .build_load(field_llvm_type, field_ptr, "field.val")
                                        .unwrap();
                                    self.emit_pattern_bindings(field_pat, field_val, &field_type);
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

    fn emit_enum_construction(
        &mut self,
        enum_name: &str,
        variant: &str,
        args: &EnumArgs,
    ) -> BasicValueEnum<'ctx> {
        let variants = self.enum_types.get(enum_name).unwrap().clone();
        let variant_idx = variants.iter().position(|(n, _)| n == variant).unwrap();

        let enum_llvm_type = self.llvm_type(&Type::Named(enum_name.to_string()));
        let enum_addr = self
            .builder
            .build_alloca(enum_llvm_type, "enum.new")
            .unwrap();

        // Set tag
        let i32_type = self.context.i32_type();
        let tag_ptr = self
            .builder
            .build_struct_gep(enum_llvm_type, enum_addr, 0, "tag.ptr")
            .unwrap();
        let tag_val = i32_type.const_int(variant_idx as u64, false);
        self.builder.build_store(tag_ptr, tag_val).unwrap();

        // Set payload
        let variant_kind = variants[variant_idx].1.clone();
        match (&variant_kind, args) {
            (VariantKind::Unit, _) => {}
            (VariantKind::Tuple(types), EnumArgs::Tuple(exprs)) => {
                let payload_ptr = self
                    .builder
                    .build_struct_gep(enum_llvm_type, enum_addr, 1, "payload.ptr")
                    .unwrap();
                let types = types.clone();
                let mut offset: u64 = 0;
                for (expr, field_type) in exprs.iter().zip(types.iter()) {
                    let val = self.emit_expr(expr);
                    let field_ptr = unsafe {
                        self.builder
                            .build_gep(
                                self.context.i8_type(),
                                payload_ptr,
                                &[self.context.i64_type().const_int(offset, false)],
                                "field.ptr",
                            )
                            .unwrap()
                    };
                    self.builder.build_store(field_ptr, val).unwrap();
                    offset += self.type_size(field_type);
                }
            }
            (VariantKind::Struct(fields), EnumArgs::Struct(field_exprs)) => {
                let payload_ptr = self
                    .builder
                    .build_struct_gep(enum_llvm_type, enum_addr, 1, "payload.ptr")
                    .unwrap();
                let fields = fields.clone();
                for (field_name, expr) in field_exprs {
                    if let Some((offset, _field_type)) = self.enum_field_offset(&fields, field_name)
                    {
                        let val = self.emit_expr(expr);
                        let field_ptr = unsafe {
                            self.builder
                                .build_gep(
                                    self.context.i8_type(),
                                    payload_ptr,
                                    &[self.context.i64_type().const_int(offset, false)],
                                    "field.ptr",
                                )
                                .unwrap()
                        };
                        self.builder.build_store(field_ptr, val).unwrap();
                    }
                }
            }
            _ => {}
        }

        self.builder
            .build_load(enum_llvm_type, enum_addr, "enum.val")
            .unwrap()
    }

    fn find_break_value_type_in_block(&self, block: &Block) -> Option<Type> {
        for stmt in &block.stmts {
            match stmt {
                Stmt::Break {
                    value: Some(expr), ..
                } => {
                    return Some(self.infer_type(expr));
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
            _ => None,
        }
    }

    fn enum_field_offset(&self, fields: &[FieldDef], target: &str) -> Option<(u64, Type)> {
        let mut offset: u64 = 0;
        for f in fields {
            if f.name == target {
                return Some((offset, f.field_type.clone()));
            }
            offset += self.type_size(&f.field_type);
        }
        None
    }
}

use crate::ast::*;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, IntValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;
use std::collections::HashMap;
use std::path::Path;

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    vars: Vec<HashMap<String, PointerValue<'ctx>>>,
    printf_fn: FunctionValue<'ctx>,
    exit_fn: FunctionValue<'ctx>,
    div_zero_msg: PointerValue<'ctx>,
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
        let fmt_str = context.const_string(b"%d\n", true);
        let fmt_global = module.add_global(fmt_str.get_type(), None, ".fmt_i32");
        fmt_global.set_initializer(&fmt_str);
        fmt_global.set_constant(true);
        fmt_global.set_linkage(Linkage::Private);

        let div_zero_str = context.const_string(b"runtime error: division by zero\n", true);
        let div_zero_global = module.add_global(div_zero_str.get_type(), None, ".div_zero_msg");
        div_zero_global.set_initializer(&div_zero_str);
        div_zero_global.set_constant(true);
        div_zero_global.set_linkage(Linkage::Private);

        // Built-in: cz_print_i32
        let print_i32_type = i32_type.fn_type(&[i32_type.into()], false);
        let print_i32_fn = module.add_function("cz_print_i32", print_i32_type, None);
        let entry = context.append_basic_block(print_i32_fn, "entry");
        builder.position_at_end(entry);
        let x_param = print_i32_fn.get_nth_param(0).unwrap().into_int_value();
        builder
            .build_call(
                printf_fn,
                &[fmt_global.as_pointer_value().into(), x_param.into()],
                "printf_call",
            )
            .unwrap();
        builder
            .build_return(Some(&i32_type.const_int(0, false)))
            .unwrap();

        Self {
            context,
            module,
            builder,
            vars: Vec::new(),
            printf_fn,
            exit_fn,
            div_zero_msg: div_zero_global.as_pointer_value(),
        }
    }

    pub fn generate(&mut self, program: &Program) {
        let i32_type = self.context.i32_type();

        // Pass 1: Register all function signatures (enables forward references)
        for func in &program.functions {
            let c_name = Self::mangle(&func.name);
            if self.module.get_function(&c_name).is_none() {
                let param_types: Vec<_> = func.params.iter().map(|_| i32_type.into()).collect();
                let fn_type = i32_type.fn_type(&param_types, false);
                self.module.add_function(&c_name, fn_type, None);
            }
        }

        // Pass 2: Generate function bodies
        for func in &program.functions {
            self.emit_function(func);
        }
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

    fn push_scope(&mut self) {
        self.vars.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.vars.pop();
    }

    fn define_var(&mut self, name: &str, ptr: PointerValue<'ctx>) {
        if let Some(scope) = self.vars.last_mut() {
            scope.insert(name.to_string(), ptr);
        }
    }

    fn lookup_var(&self, name: &str) -> PointerValue<'ctx> {
        for scope in self.vars.iter().rev() {
            if let Some(ptr) = scope.get(name) {
                return *ptr;
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

    // --- Function/Block Emission ---

    fn emit_function(&mut self, func: &FunctionDef) {
        self.vars.clear();

        let i32_type = self.context.i32_type();
        let c_name = Self::mangle(&func.name);

        // Function was already registered in Pass 1
        let fn_value = self.module.get_function(&c_name).unwrap();

        let entry = self.context.append_basic_block(fn_value, "entry");
        self.builder.position_at_end(entry);

        self.push_scope();

        for (i, param) in func.params.iter().enumerate() {
            let param_val = fn_value.get_nth_param(i as u32).unwrap().into_int_value();
            let alloca = self.builder.build_alloca(i32_type, &param.name).unwrap();
            self.builder.build_store(alloca, param_val).unwrap();
            self.define_var(&param.name, alloca);
        }

        self.emit_block_as_return(&func.body);
        self.pop_scope();
    }

    fn emit_block_as_return(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.emit_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            let val = self.emit_expr(expr);
            self.builder.build_return(Some(&val)).unwrap();
        }
        // Ensure dead code blocks (after early return) have a terminator
        let current_block = self.builder.get_insert_block().unwrap();
        if current_block.get_terminator().is_none() {
            self.builder.build_unreachable().unwrap();
        }
    }

    fn emit_block_value(&mut self, block: &Block) -> IntValue<'ctx> {
        self.push_scope();
        for stmt in &block.stmts {
            self.emit_stmt(stmt);
        }
        let val = if let Some(expr) = &block.expr {
            self.emit_expr(expr)
        } else {
            self.context.i32_type().const_int(0, false)
        };
        self.pop_scope();
        val
    }

    // --- Statement Emission ---

    fn emit_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { name, init, .. } => {
                let val = self.emit_expr(init);
                let i32_type = self.context.i32_type();
                let alloca = self.builder.build_alloca(i32_type, name).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.define_var(name, alloca);
            }
            Stmt::While { condition, body } => {
                let current_fn = self.current_function();
                let cond_bb = self.context.append_basic_block(current_fn, "while.cond");
                let body_bb = self.context.append_basic_block(current_fn, "while.body");
                let end_bb = self.context.append_basic_block(current_fn, "while.end");

                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(cond_bb);
                let cond_val = self.emit_expr(condition);
                let zero = self.context.i32_type().const_int(0, false);
                let cond_bool = self
                    .builder
                    .build_int_compare(IntPredicate::NE, cond_val, zero, "while.cmp")
                    .unwrap();
                self.builder
                    .build_conditional_branch(cond_bool, body_bb, end_bb)
                    .unwrap();

                self.builder.position_at_end(body_bb);
                self.push_scope();
                for s in &body.stmts {
                    self.emit_stmt(s);
                }
                self.pop_scope();
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(end_bb);
            }
            Stmt::Return { value, .. } => {
                let val = self.emit_expr(value);
                self.builder.build_return(Some(&val)).unwrap();
                let current_fn = self.current_function();
                let after_bb = self.context.append_basic_block(current_fn, "after.ret");
                self.builder.position_at_end(after_bb);
            }
            Stmt::Expr(expr) => {
                self.emit_expr(expr);
            }
        }
    }

    // --- Expression Emission ---

    fn emit_expr(&mut self, expr: &Expr) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        match expr {
            Expr::IntegerLiteral(val) => i32_type.const_int(*val as u64, true),
            Expr::BoolLiteral(b) => i32_type.const_int(if *b { 1 } else { 0 }, false),
            Expr::Identifier(name, _) => {
                let addr = self.lookup_var(name);
                self.builder
                    .build_load(i32_type, addr, name)
                    .unwrap()
                    .into_int_value()
            }
            Expr::BinaryOp { op, left, right } => match op {
                BinOp::And => self.emit_and(left, right),
                BinOp::Or => self.emit_or(left, right),
                BinOp::Div | BinOp::Mod => self.emit_div_mod(left, right, *op),
                _ => {
                    let lval = self.emit_expr(left);
                    let rval = self.emit_expr(right);
                    self.emit_binop(*op, lval, rval)
                }
            },
            Expr::UnaryOp { op, operand } => {
                let val = self.emit_expr(operand);
                match op {
                    UnaryOp::Neg => self.builder.build_int_neg(val, "neg").unwrap(),
                    UnaryOp::Not => {
                        let zero = i32_type.const_int(0, false);
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, val, zero, "not.cmp")
                            .unwrap();
                        self.builder
                            .build_int_z_extend(cmp, i32_type, "not.ext")
                            .unwrap()
                    }
                }
            }
            Expr::Call { name, args, .. } => {
                let arg_vals: Vec<BasicMetadataValueEnum> =
                    args.iter().map(|a| self.emit_expr(a).into()).collect();
                let c_name = Self::mangle(name);
                let callee = self
                    .module
                    .get_function(&c_name)
                    .unwrap_or_else(|| panic!("codegen: undefined function: {}", name));
                self.builder
                    .build_call(callee, &arg_vals, "call")
                    .unwrap()
                    .try_as_basic_value()
                    .unwrap_basic()
                    .into_int_value()
            }
            Expr::Assign { name, value, .. } => {
                let val = self.emit_expr(value);
                let addr = self.lookup_var(name);
                self.builder.build_store(addr, val).unwrap();
                val
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => match else_block {
                Some(else_clause) => self.emit_if_expr(condition, then_block, else_clause),
                None => self.emit_if_stmt(condition, then_block),
            },
            Expr::Block(block) => self.emit_block_value(block),
        }
    }

    fn emit_binop(
        &mut self,
        op: BinOp,
        lval: IntValue<'ctx>,
        rval: IntValue<'ctx>,
    ) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        match op {
            BinOp::Add => self.builder.build_int_add(lval, rval, "add").unwrap(),
            BinOp::Sub => self.builder.build_int_sub(lval, rval, "sub").unwrap(),
            BinOp::Mul => self.builder.build_int_mul(lval, rval, "mul").unwrap(),
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
                let cmp = self
                    .builder
                    .build_int_compare(pred, lval, rval, "cmp")
                    .unwrap();
                self.builder
                    .build_int_z_extend(cmp, i32_type, "zext")
                    .unwrap()
            }
            _ => unreachable!(),
        }
    }

    fn emit_and(&mut self, left: &Expr, right: &Expr) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let current_fn = self.current_function();

        let result_addr = self.builder.build_alloca(i32_type, "and.result").unwrap();

        let lval = self.emit_expr(left);
        let lcond = self
            .builder
            .build_int_compare(IntPredicate::NE, lval, zero, "and.lcond")
            .unwrap();

        let rhs_bb = self.context.append_basic_block(current_fn, "and.rhs");
        let false_bb = self.context.append_basic_block(current_fn, "and.false");
        let merge_bb = self.context.append_basic_block(current_fn, "and.merge");

        self.builder
            .build_conditional_branch(lcond, rhs_bb, false_bb)
            .unwrap();

        self.builder.position_at_end(rhs_bb);
        let rval = self.emit_expr(right);
        let rcond = self
            .builder
            .build_int_compare(IntPredicate::NE, rval, zero, "and.rcond")
            .unwrap();
        let rext = self
            .builder
            .build_int_z_extend(rcond, i32_type, "and.rext")
            .unwrap();
        self.builder.build_store(result_addr, rext).unwrap();
        self.builder
            .build_unconditional_branch(merge_bb)
            .unwrap();

        self.builder.position_at_end(false_bb);
        self.builder.build_store(result_addr, zero).unwrap();
        self.builder
            .build_unconditional_branch(merge_bb)
            .unwrap();

        self.builder.position_at_end(merge_bb);
        self.builder
            .build_load(i32_type, result_addr, "and.val")
            .unwrap()
            .into_int_value()
    }

    fn emit_or(&mut self, left: &Expr, right: &Expr) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let current_fn = self.current_function();

        let result_addr = self.builder.build_alloca(i32_type, "or.result").unwrap();

        let lval = self.emit_expr(left);
        let lcond = self
            .builder
            .build_int_compare(IntPredicate::NE, lval, zero, "or.lcond")
            .unwrap();

        let true_bb = self.context.append_basic_block(current_fn, "or.true");
        let rhs_bb = self.context.append_basic_block(current_fn, "or.rhs");
        let merge_bb = self.context.append_basic_block(current_fn, "or.merge");

        self.builder
            .build_conditional_branch(lcond, true_bb, rhs_bb)
            .unwrap();

        self.builder.position_at_end(true_bb);
        self.builder.build_store(result_addr, lval).unwrap();
        self.builder
            .build_unconditional_branch(merge_bb)
            .unwrap();

        self.builder.position_at_end(rhs_bb);
        let rval = self.emit_expr(right);
        self.builder.build_store(result_addr, rval).unwrap();
        self.builder
            .build_unconditional_branch(merge_bb)
            .unwrap();

        self.builder.position_at_end(merge_bb);
        self.builder
            .build_load(i32_type, result_addr, "or.val")
            .unwrap()
            .into_int_value()
    }

    fn emit_div_mod(&mut self, left: &Expr, right: &Expr, op: BinOp) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let current_fn = self.current_function();

        let lval = self.emit_expr(left);
        let rval = self.emit_expr(right);

        let is_zero = self
            .builder
            .build_int_compare(IntPredicate::EQ, rval, zero, "div.zero")
            .unwrap();

        let trap_bb = self.context.append_basic_block(current_fn, "div.trap");
        let ok_bb = self.context.append_basic_block(current_fn, "div.ok");

        self.builder
            .build_conditional_branch(is_zero, trap_bb, ok_bb)
            .unwrap();

        // Trap: print error and exit
        self.builder.position_at_end(trap_bb);
        self.builder
            .build_call(
                self.printf_fn,
                &[self.div_zero_msg.into()],
                "trap.printf",
            )
            .unwrap();
        self.builder
            .build_call(
                self.exit_fn,
                &[i32_type.const_int(1, false).into()],
                "",
            )
            .unwrap();
        self.builder.build_unreachable().unwrap();

        // OK: perform the operation
        self.builder.position_at_end(ok_bb);
        match op {
            BinOp::Div => self
                .builder
                .build_int_signed_div(lval, rval, "sdiv")
                .unwrap(),
            BinOp::Mod => self
                .builder
                .build_int_signed_rem(lval, rval, "srem")
                .unwrap(),
            _ => unreachable!(),
        }
    }

    fn emit_if_expr(
        &mut self,
        condition: &Expr,
        then_block: &Block,
        else_clause: &ElseClause,
    ) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let current_fn = self.current_function();

        let result_addr = self.builder.build_alloca(i32_type, "if.result").unwrap();

        let cond_val = self.emit_expr(condition);
        let cond_bool = self
            .builder
            .build_int_compare(IntPredicate::NE, cond_val, zero, "if.cond")
            .unwrap();

        let then_bb = self.context.append_basic_block(current_fn, "if.then");
        let else_bb = self.context.append_basic_block(current_fn, "if.else");
        let merge_bb = self.context.append_basic_block(current_fn, "if.merge");

        self.builder
            .build_conditional_branch(cond_bool, then_bb, else_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        let then_val = self.emit_block_value(then_block);
        self.builder.build_store(result_addr, then_val).unwrap();
        self.builder
            .build_unconditional_branch(merge_bb)
            .unwrap();

        self.builder.position_at_end(else_bb);
        let else_val = match else_clause {
            ElseClause::ElseBlock(block) => self.emit_block_value(block),
            ElseClause::ElseIf(if_expr) => self.emit_expr(if_expr),
        };
        self.builder.build_store(result_addr, else_val).unwrap();
        self.builder
            .build_unconditional_branch(merge_bb)
            .unwrap();

        self.builder.position_at_end(merge_bb);
        self.builder
            .build_load(i32_type, result_addr, "if.val")
            .unwrap()
            .into_int_value()
    }

    fn emit_if_stmt(&mut self, condition: &Expr, then_block: &Block) -> IntValue<'ctx> {
        let i32_type = self.context.i32_type();
        let zero = i32_type.const_int(0, false);
        let current_fn = self.current_function();

        let cond_val = self.emit_expr(condition);
        let cond_bool = self
            .builder
            .build_int_compare(IntPredicate::NE, cond_val, zero, "if.cond")
            .unwrap();

        let then_bb = self.context.append_basic_block(current_fn, "if.then");
        let end_bb = self.context.append_basic_block(current_fn, "if.end");

        self.builder
            .build_conditional_branch(cond_bool, then_bb, end_bb)
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
        self.builder.build_unconditional_branch(end_bb).unwrap();

        self.builder.position_at_end(end_bb);
        zero
    }
}

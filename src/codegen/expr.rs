use crate::hir::types::Type;
use crate::hir::*;
use crate::syntax::ast::{BinOp, Pattern, UnaryOp, VariantKind};

use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum};

use super::pattern::collect_binding_names_from_pattern;
use super::types::{float_bit_width, int_bit_width, type_size};
use super::{CodeGen, CodeGenError};

impl<'ctx> CodeGen<'ctx> {
    /// HIR 式を LLVM IR に変換し、値を返す。
    pub(crate) fn emit_expr(
        &mut self,
        expr_id: HirExprId,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let expr = self.hir_exprs.get(expr_id);
        let expr_ty = expr.ty.clone();
        match &expr.kind {
            HirExprKind::IntegerLiteral { value } => {
                let value = *value;
                let llvm_ty = match &expr_ty {
                    Type::I8 => self.context.i8_type(),
                    Type::I16 => self.context.i16_type(),
                    Type::I64 => self.context.i64_type(),
                    _ => self.context.i32_type(),
                };
                Ok(llvm_ty.const_int(value as u64, true).into())
            }
            HirExprKind::FloatLiteral { value } => {
                let value = *value;
                let llvm_ty = match &expr_ty {
                    Type::F32 => self.context.f32_type(),
                    _ => self.context.f64_type(),
                };
                Ok(llvm_ty.const_float(value).into())
            }
            HirExprKind::BoolLiteral(b) => {
                let b = *b;
                Ok(self
                    .context
                    .bool_type()
                    .const_int(if b { 1 } else { 0 }, false)
                    .into())
            }
            HirExprKind::UnitLiteral => Ok(self.unit_value()),
            HirExprKind::Var(var_id) => {
                let var_id = *var_id;
                let var_info = &self.vars[var_id.0 as usize];
                let llvm_ty = self.llvm_type(&var_info.ty)?;
                let ptr = self.var_ptrs[var_id.0 as usize]
                    .ok_or_else(|| CodeGenError::UndefinedVariable(var_info.name.clone()))?;
                Ok(self
                    .builder
                    .build_load(llvm_ty, ptr, &var_info.name)
                    .unwrap())
            }
            HirExprKind::BinaryOp { op, left, right } => {
                let op = *op;
                let left_id = *left;
                let right_id = *right;
                let left_ty = self.hir_exprs.get(left_id).ty.clone();
                match op {
                    BinOp::And => self.emit_and(left_id, right_id),
                    BinOp::Or => self.emit_or(left_id, right_id),
                    BinOp::Div | BinOp::Mod => self.emit_div_mod(left_id, right_id, op, &left_ty),
                    _ => {
                        let lval = self.emit_expr(left_id)?;
                        let rval = self.emit_expr(right_id)?;
                        self.emit_binop(op, lval, rval, &left_ty)
                    }
                }
            }
            HirExprKind::UnaryOp { op, operand } => {
                let op = *op;
                let operand_id = *operand;
                let operand_ty = self.hir_exprs.get(operand_id).ty.clone();
                let val = self.emit_expr(operand_id)?;
                match op {
                    UnaryOp::Neg => {
                        if operand_ty.is_float() {
                            Ok(self
                                .builder
                                .build_float_neg(val.into_float_value(), "fneg")
                                .unwrap()
                                .into())
                        } else {
                            Ok(self
                                .builder
                                .build_int_neg(val.into_int_value(), "neg")
                                .unwrap()
                                .into())
                        }
                    }
                    UnaryOp::Not => {
                        let bool_val = val.into_int_value();
                        Ok(self.builder.build_not(bool_val, "not").unwrap().into())
                    }
                }
            }
            HirExprKind::Cast { expr, target_type } => {
                let inner_id = *expr;
                let target = target_type.clone();
                let source = self.hir_exprs.get(inner_id).ty.clone();
                let val = self.emit_expr(inner_id)?;
                self.emit_cast(val, &source, &target)
            }
            HirExprKind::Call { func, args } => {
                let func_id = *func;
                let arg_ids = args.clone();
                let mut arg_vals: Vec<BasicMetadataValueEnum> = Vec::new();
                for arg_id in &arg_ids {
                    arg_vals.push(self.emit_expr(*arg_id)?.into());
                }

                let func_name = &self.func_names[func_id.0 as usize];
                let callee = self.func_values[func_id.0 as usize]
                    .ok_or_else(|| CodeGenError::UndefinedFunction(func_name.clone()))?;

                let call_val = self.builder.build_call(callee, &arg_vals, "call").unwrap();

                if callee.get_type().get_return_type().is_none() {
                    Ok(self.unit_value())
                } else {
                    Ok(call_val.try_as_basic_value().unwrap_basic())
                }
            }
            HirExprKind::Assign { var, value } => {
                let var_id = *var;
                let value_id = *value;
                let val = self.emit_expr(value_id)?;
                let var_info = &self.vars[var_id.0 as usize];
                let ptr = self.var_ptrs[var_id.0 as usize]
                    .ok_or_else(|| CodeGenError::UndefinedVariable(var_info.name.clone()))?;
                self.builder.build_store(ptr, val).unwrap();
                Ok(self.unit_value())
            }
            HirExprKind::If {
                condition,
                then_block,
                else_block,
            } => {
                let cond_id = *condition;
                // Clone blocks to avoid borrow issues
                let then_stmts = then_block.stmts.clone();
                let then_expr = then_block.expr;
                let else_clause = match else_block {
                    Some(HirElseClause::ElseBlock(block)) => {
                        Some((Some(block.stmts.clone()), Some(block.expr), None))
                    }
                    Some(HirElseClause::ElseIf(id)) => Some((None, None, Some(*id))),
                    None => None,
                };

                match else_clause {
                    Some(else_info) => {
                        let then_block_data = HirBlock {
                            stmts: then_stmts,
                            expr: then_expr,
                        };
                        self.emit_if_expr(cond_id, &then_block_data, else_info)
                    }
                    None => {
                        let then_block_data = HirBlock {
                            stmts: then_stmts,
                            expr: then_expr,
                        };
                        self.emit_if_stmt(cond_id, &then_block_data)
                    }
                }
            }
            HirExprKind::While {
                label,
                condition,
                body,
            } => {
                let label = label.clone();
                let cond_id = *condition;
                let body_stmts = body.stmts.clone();
                let body_expr = body.expr;
                let body_data = HirBlock {
                    stmts: body_stmts,
                    expr: body_expr,
                };
                self.emit_while(&label, cond_id, &body_data, &expr_ty)
            }
            HirExprKind::Match { expr, arms } => {
                let match_expr_id = *expr;
                let arms_data: Vec<_> = arms
                    .iter()
                    .map(|arm| (arm.pattern.clone(), arm.body))
                    .collect();
                self.emit_match(match_expr_id, &arms_data, &expr_ty)
            }
            HirExprKind::Block(block) => {
                let stmts = block.stmts.clone();
                let expr = block.expr;
                let block_data = HirBlock { stmts, expr };
                self.emit_block_value(&block_data)
            }
            HirExprKind::FieldAccess {
                expr,
                struct_name: _,
                field_index,
            } => {
                let inner_id = *expr;
                let field_index = *field_index;
                let val = self.emit_expr(inner_id)?;
                Ok(self
                    .builder
                    .build_extract_value(val.into_struct_value(), field_index as u32, "field")
                    .unwrap())
            }
            HirExprKind::TupleIndex { expr, index } => {
                let inner_id = *expr;
                let index = *index;
                let val = self.emit_expr(inner_id)?;
                Ok(self
                    .builder
                    .build_extract_value(val.into_struct_value(), index, "tuple.idx")
                    .unwrap())
            }
            HirExprKind::TupleExpr(elems) => {
                let elem_ids = elems.clone();
                let mut types: Vec<BasicTypeEnum> = Vec::new();
                for &eid in &elem_ids {
                    let ty = &self.hir_exprs.get(eid).ty;
                    types.push(self.llvm_type(ty)?);
                }
                let tuple_type = self.context.struct_type(&types, false);
                let mut tuple_val = tuple_type.get_undef();
                for (i, &eid) in elem_ids.iter().enumerate() {
                    let val = self.emit_expr(eid)?;
                    tuple_val = self
                        .builder
                        .build_insert_value(tuple_val, val, i as u32, "tuple.insert")
                        .unwrap()
                        .into_struct_value();
                }
                Ok(tuple_val.into())
            }
            HirExprKind::StructExpr { name, fields } => {
                let name = name.clone();
                let field_ids = fields.clone();
                let struct_type = self
                    .module
                    .get_struct_type(&format!("cz.{}", name))
                    .unwrap();
                let mut struct_val = struct_type.get_undef();
                // Fields are already in definition order
                for (i, &field_id) in field_ids.iter().enumerate() {
                    let val = self.emit_expr(field_id)?;
                    struct_val = self
                        .builder
                        .build_insert_value(struct_val, val, i as u32, "struct.insert")
                        .unwrap()
                        .into_struct_value();
                }
                Ok(struct_val.into())
            }
            HirExprKind::EnumExpr {
                enum_name,
                variant_index,
                args,
            } => {
                let enum_name = enum_name.clone();
                let variant_index = *variant_index;
                let args_data = match args {
                    HirEnumArgs::Unit => HirEnumArgsData::Unit,
                    HirEnumArgs::Tuple(ids) => HirEnumArgsData::Tuple(ids.clone()),
                    HirEnumArgs::Struct(ids) => HirEnumArgsData::Struct(ids.clone()),
                };
                self.emit_enum_construction(&enum_name, variant_index, &args_data)
            }
        }
    }

    // --- Binary operations ---

    fn emit_binop(
        &mut self,
        op: BinOp,
        lval: BasicValueEnum<'ctx>,
        rval: BasicValueEnum<'ctx>,
        ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        if ty.is_float() {
            let lf = lval.into_float_value();
            let rf = rval.into_float_value();
            Ok(match op {
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
            })
        } else {
            let li = lval.into_int_value();
            let ri = rval.into_int_value();
            Ok(match op {
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
            })
        }
    }

    fn emit_and(
        &mut self,
        left: HirExprId,
        right: HirExprId,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let i1_type = self.context.bool_type();
        let current_fn = self.current_function();

        let result_addr = self.builder.build_alloca(i1_type, "and.result").unwrap();

        let lval = self.emit_expr(left)?.into_int_value();

        let rhs_bb = self.context.append_basic_block(current_fn, "and.rhs");
        let false_bb = self.context.append_basic_block(current_fn, "and.false");
        let merge_bb = self.context.append_basic_block(current_fn, "and.merge");

        self.builder
            .build_conditional_branch(lval, rhs_bb, false_bb)
            .unwrap();

        self.builder.position_at_end(rhs_bb);
        let rval = self.emit_expr(right)?.into_int_value();
        self.builder.build_store(result_addr, rval).unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(false_bb);
        let false_val = i1_type.const_int(0, false);
        self.builder.build_store(result_addr, false_val).unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(merge_bb);
        Ok(self
            .builder
            .build_load(i1_type, result_addr, "and.val")
            .unwrap())
    }

    fn emit_or(
        &mut self,
        left: HirExprId,
        right: HirExprId,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let i1_type = self.context.bool_type();
        let current_fn = self.current_function();

        let result_addr = self.builder.build_alloca(i1_type, "or.result").unwrap();

        let lval = self.emit_expr(left)?.into_int_value();

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
        let rval = self.emit_expr(right)?.into_int_value();
        self.builder.build_store(result_addr, rval).unwrap();
        self.builder.build_unconditional_branch(merge_bb).unwrap();

        self.builder.position_at_end(merge_bb);
        Ok(self
            .builder
            .build_load(i1_type, result_addr, "or.val")
            .unwrap())
    }

    fn emit_div_mod(
        &mut self,
        left: HirExprId,
        right: HirExprId,
        op: BinOp,
        operand_ty: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        if operand_ty.is_float() {
            let lval = self.emit_expr(left)?.into_float_value();
            let rval = self.emit_expr(right)?.into_float_value();
            Ok(self
                .builder
                .build_float_div(lval, rval, "fdiv")
                .unwrap()
                .into())
        } else {
            let lval = self.emit_expr(left)?.into_int_value();
            let rval = self.emit_expr(right)?.into_int_value();

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
            Ok(match op {
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
            })
        }
    }

    // --- Cast ---

    fn emit_cast(
        &mut self,
        val: BasicValueEnum<'ctx>,
        source: &Type,
        target: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        if source == target {
            return Ok(val);
        }

        Ok(match (source, target) {
            // Integer -> Integer
            (s, t) if s.is_integer() && t.is_integer() => {
                let src_bits = int_bit_width(s);
                let dst_bits = int_bit_width(t);
                let dst_type = self.llvm_type(t).unwrap().into_int_type();
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
            (s, t) if s.is_integer() && t.is_float() => {
                let dst_type = self.llvm_type(t).unwrap().into_float_type();
                self.builder
                    .build_signed_int_to_float(val.into_int_value(), dst_type, "sitofp")
                    .unwrap()
                    .into()
            }
            // Float -> Integer
            (s, t) if s.is_float() && t.is_integer() => {
                let dst_type = self.llvm_type(t).unwrap().into_int_type();
                self.builder
                    .build_float_to_signed_int(val.into_float_value(), dst_type, "fptosi")
                    .unwrap()
                    .into()
            }
            // Float -> Float
            (s, t) if s.is_float() && t.is_float() => {
                let dst_type = self.llvm_type(t).unwrap().into_float_type();
                let fval = val.into_float_value();
                if float_bit_width(t) > float_bit_width(s) {
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
            (Type::Bool, t) if t.is_integer() => {
                let dst_type = self.llvm_type(t).unwrap().into_int_type();
                self.builder
                    .build_int_z_extend(val.into_int_value(), dst_type, "zext")
                    .unwrap()
                    .into()
            }
            // Integer -> Bool
            (s, Type::Bool) if s.is_integer() => {
                let zero = val.into_int_value().get_type().const_int(0, false);
                self.builder
                    .build_int_compare(IntPredicate::NE, val.into_int_value(), zero, "tobool")
                    .unwrap()
                    .into()
            }
            // Bool -> Float
            (Type::Bool, t) if t.is_float() => {
                let i32_val = self
                    .builder
                    .build_int_z_extend(val.into_int_value(), self.context.i32_type(), "bool2int")
                    .unwrap();
                let dst_type = self.llvm_type(t).unwrap().into_float_type();
                self.builder
                    .build_signed_int_to_float(i32_val, dst_type, "sitofp")
                    .unwrap()
                    .into()
            }
            // Float -> Bool
            (s, Type::Bool) if s.is_float() => {
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
            _ => val,
        })
    }

    // --- If expression ---

    fn emit_if_expr(
        &mut self,
        condition: HirExprId,
        then_block: &HirBlock,
        else_info: (
            Option<Vec<HirStmtId>>,
            Option<Option<HirExprId>>,
            Option<HirExprId>,
        ),
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let result_type = if let Some(tail) = then_block.expr {
            self.hir_exprs.get(tail).ty.clone()
        } else {
            Type::Unit
        };
        let llvm_result_type = self.llvm_type(&result_type)?;

        let current_fn = self.current_function();
        let result_addr = self
            .builder
            .build_alloca(llvm_result_type, "if.result")
            .unwrap();

        let cond_val = self.emit_expr(condition)?.into_int_value();

        let then_bb = self.context.append_basic_block(current_fn, "if.then");
        let else_bb = self.context.append_basic_block(current_fn, "if.else");
        let merge_bb = self.context.append_basic_block(current_fn, "if.merge");

        self.builder
            .build_conditional_branch(cond_val, then_bb, else_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        let then_val = self.emit_block_value(then_block)?;
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
        let else_val = match else_info {
            (Some(stmts), Some(expr), None) => {
                let else_block = HirBlock { stmts, expr };
                self.emit_block_value(&else_block)?
            }
            (None, None, Some(else_if_id)) => self.emit_expr(else_if_id)?,
            _ => self.unit_value(),
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
        Ok(self
            .builder
            .build_load(llvm_result_type, result_addr, "if.val")
            .unwrap())
    }

    fn emit_if_stmt(
        &mut self,
        condition: HirExprId,
        then_block: &HirBlock,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let current_fn = self.current_function();

        let cond_val = self.emit_expr(condition)?.into_int_value();

        let then_bb = self.context.append_basic_block(current_fn, "if.then");
        let end_bb = self.context.append_basic_block(current_fn, "if.end");

        self.builder
            .build_conditional_branch(cond_val, then_bb, end_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        // Emit then block statements and tail expr
        for &stmt_id in &then_block.stmts {
            self.emit_stmt(stmt_id)?;
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
        if let Some(tail) = then_block.expr {
            self.emit_expr(tail)?;
        }
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
        Ok(self.unit_value())
    }

    // --- While ---

    fn emit_while(
        &mut self,
        label: &Option<String>,
        condition: HirExprId,
        body: &HirBlock,
        while_type: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let current_fn = self.current_function();
        let cond_bb = self.context.append_basic_block(current_fn, "while.cond");
        let body_bb = self.context.append_basic_block(current_fn, "while.body");
        let end_bb = self.context.append_basic_block(current_fn, "while.end");

        let result_addr = if *while_type != Type::Unit {
            let llvm_ty = self.llvm_type(while_type)?;
            Some(self.builder.build_alloca(llvm_ty, "while.result").unwrap())
        } else {
            None
        };

        self.builder.build_unconditional_branch(cond_bb).unwrap();

        self.builder.position_at_end(cond_bb);
        let cond_val = self.emit_expr(condition)?.into_int_value();
        self.builder
            .build_conditional_branch(cond_val, body_bb, end_bb)
            .unwrap();

        self.builder.position_at_end(body_bb);
        self.loop_stack.push(super::LoopInfo {
            break_bb: end_bb,
            cond_bb,
            result_addr,
            label: label.clone(),
        });
        for &stmt_id in &body.stmts {
            self.emit_stmt(stmt_id)?;
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
        if let Some(tail) = body.expr {
            self.emit_expr(tail)?;
        }
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
        if let Some(addr) = result_addr {
            let llvm_ty = self.llvm_type(while_type)?;
            Ok(self.builder.build_load(llvm_ty, addr, "while.val").unwrap())
        } else {
            Ok(self.unit_value())
        }
    }

    // --- Match ---

    fn emit_match(
        &mut self,
        match_expr_id: HirExprId,
        arms: &[(Pattern, HirExprId)],
        _result_type_hint: &Type,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let match_val = self.emit_expr(match_expr_id)?;
        let match_type = self.hir_exprs.get(match_expr_id).ty.clone();

        // Determine result type from first arm body
        let result_type = if let Some((_, body_id)) = arms.first() {
            self.hir_exprs.get(*body_id).ty.clone()
        } else {
            Type::Unit
        };
        let llvm_result_type = self.llvm_type(&result_type)?;

        let current_fn = self.current_function();
        let result_addr = self
            .builder
            .build_alloca(llvm_result_type, "match.result")
            .unwrap();
        let merge_bb = self.context.append_basic_block(current_fn, "match.merge");

        // Store match value in alloca for pattern use
        let match_llvm_type = self.llvm_type(&match_type)?;
        let match_addr = self
            .builder
            .build_alloca(match_llvm_type, "match.val")
            .unwrap();
        self.builder.build_store(match_addr, match_val).unwrap();

        for (i, (pattern, body_id)) in arms.iter().enumerate() {
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

            let test_result = self.emit_pattern_test(pattern, match_val_loaded, &match_type);
            self.builder
                .build_conditional_branch(test_result, body_bb, next_bb)
                .unwrap();

            self.builder.position_at_end(body_bb);

            // Collect binding VarIds for this arm
            // The lowering allocated VarIds using collect_pattern_bindings which traverses
            // the pattern in a specific order. We need to find those VarIds.
            let binding_names = collect_binding_names_from_pattern(pattern);
            let binding_var_ids = self.find_match_binding_var_ids(&binding_names, &match_type);
            let mut var_id_iter = binding_var_ids.iter();

            self.emit_pattern_bindings(pattern, match_val_loaded, &match_type, &mut var_id_iter);

            let arm_val = self.emit_expr(*body_id)?;
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

            if i + 1 < arms.len() {
                self.builder.position_at_end(next_bb);
            }
        }

        // If we fall through (no match), unreachable
        let current_block = self.builder.get_insert_block().unwrap();
        if current_block != merge_bb && current_block.get_terminator().is_none() {
            self.builder.build_unreachable().unwrap();
        }

        self.builder.position_at_end(merge_bb);
        Ok(self
            .builder
            .build_load(llvm_result_type, result_addr, "match.result")
            .unwrap())
    }

    // --- Enum construction ---

    fn emit_enum_construction(
        &mut self,
        enum_name: &str,
        variant_index: usize,
        args: &HirEnumArgsData,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        let variants = self.enum_types.get(enum_name).unwrap().clone();

        let enum_llvm_type = self.llvm_type(&Type::Named(enum_name.to_string()))?;
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
        let tag_val = i32_type.const_int(variant_index as u64, false);
        self.builder.build_store(tag_ptr, tag_val).unwrap();

        // Set payload
        let variant_kind = variants[variant_index].1.clone();
        match (&variant_kind, args) {
            (VariantKind::Unit, _) => {}
            (VariantKind::Tuple(types), HirEnumArgsData::Tuple(expr_ids)) => {
                let payload_ptr = self.get_enum_payload_ptr(enum_llvm_type, enum_addr);
                let types = types.clone();
                let mut offset: u64 = 0;
                for (expr_id, field_type) in expr_ids.iter().zip(types.iter()) {
                    let val = self.emit_expr(*expr_id)?;
                    let field_ptr = self.get_enum_field_ptr(payload_ptr, offset);
                    self.builder.build_store(field_ptr, val).unwrap();
                    offset += type_size(field_type, &self.struct_types, &self.enum_types);
                }
            }
            (VariantKind::Struct(fields), HirEnumArgsData::Struct(expr_ids)) => {
                // Fields are already in definition order from lowering
                let payload_ptr = self.get_enum_payload_ptr(enum_llvm_type, enum_addr);
                let mut offset: u64 = 0;
                for (expr_id, field_def) in expr_ids.iter().zip(fields.iter()) {
                    let val = self.emit_expr(*expr_id)?;
                    let field_ptr = self.get_enum_field_ptr(payload_ptr, offset);
                    self.builder.build_store(field_ptr, val).unwrap();
                    offset +=
                        type_size(&field_def.field_type, &self.struct_types, &self.enum_types);
                }
            }
            _ => {}
        }

        Ok(self
            .builder
            .build_load(enum_llvm_type, enum_addr, "enum.val")
            .unwrap())
    }
}

/// HirEnumArgs のデータコピー (borrowing 回避用)。
enum HirEnumArgsData {
    Unit,
    Tuple(Vec<HirExprId>),
    Struct(Vec<HirExprId>),
}

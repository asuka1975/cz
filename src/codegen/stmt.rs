use crate::hir::*;

use inkwell::values::BasicValueEnum;

use super::{CodeGen, CodeGenError};

impl<'ctx> CodeGen<'ctx> {
    /// 文を生成する。
    pub(crate) fn emit_stmt(&mut self, stmt_id: HirStmtId) -> Result<(), CodeGenError> {
        let stmt = self.hir_stmts.get(stmt_id);
        match stmt {
            HirStmt::Let { var, init, .. } => {
                let var_id = *var;
                let init_id = *init;
                let val = self.emit_expr(init_id)?;
                let var_info = &self.vars[var_id.0 as usize];
                let llvm_ty = self.llvm_type(&var_info.ty)?;
                let alloca = self.builder.build_alloca(llvm_ty, &var_info.name).unwrap();
                self.builder.build_store(alloca, val).unwrap();
                self.var_ptrs[var_id.0 as usize] = Some(alloca);
            }
            HirStmt::Return { value, .. } => {
                let value = *value;
                if let Some(val_id) = value {
                    let val = self.emit_expr(val_id)?;
                    self.builder.build_return(Some(&val)).unwrap();
                } else {
                    self.builder.build_return(None).unwrap();
                }
                let current_fn = self.current_function();
                let after_bb = self.context.append_basic_block(current_fn, "after.ret");
                self.builder.position_at_end(after_bb);
            }
            HirStmt::Break { label, value, .. } => {
                let label = label.clone();
                let value = *value;
                let loop_info_idx = if let Some(ref label_name) = label {
                    self.loop_stack
                        .iter()
                        .rposition(|l| l.label.as_deref() == Some(label_name))
                        .ok_or_else(|| CodeGenError::LabelNotFound(label_name.clone()))?
                } else {
                    self.loop_stack.len() - 1
                };

                if let Some(val_id) = value {
                    let val = self.emit_expr(val_id)?;
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
            HirStmt::Continue { label, .. } => {
                let label = label.clone();
                let loop_info_idx = if let Some(ref label_name) = label {
                    self.loop_stack
                        .iter()
                        .rposition(|l| l.label.as_deref() == Some(label_name))
                        .ok_or_else(|| CodeGenError::LabelNotFound(label_name.clone()))?
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
            HirStmt::Expr(expr_id) => {
                let expr_id = *expr_id;
                self.emit_expr(expr_id)?;
            }
        }
        Ok(())
    }

    /// ブロック内の文を生成し、末尾式があればその値を返す。
    pub(crate) fn emit_block_value(
        &mut self,
        block: &HirBlock,
    ) -> Result<BasicValueEnum<'ctx>, CodeGenError> {
        for &stmt_id in &block.stmts {
            self.emit_stmt(stmt_id)?;
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
        if let Some(expr_id) = block.expr {
            self.emit_expr(expr_id)
        } else {
            Ok(self.unit_value())
        }
    }

    /// 関数のトップレベルブロックを生成する (末尾で return を追加)。
    pub(crate) fn emit_block_as_return(
        &mut self,
        block: &HirBlock,
        return_type: &crate::hir::types::Type,
    ) -> Result<(), CodeGenError> {
        for &stmt_id in &block.stmts {
            self.emit_stmt(stmt_id)?;
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
        if let Some(expr_id) = block.expr {
            if *return_type == crate::hir::types::Type::Unit {
                self.emit_expr(expr_id)?;
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    self.builder.build_return(None).unwrap();
                }
            } else {
                let val = self.emit_expr(expr_id)?;
                let current_block = self.builder.get_insert_block().unwrap();
                if current_block.get_terminator().is_none() {
                    self.builder.build_return(Some(&val)).unwrap();
                }
            }
        }
        let current_block = self.builder.get_insert_block().unwrap();
        if current_block.get_terminator().is_none() {
            if *return_type == crate::hir::types::Type::Unit {
                self.builder.build_return(None).unwrap();
            } else {
                self.builder.build_unreachable().unwrap();
            }
        }
        Ok(())
    }
}

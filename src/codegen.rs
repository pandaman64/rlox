use std::{
    cell::{Cell, RefCell},
    convert::TryFrom,
};

use crate::{
    ast::{
        BinOpKind, BlockStmt, Decl, Expr, ForInit, Identifier, Primary, Stmt, UnaryOpKind, VarDecl,
    },
    line_map::LineMap,
    opcode::{Chunk, OpCode},
    table::InternedStr,
    value::Value,
    vm::{
        object::{self, RawFunction, RawObject},
        Vm,
    },
};

const GLOBAL_BLOCK: usize = 0;
const UNASSIGNED: usize = usize::MAX;

#[derive(Debug)]
pub enum CodegenError {
    ShadowingInSameScope { ident: String, position: usize },
    UnassignedLocal { ident: Identifier },
    LoopTooLarge { position: usize },
    ReturnFromTopLevel { position: usize },
    TooManyLocalVariables { position: usize },
    TooManyConstants { position: usize },
    TooManyParameters { position: usize },
    TooManyArguments { position: usize },
    TooManyUpvalues { position: usize },
}

struct Local {
    ident: String,
    depth: usize,
    captured: Cell<bool>,
}

pub struct Upvalue {
    index: u8,
    is_local: bool,
}

#[derive(Debug)]
pub enum FunctionKind {
    Function,
    Script,
}

pub struct Compiler<'parent, 'map> {
    parent: Option<&'parent Compiler<'parent, 'map>>,
    function: RawFunction,
    kind: FunctionKind,
    locals: Vec<Local>,
    upvalues: RefCell<Vec<Upvalue>>,
    block_depth: usize,
    line_map: &'map LineMap,
    return_position: Option<usize>,
    errors: RefCell<Vec<CodegenError>>,
}

fn new_locals() -> Vec<Local> {
    vec![
        // This corresponds to the callee, and local variables starts with index 1
        Local {
            ident: "<callee>".into(),
            depth: 0,
            captured: Cell::new(false),
        },
    ]
}

fn mark_nothing(_worklist: &mut Vec<RawObject>) {}

impl<'parent, 'map> Compiler<'parent, 'map> {
    pub fn new_script(vm: &mut Vm<'_>, line_map: &'map LineMap) -> Self {
        Self {
            parent: None,
            function: vm.allocate_script(mark_nothing),
            kind: FunctionKind::Script,
            locals: new_locals(),
            upvalues: RefCell::new(vec![]),
            block_depth: 0,
            line_map,
            return_position: None,
            errors: RefCell::new(vec![]),
        }
    }

    fn new_function(
        vm: &mut Vm<'_>,
        parent: &'parent Compiler<'parent, 'map>,
        name: InternedStr,
        arity: u8,
        line_map: &'map LineMap,
        return_position: usize,
    ) -> Self {
        Self {
            parent: Some(parent),
            function: vm.allocate_function(name, arity, 0, mark_nothing),
            kind: FunctionKind::Function,
            locals: new_locals(),
            upvalues: RefCell::new(vec![]),
            block_depth: 0,
            line_map,
            return_position: Some(return_position),
            errors: RefCell::new(vec![]),
        }
    }

    fn mark(&self) -> impl FnOnce(&mut Vec<RawObject>) + '_ {
        move |worklist| {
            // SAFETY: self owns the function and must not be deallocated by vm
            unsafe {
                object::mark(self.function.cast(), worklist);
            }
        }
    }

    fn chunk(&self) -> &Chunk {
        // SAFETY: self owns the function and must not be deallocated by vm
        unsafe { self.function.as_ref().chunk() }
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        // SAFETY: self owns the function and must not be deallocated by vm
        unsafe { self.function.as_mut().chunk_mut() }
    }

    /// - Returns `Some((function, upvalues, error))` if the code contains at least one declaration.
    /// - Returns `None` if the code does not contain any declaration.
    pub fn finish(mut self) -> Option<(RawFunction, Vec<Upvalue>, Vec<CodegenError>)> {
        let return_position = self.return_position?;
        self.gen_return(return_position);
        Some((
            self.function,
            self.upvalues.into_inner(),
            self.errors.into_inner(),
        ))
    }

    fn push_local(&mut self, ident: Identifier) {
        let name = ident.to_str();
        for local in self.locals.iter().rev() {
            if local.depth != self.block_depth {
                break;
            }

            if local.ident == name {
                self.errors
                    .get_mut()
                    .push(CodegenError::ShadowingInSameScope {
                        ident: name.into(),
                        position: ident.start(),
                    });
                break;
            }
        }

        if self.locals.len() < 256 {
            self.locals.push(Local {
                ident: name.into(),
                depth: UNASSIGNED,
                captured: Cell::new(false),
            });
        } else {
            self.errors
                .get_mut()
                .push(CodegenError::TooManyLocalVariables {
                    position: ident.start(),
                });
        }
    }

    fn begin_block(&mut self) {
        self.block_depth += 1;
    }

    fn end_block(&mut self, position: usize) {
        while let Some(local) = self.locals.last() {
            if local.depth != self.block_depth {
                break;
            }
            if !local.captured.get() {
                self.push_opcode(OpCode::Pop, position);
            } else {
                self.push_opcode(OpCode::CloseUpvalue, position);
            }
            self.locals.pop();
        }
        self.block_depth -= 1;
    }

    fn resolve_local(&self, ident: Identifier) -> Option<usize> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| {
                if local.ident == ident.to_str() {
                    if local.depth == UNASSIGNED {
                        self.errors
                            .borrow_mut()
                            .push(CodegenError::UnassignedLocal {
                                ident: ident.clone(),
                            });
                    }
                    true
                } else {
                    false
                }
            })
            .map(|(i, _)| i)
    }

    fn add_upvalue(&self, index: u8, is_local: bool, position: usize) -> usize {
        let mut upvalues = self.upvalues.borrow_mut();

        if let Some(i) = upvalues
            .iter()
            .position(|upvalue| upvalue.index == index && upvalue.is_local == is_local)
        {
            return i;
        }

        let ret = upvalues.len();

        if ret < 256 {
            upvalues.push(Upvalue { index, is_local });
            ret
        } else {
            self.errors
                .borrow_mut()
                .push(CodegenError::TooManyUpvalues { position });
            255
        }
    }

    fn resolve_upvalue(&self, ident: Identifier) -> Option<usize> {
        let parent = self.parent?;
        let position = ident.start();
        match parent.resolve_local(ident.clone()) {
            Some(local) => {
                parent.locals[local].captured.set(true);
                Some(self.add_upvalue(u8::try_from(local).unwrap(), true, position))
            }
            None => {
                let index = parent.resolve_upvalue(ident)?;
                Some(self.add_upvalue(u8::try_from(index).unwrap(), false, position))
            }
        }
    }

    fn resolve(&mut self, vm: &mut Vm<'_>, ident: Identifier) -> (OpCode, OpCode, u8) {
        if let Some(i) = self.resolve_local(ident.clone()) {
            (OpCode::GetLocal, OpCode::SetLocal, u8::try_from(i).unwrap())
        } else if let Some(i) = self.resolve_upvalue(ident.clone()) {
            (
                OpCode::GetUpvalue,
                OpCode::SetUpvalue,
                u8::try_from(i).unwrap(),
            )
        } else {
            let position = ident.start();
            let ident = vm.allocate_string(ident.to_str().into(), self.mark());
            let index = self.push_constant(Value::Object(ident.into_raw_obj()), position);

            (OpCode::GetGlobal, OpCode::SetGlobal, index)
        }
    }

    fn push_opcode(&mut self, code: OpCode, position: usize) {
        let line = self.line_map.resolve(position);
        self.chunk_mut().push_code(code as u8, line);
    }

    fn push_u8(&mut self, constant: u8, position: usize) {
        let line = self.line_map.resolve(position);
        self.chunk_mut().push_code(constant, line);
    }

    fn push_constant(&mut self, value: Value, position: usize) -> u8 {
        if self.chunk().constants().len() < 256 {
            self.chunk_mut().push_constant(value)
        } else {
            self.errors
                .get_mut()
                .push(CodegenError::TooManyConstants { position });
            0
        }
    }

    fn allocate_jump_location(&mut self, position: usize) -> usize {
        let line = self.line_map.resolve(position);
        self.chunk_mut().allocate_jump_location(line)
    }

    fn fill_jump_location(&mut self, jump: usize, ip: usize, position: usize) {
        if self.chunk_mut().fill_jump_location(jump, ip).is_err() {
            self.errors
                .get_mut()
                .push(CodegenError::LoopTooLarge { position })
        };
    }

    fn fill_jump_location_with_current(&mut self, jump: usize, position: usize) {
        if self
            .chunk_mut()
            .fill_jump_location_with_current(jump)
            .is_err()
        {
            self.errors
                .get_mut()
                .push(CodegenError::LoopTooLarge { position })
        }
    }

    fn current_ip(&self) -> usize {
        self.chunk().code().len()
    }

    fn gen_return(&mut self, position: usize) {
        self.push_opcode(OpCode::Nil, position);
        self.push_opcode(OpCode::Return, position);
    }

    fn gen_place(&mut self, vm: &mut Vm<'_>, expr: Expr) -> (bool, Identifier) {
        match expr {
            Expr::Primary(Primary::Identifier(ident)) => (false, ident),
            Expr::BinOp(expr) if expr.kind() == BinOpKind::Dot => {
                let mut operands = expr.operands();
                let target = operands.next().unwrap();
                let field = operands.next().unwrap();
                let ident = match field {
                    Expr::Primary(Primary::Identifier(ident)) => ident,
                    _ => unreachable!("non-identifier expression is used as field"),
                };
                self.gen_expr(vm, target);
                (true, ident)
            }
            _ => unreachable!("place expression must be an identifier or dot expression"),
        }
    }

    fn gen_expr(&mut self, vm: &mut Vm<'_>, expr: Expr) {
        match expr {
            Expr::ParenExpr(expr) => {
                self.gen_expr(vm, expr.expr().unwrap());
            }
            Expr::UnaryOp(expr) => {
                let operand = expr.operand().unwrap();
                self.gen_expr(vm, operand);
                let opcode = match expr.kind() {
                    UnaryOpKind::Negation => OpCode::Negate,
                    UnaryOpKind::Not => OpCode::Not,
                };
                self.push_opcode(opcode, expr.start());
            }
            Expr::BinOp(expr) => {
                let opcodes: &[OpCode] = match expr.kind() {
                    BinOpKind::Assignment => {
                        let mut operands = expr.operands();
                        let lhs = operands.next().unwrap();
                        let rhs = operands.next().unwrap();
                        let (has_target, ident) = self.gen_place(vm, lhs);
                        if has_target {
                            todo!()
                        } else {
                            let (_get_op, set_op, index) = self.resolve(vm, ident);
                            self.gen_expr(vm, rhs);
                            self.push_opcode(set_op, expr.start());
                            self.push_u8(index, expr.start());
                        }
                        return;
                    }
                    BinOpKind::Or => {
                        let expr_end = expr.end();
                        let mut operands = expr.operands();
                        let lhs = operands.next().unwrap();
                        let rhs = operands.next().unwrap();

                        self.gen_expr(vm, lhs);

                        self.push_opcode(OpCode::JumpIfFalse, rhs.start());
                        let rhs_jump = self.allocate_jump_location(expr_end);

                        self.push_opcode(OpCode::Jump, rhs.start());
                        let end_jump = self.allocate_jump_location(expr_end);

                        self.fill_jump_location_with_current(rhs_jump, expr_end);
                        self.push_opcode(OpCode::Pop, rhs.start());
                        self.gen_expr(vm, rhs);

                        self.fill_jump_location_with_current(end_jump, expr_end);
                        return;
                    }
                    BinOpKind::And => {
                        let expr_end = expr.end();
                        let mut operands = expr.operands();
                        let lhs = operands.next().unwrap();
                        let rhs = operands.next().unwrap();

                        self.gen_expr(vm, lhs);

                        self.push_opcode(OpCode::JumpIfFalse, rhs.start());
                        let end_jump = self.allocate_jump_location(expr_end);

                        self.push_opcode(OpCode::Pop, rhs.start());
                        self.gen_expr(vm, rhs);

                        self.fill_jump_location_with_current(end_jump, expr_end);
                        return;
                    }
                    BinOpKind::Equal => &[OpCode::Equal],
                    BinOpKind::NotEqual => &[OpCode::Equal, OpCode::Not],
                    BinOpKind::Less => &[OpCode::Less],
                    BinOpKind::Greater => &[OpCode::Greater],
                    BinOpKind::LessEqual => &[OpCode::Greater, OpCode::Not],
                    BinOpKind::GreaterEqual => &[OpCode::Less, OpCode::Not],
                    BinOpKind::Add => &[OpCode::Add],
                    BinOpKind::Subtract => &[OpCode::Subtract],
                    BinOpKind::Multiply => &[OpCode::Multiply],
                    BinOpKind::Divide => &[OpCode::Divide],
                    BinOpKind::Dot => todo!(),
                };
                for operand in expr.operands() {
                    self.gen_expr(vm, operand);
                }
                for opcode in opcodes.iter() {
                    self.push_opcode(*opcode, expr.start());
                }
            }
            Expr::Primary(expr) => match expr {
                Primary::Identifier(ident) => {
                    let position = ident.start();
                    let (get_op, _set_op, index) = self.resolve(vm, ident);
                    self.push_opcode(get_op, position);
                    self.push_u8(index, position);
                }
                Primary::NilLiteral(n) => {
                    self.push_opcode(OpCode::Nil, n.start());
                }
                Primary::BooleanLiteral(b) => {
                    if b.to_boolean() {
                        self.push_opcode(OpCode::True, b.start());
                    } else {
                        self.push_opcode(OpCode::False, b.start());
                    }
                }
                Primary::StringLiteral(s) => {
                    let obj = vm.allocate_string(s.to_str().into(), self.mark());
                    let index = self.push_constant(Value::Object(obj.into_raw_obj()), s.start());
                    self.push_opcode(OpCode::Constant, s.start());
                    self.push_u8(index, s.start());
                }
                Primary::NumberLiteral(num) => {
                    let index = self.push_constant(Value::Number(num.to_number()), num.start());
                    self.push_opcode(OpCode::Constant, num.start());
                    self.push_u8(index, num.start());
                }
            },
            Expr::Call(expr) => {
                self.gen_expr(vm, expr.function().unwrap());
                let mut args = 0;
                for arg in expr.args() {
                    let position = arg.start();
                    self.gen_expr(vm, arg);
                    args += 1;

                    if args == 256 {
                        self.errors
                            .get_mut()
                            .push(CodegenError::TooManyArguments { position });
                        return;
                    }
                }
                self.push_opcode(OpCode::Call, expr.start());
                self.push_u8(u8::try_from(args).unwrap(), expr.start());
            }
        }
    }

    fn gen_block_stmt(&mut self, vm: &mut Vm<'_>, stmt: BlockStmt, start_block: bool) {
        if start_block {
            self.begin_block();
        }
        for decl in stmt.decls() {
            self.gen_decl(vm, decl);
        }
        if start_block {
            self.end_block(stmt.start());
        }
    }

    fn gen_stmt(&mut self, vm: &mut Vm<'_>, stmt: Stmt) {
        match stmt {
            Stmt::ExprStmt(stmt) => {
                let expr = stmt.expr().unwrap();
                let position = expr.start();
                self.gen_expr(vm, expr);
                self.push_opcode(OpCode::Pop, position);
            }
            Stmt::PrintStmt(stmt) => {
                self.gen_expr(vm, stmt.expr().unwrap());
                self.push_opcode(OpCode::Print, stmt.start());
            }
            Stmt::ReturnStmt(stmt) => match self.kind {
                FunctionKind::Script => {
                    self.errors
                        .get_mut()
                        .push(CodegenError::ReturnFromTopLevel {
                            position: stmt.start(),
                        })
                }
                FunctionKind::Function => {
                    match stmt.expr() {
                        Some(expr) => self.gen_expr(vm, expr),
                        None => self.push_opcode(OpCode::Nil, stmt.start()),
                    }
                    self.push_opcode(OpCode::Return, stmt.start());
                }
            },
            Stmt::BlockStmt(stmt) => self.gen_block_stmt(vm, stmt, true),
            Stmt::IfStmt(stmt) => {
                let stmt_end = stmt.end();
                let cond = stmt.cond().unwrap();
                let cond_position = cond.start();
                let mut branches = stmt.branches();
                let then_branch = branches.next().unwrap();
                let else_branch = branches.next();

                self.gen_expr(vm, cond);

                self.push_opcode(OpCode::JumpIfFalse, cond_position);
                let else_jump = self.allocate_jump_location(stmt_end);

                self.push_opcode(OpCode::Pop, cond_position);
                self.gen_stmt(vm, then_branch);

                self.push_opcode(OpCode::Jump, cond_position);
                let end_jump = self.allocate_jump_location(stmt_end);

                if let Some(else_branch) = else_branch {
                    self.fill_jump_location_with_current(else_jump, stmt_end);
                    self.push_opcode(OpCode::Pop, else_branch.start());
                    self.gen_stmt(vm, else_branch);
                } else {
                    self.fill_jump_location_with_current(else_jump, stmt_end);
                    self.push_opcode(OpCode::Pop, stmt.start());
                }

                self.fill_jump_location_with_current(end_jump, stmt_end);
            }
            Stmt::WhileStmt(stmt) => {
                let stmt_end = stmt.end();
                let cond = stmt.cond().unwrap();
                let body = stmt.body().unwrap();

                let start_loop_ip = self.current_ip();

                self.gen_expr(vm, cond);

                self.push_opcode(OpCode::JumpIfFalse, body.start());
                let end_jump = self.allocate_jump_location(stmt_end);

                self.push_opcode(OpCode::Pop, body.start());
                self.gen_stmt(vm, body);

                self.push_opcode(OpCode::Jump, stmt_end);
                let start_jump = self.allocate_jump_location(stmt_end);
                self.fill_jump_location(start_jump, start_loop_ip, stmt_end);

                self.fill_jump_location_with_current(end_jump, stmt_end);
                self.push_opcode(OpCode::Pop, stmt_end);
            }
            Stmt::ForStmt(stmt) => {
                let stmt_end = stmt.end();
                self.begin_block();

                match stmt.init() {
                    Some(ForInit::Expr(expr)) => {
                        let position = expr.start();
                        self.gen_expr(vm, expr);
                        self.push_opcode(OpCode::Pop, position);
                    }
                    Some(ForInit::VarDecl(decl)) => self.gen_var_decl(vm, decl),
                    _ => {}
                }

                let start_loop_ip = self.current_ip();

                let end_jump = stmt.cond().unwrap().expr().map(|expr| {
                    let position = expr.start();
                    self.gen_expr(vm, expr);
                    self.push_opcode(OpCode::JumpIfFalse, position);
                    let end_jump = self.allocate_jump_location(stmt_end);
                    self.push_opcode(OpCode::Pop, position);
                    end_jump
                });

                self.gen_stmt(vm, stmt.body().unwrap());

                if let Some(expr) = stmt.incr().unwrap().expr() {
                    let position = expr.start();
                    self.gen_expr(vm, expr);
                    self.push_opcode(OpCode::Pop, position);
                }

                self.push_opcode(OpCode::Jump, stmt_end);
                let start_jump = self.allocate_jump_location(stmt_end);
                self.fill_jump_location(start_jump, start_loop_ip, stmt_end);

                if let Some(end_jump) = end_jump {
                    self.fill_jump_location_with_current(end_jump, stmt_end);
                }
                self.push_opcode(OpCode::Pop, stmt_end);

                self.end_block(stmt_end);
            }
        }
    }

    fn define_variable<F>(
        &mut self,
        vm: &mut Vm<'_>,
        ident: Identifier,
        gen_value: F,
        allow_reference_in_value: bool,
    ) where
        F: FnOnce(&mut Self, &mut Vm<'_>) -> usize,
    {
        if self.block_depth > GLOBAL_BLOCK {
            let idx = self.locals.len();
            self.push_local(ident);
            // if allow_reference_in_value is set, we declare that the local is initialized already,
            // so that the generated value can refer to the variable defined.
            if allow_reference_in_value {
                self.locals[idx].depth = self.block_depth;
            }
            gen_value(self, vm);
            // it is possible that the locals slot overflows and this local is not inserted
            if !allow_reference_in_value && idx < self.locals.len() {
                self.locals[idx].depth = self.block_depth;
            }
        } else {
            let position = ident.start();
            let ident = vm.allocate_string(ident.to_str().into(), self.mark());
            let index = self.push_constant(Value::Object(ident.into_raw_obj()), position);
            let position = gen_value(self, vm);
            self.push_opcode(OpCode::DefineGlobal, position);
            self.push_u8(index, position);
        }
    }

    fn gen_var_decl(&mut self, vm: &mut Vm<'_>, decl: VarDecl) {
        let ident = decl.ident().unwrap();
        let expr = decl.expr();
        self.define_variable(
            vm,
            ident,
            move |this, vm| match expr {
                Some(expr) => {
                    this.gen_expr(vm, expr);
                    decl.start()
                }
                None => {
                    this.push_opcode(OpCode::Nil, decl.start());
                    decl.start()
                }
            },
            false,
        );
    }

    pub fn gen_decl(&mut self, vm: &mut Vm<'_>, decl: Decl) {
        self.return_position = Some(decl.return_position());

        match decl {
            Decl::VarDecl(decl) => self.gen_var_decl(vm, decl),
            Decl::FunDecl(decl) => {
                let name = decl.ident().unwrap();
                let name = vm.allocate_string(name.to_str().into(), self.mark());
                let arity = match u8::try_from(decl.params().count()) {
                    Ok(arity) => arity,
                    Err(_) => {
                        let ident = decl.params().nth(255).unwrap();
                        self.errors.get_mut().push(CodegenError::TooManyParameters {
                            position: ident.start(),
                        });
                        return;
                    }
                };

                // register name as GC root
                vm.push(Value::Object(name.into_raw_obj()));

                self.define_variable(
                    vm,
                    decl.ident().unwrap(),
                    move |this, vm| {
                        let mut compiler = Compiler::new_function(
                            vm,
                            this,
                            name,
                            arity,
                            this.line_map,
                            decl.return_position(),
                        );
                        compiler.begin_block();
                        for param in decl.params() {
                            compiler.push_local(param);
                            compiler.locals.last_mut().unwrap().depth = compiler.block_depth;
                        }
                        compiler.gen_block_stmt(vm, decl.body().unwrap(), false);
                        compiler.end_block(decl.start());
                        let (mut fun_obj, upvalues, errors) = compiler.finish().unwrap();
                        // the function is valid
                        unsafe {
                            *fun_obj.as_mut().upvalues_mut() =
                                u8::try_from(upvalues.len()).unwrap_or(255);
                        }
                        this.errors.get_mut().extend(errors);

                        let index = this.push_constant(Value::Object(fun_obj.cast()), decl.start());
                        this.push_opcode(OpCode::Closure, decl.start());
                        this.push_u8(index, decl.start());

                        for upvalue in upvalues.iter() {
                            let is_local = if upvalue.is_local { 1 } else { 0 };
                            this.push_u8(is_local, decl.start());
                            this.push_u8(upvalue.index, decl.start());
                        }

                        decl.start()
                    },
                    // functions can refer to themselves inside their body because they are executed
                    // only after the initialization.
                    true,
                );

                // remove name from GC root
                vm.pop();
            }
            Decl::Stmt(stmt) => self.gen_stmt(vm, stmt),
        }
    }
}

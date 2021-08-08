use crate::{
    ast::{
        BinOpKind, BlockStmt, Decl, Expr, ForInit, Identifier, Primary, Stmt, UnaryOpKind, VarDecl,
    },
    object::Function,
    opcode::{Chunk, OpCode},
    table::InternedStr,
    value::Value,
    vm::Vm,
};

const GLOBAL_BLOCK: usize = 0;

struct Local {
    ident: String,
    depth: usize,
}

#[derive(Debug)]
pub enum FunctionKind {
    Function,
    Script,
}

pub struct Compiler<'parent> {
    parent: Option<&'parent Compiler<'parent>>,
    function: Function,
    kind: FunctionKind,
    locals: Vec<Local>,
    block_depth: usize,
}

impl<'parent> Compiler<'parent> {
    pub fn new_script() -> Self {
        Self {
            parent: None,
            function: Function::new_script(),
            kind: FunctionKind::Script,
            locals: vec![
                // used by compiler
                // Local {
                //     ident: "".into(),
                //     depth: 0,
                // },
            ],
            block_depth: 0,
        }
    }

    fn new_function(parent: &'parent Compiler<'parent>, name: InternedStr, arity: u32) -> Self {
        Self {
            parent: Some(parent),
            function: Function::new_function(name, arity),
            kind: FunctionKind::Function,
            locals: vec![],
            block_depth: 0,
        }
    }

    pub fn finish(mut self) -> Function {
        self.gen_return();
        self.function
    }

    fn push_local(&mut self, ident: &str) {
        assert!(
            self.locals.len() < 256,
            "more than 255 local variables are not supported"
        );

        for local in self.locals.iter().rev() {
            if local.depth != self.block_depth {
                break;
            }

            if local.ident == ident {
                panic!("shadowing in the same scope is not supported: {}", ident);
            }
        }

        self.locals.push(Local {
            ident: ident.into(),
            depth: usize::MAX,
        });
    }

    fn begin_block(&mut self) {
        self.block_depth += 1;
    }

    fn end_block(&mut self) {
        while let Some(local) = self.locals.last() {
            if local.depth != self.block_depth {
                break;
            }
            self.locals.pop();
            self.chunk_mut().push_code(OpCode::Pop as _, 0);
        }
        self.block_depth -= 1;
    }

    fn resolve_local(&self, ident: &str) -> Option<usize> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| {
                if local.ident == ident {
                    assert_ne!(
                        local.depth,
                        usize::MAX,
                        "initializer of local variable cannot read the local"
                    );
                    true
                } else {
                    false
                }
            })
            .map(|(i, _)| i)
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        self.function.chunk_mut()
    }

    fn gen_return(&mut self) {
        self.chunk_mut().push_code(OpCode::Nil as _, 0);
        self.chunk_mut().push_code(OpCode::Return as _, 0);
    }

    fn gen_place(&mut self, vm: &mut Vm, expr: Expr) -> (bool, Identifier) {
        match expr {
            Expr::Primary(Primary::Identifier(ident)) => (false, ident),
            Expr::BinOp(expr) if expr.kind() == BinOpKind::Dot => {
                let mut operands = expr.operands();
                let target = operands.next().unwrap();
                let field = operands.next().unwrap();
                let ident = match field {
                    Expr::Primary(Primary::Identifier(ident)) => ident,
                    _ => todo!("non-identifier expression is used as field"),
                };
                self.gen_expr(vm, target);
                (true, ident)
            }
            // TODO: emit error message
            _ => todo!("place expression must be an identifier or dot expression"),
        }
    }

    fn gen_expr(&mut self, vm: &mut Vm, expr: Expr) {
        match expr {
            Expr::UnaryOp(expr) => {
                let operand = Expr::cast(expr.operand().unwrap()).unwrap();
                self.gen_expr(vm, operand);
                let opcode = match expr.kind() {
                    UnaryOpKind::Negation => OpCode::Negate,
                    UnaryOpKind::Not => OpCode::Not,
                };
                self.chunk_mut().push_code(opcode as _, 0);
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
                            match self.resolve_local(ident.to_str()) {
                                Some(i) => {
                                    self.gen_expr(vm, rhs);
                                    self.chunk_mut().push_code(OpCode::SetLocal as _, 0);
                                    self.chunk_mut().push_code(i as _, 0);
                                }
                                None => {
                                    let ident = vm.allocate_string(ident.to_str().into());
                                    let index = self
                                        .chunk_mut()
                                        .push_constant(Value::Object(ident.into_raw_obj()));

                                    self.gen_expr(vm, rhs);
                                    self.chunk_mut().push_code(OpCode::SetGlobal as _, 0);
                                    self.chunk_mut().push_code(index, 0);
                                }
                            }
                        }
                        return;
                    }
                    BinOpKind::Or => {
                        let mut operands = expr.operands();
                        let lhs = operands.next().unwrap();
                        let rhs = operands.next().unwrap();

                        self.gen_expr(vm, lhs);

                        self.chunk_mut().push_code(OpCode::JumpIfFalse as _, 0);
                        let rhs_jump = self.chunk_mut().allocate_jump_location(0);

                        self.chunk_mut().push_code(OpCode::Jump as _, 0);
                        let end_jump = self.chunk_mut().allocate_jump_location(0);

                        self.chunk_mut().fill_jump_location_with_current(rhs_jump);
                        self.chunk_mut().push_code(OpCode::Pop as _, 0);
                        self.gen_expr(vm, rhs);

                        self.chunk_mut().fill_jump_location_with_current(end_jump);
                        return;
                    }
                    BinOpKind::And => {
                        let mut operands = expr.operands();
                        let lhs = operands.next().unwrap();
                        let rhs = operands.next().unwrap();

                        self.gen_expr(vm, lhs);

                        self.chunk_mut().push_code(OpCode::JumpIfFalse as _, 0);
                        let end_jump = self.chunk_mut().allocate_jump_location(0);

                        self.chunk_mut().push_code(OpCode::Pop as _, 0);
                        self.gen_expr(vm, rhs);

                        self.chunk_mut().fill_jump_location_with_current(end_jump);
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
                    self.chunk_mut().push_code(*opcode as _, 0);
                }
            }
            Expr::Primary(expr) => match expr {
                Primary::Identifier(ident) => match self.resolve_local(ident.to_str()) {
                    Some(i) => {
                        self.chunk_mut().push_code(OpCode::GetLocal as _, 0);
                        self.chunk_mut().push_code(i as u8, 0);
                    }
                    None => {
                        let ident = vm.allocate_string(ident.to_str().into());
                        let index = self
                            .chunk_mut()
                            .push_constant(Value::Object(ident.into_raw_obj()));

                        self.chunk_mut().push_code(OpCode::GetGlobal as _, 0);
                        self.chunk_mut().push_code(index, 0);
                    }
                },
                Primary::NilLiteral(_) => {
                    self.chunk_mut().push_code(OpCode::Nil as _, 0);
                }
                Primary::BooleanLiteral(b) => {
                    if b.to_boolean() {
                        self.chunk_mut().push_code(OpCode::True as _, 0);
                    } else {
                        self.chunk_mut().push_code(OpCode::False as _, 0);
                    }
                }
                Primary::StringLiteral(s) => {
                    let obj = vm.allocate_string(s.to_str().into());
                    let index = self
                        .chunk_mut()
                        .push_constant(Value::Object(obj.into_raw_obj()));
                    self.chunk_mut().push_code(OpCode::Constant as _, 0);
                    self.chunk_mut().push_code(index, 0);
                }
                Primary::NumberLiteral(num) => {
                    let index = self
                        .chunk_mut()
                        .push_constant(Value::Number(num.to_number()));
                    self.chunk_mut().push_code(OpCode::Constant as _, 0);
                    self.chunk_mut().push_code(index, 0);
                }
            },
        }
    }

    fn gen_block_stmt(&mut self, vm: &mut Vm, stmt: BlockStmt) {
        self.begin_block();
        for decl in stmt.decls() {
            self.gen_decl(vm, decl);
        }
        self.end_block();
    }

    fn gen_stmt(&mut self, vm: &mut Vm, stmt: Stmt) {
        match stmt {
            Stmt::ExprStmt(stmt) => {
                self.gen_expr(vm, stmt.expr().unwrap());
                self.chunk_mut().push_code(OpCode::Pop as _, 0);
            }
            Stmt::PrintStmt(stmt) => {
                self.gen_expr(vm, stmt.expr().unwrap());
                self.chunk_mut().push_code(OpCode::Print as _, 0);
            }
            Stmt::BlockStmt(stmt) => self.gen_block_stmt(vm, stmt),
            Stmt::IfStmt(stmt) => {
                let cond = stmt.cond().unwrap();
                let mut branches = stmt.branches();
                let then_branch = branches.next().unwrap();
                let else_branch = branches.next();

                self.gen_expr(vm, cond);

                self.chunk_mut().push_code(OpCode::JumpIfFalse as _, 0);
                let else_jump = self.chunk_mut().allocate_jump_location(0);

                self.chunk_mut().push_code(OpCode::Pop as _, 0);
                self.gen_stmt(vm, then_branch);

                if let Some(else_branch) = else_branch {
                    self.chunk_mut().push_code(OpCode::Jump as _, 0);
                    let end_jump = self.chunk_mut().allocate_jump_location(0);

                    self.chunk_mut().fill_jump_location_with_current(else_jump);
                    self.chunk_mut().push_code(OpCode::Pop as _, 0);
                    self.gen_stmt(vm, else_branch);

                    self.chunk_mut().fill_jump_location_with_current(end_jump);
                } else {
                    self.chunk_mut().fill_jump_location_with_current(else_jump);
                    self.chunk_mut().push_code(OpCode::Pop as _, 0);
                }
            }
            Stmt::WhileStmt(stmt) => {
                let cond = stmt.cond().unwrap();
                let body = stmt.body().unwrap();

                let start_loop_ip = self.chunk_mut().code().len();

                self.gen_expr(vm, cond);

                self.chunk_mut().push_code(OpCode::JumpIfFalse as _, 0);
                let end_jump = self.chunk_mut().allocate_jump_location(0);

                self.chunk_mut().push_code(OpCode::Pop as _, 0);
                self.gen_stmt(vm, body);

                self.chunk_mut().push_code(OpCode::Jump as _, 0);
                let start_jump = self.chunk_mut().allocate_jump_location(0);
                self.chunk_mut()
                    .fill_jump_location(start_jump, start_loop_ip);

                self.chunk_mut().fill_jump_location_with_current(end_jump);
                self.chunk_mut().push_code(OpCode::Pop as _, 0);
            }
            Stmt::ForStmt(stmt) => {
                self.begin_block();

                match stmt.init() {
                    Some(ForInit::Expr(expr)) => {
                        self.gen_expr(vm, expr);
                        self.chunk_mut().push_code(OpCode::Pop as _, 0);
                    }
                    Some(ForInit::VarDecl(decl)) => self.gen_var_decl(vm, decl),
                    _ => {}
                }

                let start_loop_ip = self.chunk_mut().code().len();

                let end_jump = stmt.cond().map(|cond| {
                    self.gen_expr(vm, cond.expr().unwrap());
                    self.chunk_mut().push_code(OpCode::JumpIfFalse as _, 0);
                    let end_jump = self.chunk_mut().allocate_jump_location(0);
                    self.chunk_mut().push_code(OpCode::Pop as _, 0);
                    end_jump
                });

                self.gen_stmt(vm, stmt.body().unwrap());

                if let Some(incr) = stmt.incr() {
                    self.gen_expr(vm, incr.expr().unwrap());
                    self.chunk_mut().push_code(OpCode::Pop as _, 0);
                }

                self.chunk_mut().push_code(OpCode::Jump as _, 0);
                let start_jump = self.chunk_mut().allocate_jump_location(0);
                self.chunk_mut()
                    .fill_jump_location(start_jump, start_loop_ip);

                if let Some(end_jump) = end_jump {
                    self.chunk_mut().fill_jump_location_with_current(end_jump);
                }
                self.chunk_mut().push_code(OpCode::Pop as _, 0);

                self.end_block();
            }
        }
    }

    fn define_variable<F>(&mut self, vm: &mut Vm, ident: Identifier, gen_value: F)
    where
        F: FnOnce(&mut Self, &mut Vm),
    {
        eprintln!(
            "ident = {}, block_depth = {}",
            ident.to_str(),
            self.block_depth
        );
        if self.block_depth > GLOBAL_BLOCK {
            let idx = self.locals.len();
            self.push_local(ident.to_str());
            gen_value(self, vm);
            assert_eq!(idx + 1, self.locals.len());
            self.locals[idx].depth = self.block_depth;
        } else {
            let ident = vm.allocate_string(ident.to_str().into());
            let index = self
                .chunk_mut()
                .push_constant(Value::Object(ident.into_raw_obj()));
            gen_value(self, vm);
            self.chunk_mut().push_code(OpCode::DefineGlobal as _, 0);
            self.chunk_mut().push_code(index, 0);
        }
    }

    fn gen_var_decl(&mut self, vm: &mut Vm, decl: VarDecl) {
        let ident = decl.ident().unwrap();
        let expr = decl.expr();
        self.define_variable(vm, ident, move |this, vm| match expr {
            Some(expr) => this.gen_expr(vm, expr),
            None => this.chunk_mut().push_code(OpCode::Nil as _, 0),
        });
    }

    pub fn gen_decl(&mut self, vm: &mut Vm, decl: Decl) {
        match decl {
            Decl::VarDecl(decl) => self.gen_var_decl(vm, decl),
            Decl::FunDecl(decl) => {
                let name = decl.ident().unwrap();
                let name = vm.allocate_string(name.to_str().into());
                let arity = decl.params().count() as u32;
                let mut compiler = Compiler::new_function(self, name, arity);
                compiler.begin_block();
                for param in decl.params() {
                    compiler.push_local(param.to_str());
                    compiler.locals.last_mut().unwrap().depth = compiler.block_depth;
                }
                compiler.gen_block_stmt(vm, decl.body().unwrap());
                compiler.end_block();
                let function = compiler.finish();
                let fun_obj = vm.allocate_function(function);

                self.define_variable(vm, decl.ident().unwrap(), move |this, _vm| {
                    let index = this.chunk_mut().push_constant(Value::Object(fun_obj));
                    this.chunk_mut().push_code(OpCode::Constant as _, 0);
                    this.chunk_mut().push_code(index, 0);
                });
            }
            Decl::Stmt(stmt) => self.gen_stmt(vm, stmt),
        }
    }
}

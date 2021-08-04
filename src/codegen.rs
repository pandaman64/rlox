use crate::{
    ast::{BinOpKind, Decl, Expr, Identifier, Primary, Stmt, UnaryOpKind},
    opcode::{Chunk, OpCode},
    value::Value,
    vm::Vm,
};

const GLOBAL_BLOCK: usize = 0;

struct Local {
    ident: String,
    depth: usize,
}

pub struct Compiler {
    locals: Vec<Local>,
    block_depth: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            locals: vec![],
            block_depth: 0,
        }
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

    fn end_block(&mut self, chunk: &mut Chunk) {
        while let Some(local) = self.locals.last() {
            if local.depth != self.block_depth {
                break;
            }
            self.locals.pop();
            chunk.push_code(OpCode::Pop as _, 0);
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

    fn gen_place(&mut self, vm: &mut Vm, chunk: &mut Chunk, expr: Expr) -> (bool, Identifier) {
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
                self.gen_expr(vm, chunk, target);
                (true, ident)
            }
            // TODO: emit error message
            _ => todo!("place expression must be an identifier or dot expression"),
        }
    }

    fn gen_expr(&mut self, vm: &mut Vm, chunk: &mut Chunk, expr: Expr) {
        match expr {
            Expr::UnaryOp(expr) => {
                let operand = Expr::cast(expr.operand().unwrap()).unwrap();
                self.gen_expr(vm, chunk, operand);
                let opcode = match expr.kind() {
                    UnaryOpKind::Negation => OpCode::Negate,
                    UnaryOpKind::Not => OpCode::Not,
                };
                chunk.push_code(opcode as _, 0);
            }
            Expr::BinOp(expr) => {
                let opcodes: &[OpCode] = match expr.kind() {
                    BinOpKind::Assignment => {
                        let mut operands = expr.operands();
                        let lhs = operands.next().unwrap();
                        let rhs = operands.next().unwrap();
                        let (has_target, ident) = self.gen_place(vm, chunk, lhs);
                        if has_target {
                            todo!()
                        } else {
                            match self.resolve_local(ident.to_str()) {
                                Some(i) => {
                                    self.gen_expr(vm, chunk, rhs);
                                    chunk.push_code(OpCode::SetGlobal as _, 0);
                                    chunk.push_code(i as _, 0);
                                }
                                None => {
                                    let ident = vm.allocate_string(ident.to_str().into());
                                    let index =
                                        chunk.push_constant(Value::Object(ident.into_raw_obj()));

                                    self.gen_expr(vm, chunk, rhs);
                                    chunk.push_code(OpCode::SetGlobal as _, 0);
                                    chunk.push_code(index, 0);
                                }
                            }
                        }
                        return;
                    }
                    BinOpKind::Or => todo!(),
                    BinOpKind::And => todo!(),
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
                    self.gen_expr(vm, chunk, operand);
                }
                for opcode in opcodes.iter() {
                    chunk.push_code(*opcode as _, 0);
                }
            }
            Expr::Primary(expr) => match expr {
                Primary::Identifier(ident) => match self.resolve_local(ident.to_str()) {
                    Some(i) => {
                        chunk.push_code(OpCode::GetLocal as _, 0);
                        chunk.push_code(i as u8, 0);
                    }
                    None => {
                        let ident = vm.allocate_string(ident.to_str().into());
                        let index = chunk.push_constant(Value::Object(ident.into_raw_obj()));

                        chunk.push_code(OpCode::GetGlobal as _, 0);
                        chunk.push_code(index, 0);
                    }
                },
                Primary::NilLiteral(_) => {
                    chunk.push_code(OpCode::Nil as _, 0);
                }
                Primary::BooleanLiteral(b) => {
                    if b.to_boolean() {
                        chunk.push_code(OpCode::True as _, 0);
                    } else {
                        chunk.push_code(OpCode::False as _, 0);
                    }
                }
                Primary::StringLiteral(s) => {
                    let obj = vm.allocate_string(s.to_str().into());
                    let index = chunk.push_constant(Value::Object(obj.into_raw_obj()));
                    chunk.push_code(OpCode::Constant as _, 0);
                    chunk.push_code(index, 0);
                }
                Primary::NumberLiteral(num) => {
                    let index = chunk.push_constant(Value::Number(num.to_number()));
                    chunk.push_code(OpCode::Constant as _, 0);
                    chunk.push_code(index, 0);
                }
            },
        }
    }

    fn gen_stmt(&mut self, vm: &mut Vm, chunk: &mut Chunk, stmt: Stmt) {
        match stmt {
            Stmt::ExprStmt(stmt) => {
                self.gen_expr(vm, chunk, stmt.expr().unwrap());
                chunk.push_code(OpCode::Pop as _, 0);
            }
            Stmt::PrintStmt(stmt) => {
                self.gen_expr(vm, chunk, stmt.expr().unwrap());
                chunk.push_code(OpCode::Print as _, 0);
            }
            Stmt::BlockStmt(stmt) => {
                self.begin_block();
                for decl in stmt.decls() {
                    self.gen_decl(vm, chunk, decl);
                }
                self.end_block(chunk);
            }
            Stmt::IfStmt(stmt) => {
                let cond = stmt.cond().unwrap();
                let mut branches = stmt.branches();
                let then_branch = branches.next().unwrap();
                let else_branch = branches.next();

                self.gen_expr(vm, chunk, cond);

                chunk.push_code(OpCode::JumpIfFalse as _, 0);
                let else_jump = chunk.allocate_jump_location(0);
                chunk.push_code(OpCode::Pop as _, 0);

                self.gen_stmt(vm, chunk, then_branch);

                chunk.fill_jump_location(else_jump);
                if let Some(else_branch) = else_branch {
                    // chunk.fill_jump_location(else_jump);
                    // chunk.push_code(OpCode::Jump as _, 0);
                    // after_if.push(chunk.allocate_jump_location());
                    // self.gen_stmt(vm, chunk, else_stmt);
                    todo!()
                } else {
                    chunk.push_code(OpCode::Pop as _, 0);
                }
            }
        }
    }

    pub fn gen_decl(&mut self, vm: &mut Vm, chunk: &mut Chunk, decl: Decl) {
        match decl {
            Decl::VarDecl(decl) => {
                if self.block_depth > GLOBAL_BLOCK {
                    let ident = decl.ident().unwrap();
                    let ident = ident.to_str();
                    self.push_local(ident);

                    match decl.expr() {
                        Some(expr) => self.gen_expr(vm, chunk, expr),
                        None => chunk.push_code(OpCode::Nil as _, 0),
                    }

                    self.locals.last_mut().unwrap().depth = self.block_depth;
                } else {
                    let ident = vm.allocate_string(decl.ident().unwrap().to_str().into());
                    let index = chunk.push_constant(Value::Object(ident.into_raw_obj()));

                    match decl.expr() {
                        Some(expr) => self.gen_expr(vm, chunk, expr),
                        None => chunk.push_code(OpCode::Nil as _, 0),
                    }
                    chunk.push_code(OpCode::DefineGlobal as _, 0);
                    chunk.push_code(index, 0);
                }
            }
            Decl::Stmt(stmt) => self.gen_stmt(vm, chunk, stmt),
        }
    }
}

use std::{
    cell::{Cell, RefCell},
    convert::TryFrom,
    rc::Rc,
};

use crate::{
    ast::{
        BinOpKind, BlockStmt, Decl, Expr, ForInit, FunDecl, Identifier, Primary, Stmt, UnaryOpKind,
        VarDecl,
    },
    line_map::LineMap,
    opcode::{ChunkBuilder, OpCode},
    table::InternedStr,
    trace_available,
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
    UnassignedLocal { ident: String, position: usize },
    LoopTooLarge { position: usize },
    ReturnFromTopLevel { position: usize },
    TooManyLocalVariables { position: usize },
    TooManyConstants { position: usize },
    TooManyParameters { position: usize },
    TooManyArguments { position: usize },
    TooManyUpvalues { position: usize },
    ThisOutsideClass { position: usize },
    ReturnFromInit { position: usize },
    InheritFromItself { position: usize },
    SuperOutsideClass { position: usize },
    SuperWithoutSuperClass { position: usize },
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

pub enum FunctionKind {
    Function { name: InternedStr, arity: u8 },
    Method { name: InternedStr, arity: u8 },
    Initializer { arity: u8 },
    Script,
}

impl FunctionKind {
    fn mark(&self, worklist: &mut Vec<RawObject>) {
        match self {
            FunctionKind::Function { name, .. } | FunctionKind::Method { name, .. } => unsafe {
                object::mark(name.into_raw_obj(), worklist);
            },
            _ => {}
        }
    }
}

struct ClassCompiler {
    enclosing: Option<Rc<Self>>,
    has_super_class: Cell<bool>,
}

pub struct Compiler<'parent, 'map> {
    parent: Option<&'parent Compiler<'parent, 'map>>,
    current_class: Option<Rc<ClassCompiler>>,
    chunk_builder: ChunkBuilder,
    kind: FunctionKind,
    locals: Vec<Local>,
    // since we do not close values for these upvalues, we do not trace them for GC
    upvalues: RefCell<Vec<Upvalue>>,
    block_depth: usize,
    line_map: &'map LineMap,
    return_position: Option<usize>,
    errors: RefCell<Vec<CodegenError>>,
}

fn new_locals(kind: &FunctionKind) -> Vec<Local> {
    let ident = match kind {
        FunctionKind::Method { .. } | FunctionKind::Initializer { .. } => "this".into(),
        FunctionKind::Function { .. } | FunctionKind::Script => "<callee>".into(),
    };
    // This corresponds to the callee, and local variables starts with index 1
    vec![Local {
        ident,
        depth: 0,
        captured: Cell::new(false),
    }]
}

impl<'parent, 'map> Compiler<'parent, 'map> {
    pub fn new_script(line_map: &'map LineMap) -> Self {
        let kind = FunctionKind::Script;
        let locals = new_locals(&kind);
        Self {
            parent: None,
            current_class: None,
            chunk_builder: ChunkBuilder::new(),
            kind,
            locals,
            upvalues: RefCell::new(vec![]),
            block_depth: 0,
            line_map,
            return_position: None,
            errors: RefCell::new(vec![]),
        }
    }

    fn new_function(
        parent: &'parent Compiler<'parent, 'map>,
        current_class: Option<Rc<ClassCompiler>>,
        kind: FunctionKind,
        line_map: &'map LineMap,
        return_position: usize,
    ) -> Self {
        let locals = new_locals(&kind);
        Self {
            parent: Some(parent),
            current_class,
            chunk_builder: ChunkBuilder::new(),
            kind,
            locals,
            upvalues: RefCell::new(vec![]),
            block_depth: 0,
            line_map,
            return_position: Some(return_position),
            errors: RefCell::new(vec![]),
        }
    }

    fn mark(&self) -> impl FnOnce(&mut Vec<RawObject>) + '_ {
        move |worklist| {
            self.kind.mark(worklist);
            // SAFETY: self owns the chunk and must not be deallocated by vm
            unsafe {
                for constant in self.chunk_builder.constants().iter() {
                    constant.mark(worklist);
                }
            }
            if let Some(parent) = self.parent {
                parent.mark()(worklist);
            }
        }
    }

    fn chunk(&self) -> &ChunkBuilder {
        &self.chunk_builder
    }

    fn chunk_mut(&mut self) -> &mut ChunkBuilder {
        &mut self.chunk_builder
    }

    /// - Returns `Some((function, upvalues, error))` if the code contains at least one declaration.
    /// - Returns `None` if the code does not contain any declaration.
    pub fn finish(
        mut self,
        vm: &mut Vm<'_>,
    ) -> Option<(RawFunction, Vec<Upvalue>, Vec<CodegenError>)> {
        let return_position = self.return_position?;
        self.gen_return(return_position);

        let upvalues = u8::try_from(self.upvalues.get_mut().len()).unwrap_or(255);
        let function = match self.kind {
            FunctionKind::Script => vm.allocate_script(self.mark()),
            FunctionKind::Function { name, arity } | FunctionKind::Method { name, arity } => {
                vm.allocate_function(name, arity, upvalues, self.mark())
            }
            FunctionKind::Initializer { arity } => {
                vm.allocate_function(vm.init_str(), arity, upvalues, self.mark())
            }
        };
        // set chunk after allocating function so that constants in the chunk will be marked by self.mark()
        unsafe {
            vm.set_chunk(function, self.chunk_builder.take());
        }

        if trace_available() {
            // SAFETY: we constructed a valid function
            unsafe {
                function.as_ref().trace();
            }
        }

        Some((
            function,
            self.upvalues.into_inner(),
            self.errors.into_inner(),
        ))
    }

    fn push_synthetic_local(&mut self, name: &str, position: usize) {
        for local in self.locals.iter().rev() {
            if local.depth != self.block_depth {
                break;
            }

            if local.ident == name {
                self.errors
                    .get_mut()
                    .push(CodegenError::ShadowingInSameScope {
                        ident: name.into(),
                        position,
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
                .push(CodegenError::TooManyLocalVariables { position });
        }
    }

    fn push_local(&mut self, ident: Identifier) {
        self.push_synthetic_local(ident.to_str(), ident.start());
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

    fn resolve_local(&self, ident: &str, position: usize) -> Option<usize> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| {
                if local.ident == ident {
                    if local.depth == UNASSIGNED {
                        self.errors
                            .borrow_mut()
                            .push(CodegenError::UnassignedLocal {
                                ident: ident.into(),
                                position,
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

    fn resolve_upvalue(&self, ident: &str, position: usize) -> Option<usize> {
        let parent = self.parent?;
        match parent.resolve_local(ident, position) {
            Some(local) => {
                parent.locals[local].captured.set(true);
                Some(self.add_upvalue(u8::try_from(local).unwrap(), true, position))
            }
            None => {
                let index = parent.resolve_upvalue(ident, position)?;
                Some(self.add_upvalue(u8::try_from(index).unwrap(), false, position))
            }
        }
    }

    fn resolve(&mut self, vm: &mut Vm<'_>, ident: &str, position: usize) -> (OpCode, OpCode, u8) {
        if let Some(i) = self.resolve_local(ident, position) {
            (OpCode::GetLocal, OpCode::SetLocal, u8::try_from(i).unwrap())
        } else if let Some(i) = self.resolve_upvalue(ident, position) {
            (
                OpCode::GetUpvalue,
                OpCode::SetUpvalue,
                u8::try_from(i).unwrap(),
            )
        } else {
            let ident = vm.allocate_string(ident.into(), self.mark());
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
        match self.kind {
            FunctionKind::Initializer { .. } => {
                self.push_opcode(OpCode::GetLocal, position);
                self.push_u8(0, position);
            }
            _ => self.push_opcode(OpCode::Nil, position),
        }
        self.push_opcode(OpCode::Return, position);
    }

    fn gen_place(&mut self, vm: &mut Vm<'_>, expr: Expr) -> (bool, Identifier) {
        // the left hand side of assignments is guaranteed to be a valid place by the parser
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
                            self.gen_expr(vm, rhs);

                            let ident = vm.allocate_string(ident.to_str().into(), self.mark());
                            let index = self
                                .push_constant(Value::Object(ident.into_raw_obj()), expr.start());
                            self.push_opcode(OpCode::SetProperty, expr.start());
                            self.push_u8(index, expr.start());
                        } else {
                            let (_get_op, set_op, index) =
                                self.resolve(vm, ident.to_str(), ident.start());
                            self.gen_expr(vm, rhs);
                            self.push_opcode(set_op, expr.start());
                            self.push_u8(index, expr.start());
                        }
                        return;
                    }
                    BinOpKind::Dot => {
                        let mut operands = expr.operands();
                        let lhs = operands.next().unwrap();
                        let rhs = operands.next().unwrap();

                        let ident = match rhs {
                            Expr::Primary(Primary::Identifier(ident)) => ident,
                            // need to introduce opcode for dynamically selecting a field from an instance
                            _ => todo!("expect identifier after dot"),
                        };
                        self.gen_expr(vm, lhs);
                        let ident = vm.allocate_string(ident.to_str().into(), self.mark());
                        let index =
                            self.push_constant(Value::Object(ident.into_raw_obj()), expr.start());
                        self.push_opcode(OpCode::GetProperty, expr.start());
                        self.push_u8(index, expr.start());
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
                    let (get_op, _set_op, index) = self.resolve(vm, ident.to_str(), position);
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
                Primary::This(this) => {
                    if self.current_class.is_none() {
                        self.errors.get_mut().push(CodegenError::ThisOutsideClass {
                            position: this.start(),
                        });
                        return;
                    }
                    let (get_op, _set_op, index) = self.resolve(vm, "this", this.start());
                    self.push_opcode(get_op, this.start());
                    self.push_u8(index, this.start());
                }
            },
            Expr::Call(expr) => {
                macro_rules! gen_args {
                    () => {{
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
                        u8::try_from(args).unwrap()
                    }};
                }
                let callee = expr.function().unwrap();
                match callee {
                    Expr::BinOp(binop) if binop.kind() == BinOpKind::Dot => {
                        let mut operands = binop.operands();
                        let receiver = operands.next().unwrap();
                        let method = operands.next().unwrap();
                        let ident = match method {
                            Expr::Primary(Primary::Identifier(ident)) => ident,
                            _ => unreachable!("non-identifier expression is used as method"),
                        };

                        self.gen_expr(vm, receiver);
                        let args = gen_args!();
                        let name = vm.allocate_string(ident.to_str().into(), self.mark());
                        let index =
                            self.push_constant(Value::Object(name.into_raw_obj()), ident.start());
                        self.push_opcode(OpCode::Invoke, expr.start());
                        self.push_u8(index, expr.start());
                        self.push_u8(args, expr.start());
                    }
                    callee => {
                        self.gen_expr(vm, callee);
                        let args = gen_args!();
                        self.push_opcode(OpCode::Call, expr.start());
                        self.push_u8(args, expr.start());
                    }
                }
            }
            Expr::SuperMethod(expr) => {
                match &self.current_class {
                    None => {
                        self.errors.get_mut().push(CodegenError::SuperOutsideClass {
                            position: expr.start(),
                        });
                        return;
                    }
                    Some(current_class) => {
                        if !current_class.has_super_class.get() {
                            self.errors
                                .get_mut()
                                .push(CodegenError::SuperWithoutSuperClass {
                                    position: expr.start(),
                                });
                            return;
                        }
                    }
                }
                {
                    let (get_op, _set_op, index) = self.resolve(vm, "this", expr.start());
                    self.push_opcode(get_op, expr.start());
                    self.push_u8(index, expr.start());
                }
                {
                    let (get_op, _set_op, index) = self.resolve(vm, "super", expr.start());
                    self.push_opcode(get_op, expr.start());
                    self.push_u8(index, expr.start());
                }
                let method_name = expr.method_name().unwrap();
                let name = vm.allocate_string(method_name.to_str().into(), self.mark());
                let index =
                    self.push_constant(Value::Object(name.into_raw_obj()), method_name.start());
                self.push_opcode(OpCode::GetSuper, expr.start());
                self.push_u8(index, expr.start());
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
                FunctionKind::Function { .. } | FunctionKind::Method { .. } => {
                    match stmt.expr() {
                        Some(expr) => self.gen_expr(vm, expr),
                        None => self.push_opcode(OpCode::Nil, stmt.start()),
                    }
                    self.push_opcode(OpCode::Return, stmt.start());
                }
                FunctionKind::Initializer { .. } => {
                    if stmt.expr().is_some() {
                        self.errors.get_mut().push(CodegenError::ReturnFromInit {
                            position: stmt.start(),
                        });
                    }
                    self.push_opcode(OpCode::GetLocal, stmt.start());
                    self.push_u8(0, stmt.start());
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
            // moreover, it is possible that the locals slot overflows and this local is not inserted
            if allow_reference_in_value && idx < self.locals.len() {
                self.locals[idx].depth = self.block_depth;
            }
            gen_value(self, vm);
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

    fn gen_closure(
        decl: FunDecl,
        current_class: Option<Rc<ClassCompiler>>,
        kind: FunctionKind,
    ) -> impl FnOnce(&mut Compiler<'parent, 'map>, &mut Vm<'_>) -> usize {
        move |this, vm| {
            let mut compiler = Compiler::new_function(
                this,
                current_class,
                kind,
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
            let (fun_obj, upvalues, errors) = compiler.finish(vm).unwrap();
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
        }
    }

    pub fn gen_decl(&mut self, vm: &mut Vm<'_>, decl: Decl) {
        self.return_position = Some(decl.return_position());

        match decl {
            Decl::VarDecl(decl) => self.gen_var_decl(vm, decl),
            Decl::FunDecl(decl) => {
                let ident = decl.ident().unwrap();
                // TODO: if the declaration is global, we have duplicate allocation for the name
                let name = vm.allocate_string(ident.to_str().into(), self.mark());
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
                    ident,
                    Self::gen_closure(
                        decl,
                        self.current_class.clone(),
                        FunctionKind::Function { name, arity },
                    ),
                    // functions can refer to themselves inside their body because they are executed
                    // only after the initialization.
                    true,
                );

                // remove name from GC root
                vm.pop();
            }
            Decl::ClassDecl(decl) => {
                self.current_class = Some(Rc::new(ClassCompiler {
                    enclosing: self.current_class.take(),
                    has_super_class: Cell::new(false),
                }));

                let ident = decl.ident().unwrap();
                // TODO: if the declaration is global, we have duplicate allocation for the name
                let name = vm.allocate_string(ident.to_str().into(), self.mark());

                // register name as GC root
                vm.push(Value::Object(name.into_raw_obj()));

                self.define_variable(
                    vm,
                    ident.clone(),
                    |this, _vm| {
                        let index =
                            this.push_constant(Value::Object(name.into_raw_obj()), decl.start());
                        this.push_opcode(OpCode::Class, decl.start());
                        this.push_u8(index, decl.start());

                        decl.start()
                    },
                    true,
                );

                vm.pop();

                let (get_op, _set_op, index) = self.resolve(vm, ident.to_str(), ident.start());

                if let Some(super_class) = decl.super_class() {
                    if ident.to_str() == super_class.to_str() {
                        self.errors.get_mut().push(CodegenError::InheritFromItself {
                            position: super_class.start(),
                        });
                    }

                    self.begin_block();
                    self.push_synthetic_local("super", super_class.start());
                    self.locals.last_mut().unwrap().depth = self.block_depth;

                    let super_position = super_class.start();
                    let (super_get_op, _super_set_op, super_index) =
                        self.resolve(vm, super_class.to_str(), super_class.start());
                    self.push_opcode(super_get_op, super_position);
                    self.push_u8(super_index, super_position);
                    self.push_opcode(get_op, super_position);
                    self.push_u8(index, super_position);
                    self.push_opcode(OpCode::Inherit, super_position);

                    self.current_class
                        .as_mut()
                        .unwrap()
                        .has_super_class
                        .set(true);
                }

                self.push_opcode(get_op, decl.start());
                self.push_u8(index, decl.start());
                for method in decl.methods() {
                    let ident = method.ident().unwrap();
                    let method_name = vm.allocate_string(ident.to_str().into(), self.mark());
                    let arity = match u8::try_from(method.params().count()) {
                        Ok(arity) => arity,
                        Err(_) => {
                            let ident = method.params().nth(255).unwrap();
                            self.errors.get_mut().push(CodegenError::TooManyParameters {
                                position: ident.start(),
                            });
                            continue;
                        }
                    };
                    let kind = if method_name == vm.init_str() {
                        FunctionKind::Initializer { arity }
                    } else {
                        FunctionKind::Method {
                            name: method_name,
                            arity,
                        }
                    };
                    let method_position = method.start();

                    // register method_name as GC root
                    vm.push(Value::Object(method_name.into_raw_obj()));

                    Self::gen_closure(method, self.current_class.clone(), kind)(self, vm);
                    let index = self
                        .push_constant(Value::Object(method_name.into_raw_obj()), method_position);
                    self.push_opcode(OpCode::Method, method_position);
                    self.push_u8(index, method_position);

                    vm.pop();
                }
                self.push_opcode(OpCode::Pop, decl.return_position());

                if self.current_class.as_ref().unwrap().has_super_class.get() {
                    self.end_block(decl.start());
                }

                self.current_class = self.current_class.take().unwrap().enclosing.clone();
            }
            Decl::Stmt(stmt) => self.gen_stmt(vm, stmt),
        }
    }
}

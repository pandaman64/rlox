use crate::{
    ast::{BinOpKind, Declaration, Expr, Primary, Stmt, UnaryOpKind},
    value::Value,
    vm::Vm,
    Chunk, OpCode,
};

pub fn gen_expr(vm: &mut Vm, chunk: &mut Chunk, expr: Expr) {
    match expr {
        Expr::UnaryOp(expr) => {
            let operand = Expr::cast(expr.operand().unwrap()).unwrap();
            gen_expr(vm, chunk, operand);
            let opcode = match expr.kind() {
                UnaryOpKind::Negation => OpCode::Negate,
                UnaryOpKind::Not => OpCode::Not,
            };
            chunk.push_code(opcode as _, 0);
        }
        Expr::BinOp(expr) => {
            for operand in expr.operands() {
                let operand = Expr::cast(operand).unwrap();
                gen_expr(vm, chunk, operand);
            }
            let opcodes: &[OpCode] = match expr.kind() {
                BinOpKind::Assignment => todo!(),
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
            for opcode in opcodes.iter() {
                chunk.push_code(*opcode as _, 0);
            }
        }
        Expr::Primary(expr) => match expr {
            Primary::Identifier(_ident) => todo!(),
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
                let obj = vm.allocate_string(s.to_str());
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

pub fn gen_stmt(vm: &mut Vm, chunk: &mut Chunk, stmt: Stmt) {
    match stmt {
        Stmt::ExprStmt(stmt) => {
            gen_expr(vm, chunk, stmt.expr().unwrap());
            // TODO: pop value
        }
        Stmt::PrintStmt(stmt) => {
            gen_expr(vm, chunk, stmt.expr().unwrap());
            chunk.push_code(OpCode::Print as _, 0);
        }
    }
}

pub fn gen_decl(vm: &mut Vm, chunk: &mut Chunk, decl: Declaration) {
    match decl {
        Declaration::VarDecl(_) => todo!(),
        Declaration::Stmt(stmt) => gen_stmt(vm, chunk, stmt),
    }
}

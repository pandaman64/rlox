use crate::{
    ast::{BinOpKind, Expr, Primary, UnaryOpKind},
    Chunk, OpCode,
};

pub fn gen_expr(chunk: &mut Chunk, expr: Expr) {
    match expr {
        Expr::UnaryOp(expr) => {
            let operand = Expr::cast(expr.operand().unwrap()).unwrap();
            gen_expr(chunk, operand);
            let opcode = match expr.kind() {
                UnaryOpKind::Negation => OpCode::Negate,
                UnaryOpKind::Not => OpCode::Not,
            };
            chunk.push_code(opcode as _, 0);
        }
        Expr::BinOp(expr) => {
            for operand in expr.operands() {
                let operand = Expr::cast(operand).unwrap();
                gen_expr(chunk, operand);
            }
            let opcode = match expr.kind() {
                BinOpKind::Assignment => todo!(),
                BinOpKind::Or => todo!(),
                BinOpKind::And => todo!(),
                BinOpKind::Equal => todo!(),
                BinOpKind::NotEqual => todo!(),
                BinOpKind::Less => todo!(),
                BinOpKind::Greater => todo!(),
                BinOpKind::LessEqual => todo!(),
                BinOpKind::GreaterEqual => todo!(),
                BinOpKind::Add => OpCode::Add,
                BinOpKind::Subtract => OpCode::Subtract,
                BinOpKind::Multiply => OpCode::Multiply,
                BinOpKind::Divide => OpCode::Divide,
                BinOpKind::Dot => todo!(),
            };
            chunk.push_code(opcode as _, 0);
        }
        Expr::Primary(expr) => match expr {
            Primary::Identifier(_ident) => todo!(),
            Primary::StringLiteral(_s) => todo!(),
            Primary::NumberLiteral(num) => {
                let index = chunk.push_constant(num.to_number());
                chunk.push_code(OpCode::Constant as _, 0);
                chunk.push_code(index, 0);
            }
        },
    }
}

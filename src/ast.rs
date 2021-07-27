use rowan::NodeOrToken;

use crate::syntax::{SyntaxKind, SyntaxNode, SyntaxToken};

fn first_nontirivial_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.children_with_tokens()
        .filter_map(|child| match child {
            NodeOrToken::Token(token) if !token.kind().is_trivial() => Some(token),
            _ => None,
        })
        .next()
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOpKind {
    Negation,
    Not,
}

#[derive(Debug)]
pub struct UnaryOp {
    inner: SyntaxNode,
}

impl UnaryOp {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::UnaryOpNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    fn first_token(&self) -> Option<SyntaxToken> {
        first_nontirivial_token(&self.inner)
    }

    pub fn kind(&self) -> UnaryOpKind {
        match self.first_token().unwrap().kind() {
            SyntaxKind::MinusToken => UnaryOpKind::Negation,
            SyntaxKind::BangToken => UnaryOpKind::Not,
            _ => unreachable!(),
        }
    }

    pub fn operand(&self) -> Option<SyntaxNode> {
        self.inner.first_child()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOpKind {
    Assignment,
    Or,
    And,
    Equal,
    NotEqual,
    Less,
    Greater,
    LessEqual,
    GreaterEqual,
    Add,
    Subtract,
    Multiply,
    Divide,
    Dot,
}

#[derive(Debug)]
pub struct BinOp {
    inner: SyntaxNode,
}

impl BinOp {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::BinOpNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    fn first_token(&self) -> Option<SyntaxToken> {
        first_nontirivial_token(&self.inner)
    }

    pub fn kind(&self) -> BinOpKind {
        use BinOpKind::*;
        use SyntaxKind::*;

        match self.first_token().unwrap().kind() {
            EqualToken => Assignment,
            OrToken => Or,
            AndToken => And,
            EqualEqualToken => Equal,
            BangEqualToken => NotEqual,
            LessToken => Less,
            GreaterToken => Greater,
            LessEqualToken => LessEqual,
            GreaterEqualToken => GreaterEqual,
            PlusToken => Add,
            MinusToken => Subtract,
            StarToken => Multiply,
            SlashToken => Divide,
            DotToken => Dot,
            _ => unreachable!(),
        }
    }

    pub fn operands(&self) -> impl Iterator<Item = SyntaxNode> {
        self.inner.children()
    }
}

#[derive(Debug)]
pub struct Identifier {
    inner: SyntaxToken,
}

impl Identifier {
    pub fn cast(inner: SyntaxToken) -> Option<Self> {
        if inner.kind() == SyntaxKind::IdentifierToken {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn to_str(&self) -> &str {
        self.inner.text()
    }
}

#[derive(Debug)]
pub struct StringLiteral {
    inner: SyntaxToken,
}

impl StringLiteral {
    pub fn cast(inner: SyntaxToken) -> Option<Self> {
        if inner.kind() == SyntaxKind::StringLiteralToken {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn to_str(&self) -> &str {
        // this text represents the whole literal including "
        let text = self.inner.text();
        &text[1..(text.len() - 1)]
    }
}

#[derive(Debug)]
pub struct NumberLiteral {
    inner: SyntaxToken,
}

impl NumberLiteral {
    pub fn cast(inner: SyntaxToken) -> Option<Self> {
        if inner.kind() == SyntaxKind::NumberToken {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn to_number(&self) -> f64 {
        self.inner.text().parse().unwrap()
    }
}

#[derive(Debug)]
pub struct NilLiteral {
    inner: SyntaxToken,
}

impl NilLiteral {
    pub fn cast(inner: SyntaxToken) -> Option<Self> {
        if inner.kind() == SyntaxKind::NilToken {
            Some(Self { inner })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct BooleanLiteral {
    inner: SyntaxToken,
}

impl BooleanLiteral {
    pub fn cast(inner: SyntaxToken) -> Option<Self> {
        if matches!(inner.kind(), SyntaxKind::TrueToken | SyntaxKind::FalseToken) {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn to_boolean(&self) -> bool {
        match self.inner.kind() {
            SyntaxKind::TrueToken => true,
            SyntaxKind::FalseToken => false,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub enum Primary {
    Identifier(Identifier),
    NilLiteral(NilLiteral),
    BooleanLiteral(BooleanLiteral),
    StringLiteral(StringLiteral),
    NumberLiteral(NumberLiteral),
}

impl Primary {
    fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        if inner.kind() == SyntaxKind::PrimaryExprNode {
            let child = first_nontirivial_token(&inner)?;
            Some(match child.kind() {
                IdentifierToken => Self::Identifier(Identifier::cast(child)?),
                NilToken => Self::NilLiteral(NilLiteral::cast(child)?),
                TrueToken | FalseToken => Self::BooleanLiteral(BooleanLiteral::cast(child)?),
                StringLiteralToken => Self::StringLiteral(StringLiteral::cast(child)?),
                NumberToken => Self::NumberLiteral(NumberLiteral::cast(child)?),
                _ => return None,
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    UnaryOp(UnaryOp),
    BinOp(BinOp),
    Primary(Primary),
}

impl Expr {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        Some(match inner.kind() {
            ExprNode => Self::cast(inner.first_child()?)?,
            UnaryOpNode => Self::UnaryOp(UnaryOp::cast(inner)?),
            BinOpNode => Self::BinOp(BinOp::cast(inner)?),
            PrimaryExprNode => Self::Primary(Primary::cast(inner)?),
            _ => return None,
        })
    }
}

#[derive(Debug)]
pub struct VarDecl {
    inner: SyntaxNode,
}

impl VarDecl {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::VarDeclNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn ident(&self) -> Option<Identifier> {
        self.inner
            .children_with_tokens()
            .filter_map(|child| match child {
                NodeOrToken::Token(token) => Identifier::cast(token),
                _ => None,
            })
            .next()
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().filter_map(Expr::cast).next()
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    inner: SyntaxNode,
}

impl ExprStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::ExprStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().filter_map(Expr::cast).next()
    }
}

#[derive(Debug)]
pub struct PrintStmt {
    inner: SyntaxNode,
}

impl PrintStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::PrintStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().filter_map(Expr::cast).next()
    }
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(ExprStmt),
    PrintStmt(PrintStmt),
}

impl Stmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        if inner.kind() == SyntaxKind::StmtNode {
            let child = inner.first_child()?;
            Some(match child.kind() {
                ExprStmtNode => Self::ExprStmt(ExprStmt::cast(child)?),
                PrintStmtNode => Self::PrintStmt(PrintStmt::cast(child)?),
                _ => return None,
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum Decl {
    VarDecl(VarDecl),
    Stmt(Stmt),
}

impl Decl {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        if inner.kind() == DeclNode {
            let child = inner.first_child()?;
            Some(match child.kind() {
                VarDeclNode => Self::VarDecl(VarDecl::cast(child)?),
                StmtNode => Self::Stmt(Stmt::cast(child)?),
                _ => return None,
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Root {
    inner: SyntaxNode,
}

impl Root {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::RootNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn decls(&self) -> impl Iterator<Item = Decl> + '_ {
        self.inner.children().filter_map(Decl::cast)
    }
}

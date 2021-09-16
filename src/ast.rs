use crate::syntax::{NodeOrToken, SyntaxKind, SyntaxNode, SyntaxToken};

fn first_nontirivial_token(node: &SyntaxNode) -> Option<SyntaxToken> {
    node.children_with_tokens().find_map(|child| match child {
        NodeOrToken::Token(token) if !token.kind().is_trivial() => Some(token),
        _ => None,
    })
}

fn try_as_ident(child: NodeOrToken) -> Option<Identifier> {
    match child {
        NodeOrToken::Token(token) => Identifier::cast(token),
        _ => None,
    }
}

#[derive(Debug)]
pub struct ParenExpr {
    inner: SyntaxNode,
}

impl ParenExpr {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::ParenExprNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
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

    pub fn operand(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
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

    pub fn operands(&self) -> impl Iterator<Item = Expr> {
        self.inner.children().filter_map(Expr::cast)
    }
}

#[derive(Debug, Clone)]
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
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
pub struct This {
    inner: SyntaxToken,
}

impl This {
    pub fn cast(inner: SyntaxToken) -> Option<Self> {
        if matches!(inner.kind(), SyntaxKind::ThisToken) {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
    }
}

#[derive(Debug)]
pub enum Primary {
    Identifier(Identifier),
    NilLiteral(NilLiteral),
    BooleanLiteral(BooleanLiteral),
    StringLiteral(StringLiteral),
    NumberLiteral(NumberLiteral),
    This(This),
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
                ThisToken => Self::This(This::cast(child)?),
                _ => return None,
            })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        use Primary::*;

        match self {
            Identifier(ident) => ident.start(),
            NilLiteral(nil) => nil.start(),
            BooleanLiteral(boolean) => boolean.start(),
            StringLiteral(string) => string.start(),
            NumberLiteral(number) => number.start(),
            This(this) => this.start(),
        }
    }

    pub fn end(&self) -> usize {
        use Primary::*;

        match self {
            Identifier(ident) => ident.end(),
            NilLiteral(nil) => nil.end(),
            BooleanLiteral(boolean) => boolean.end(),
            StringLiteral(string) => string.end(),
            NumberLiteral(number) => number.end(),
            This(this) => this.end(),
        }
    }
}

#[derive(Debug)]
pub struct CallExpr {
    inner: SyntaxNode,
}

impl CallExpr {
    fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::CallExprNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
    }

    pub fn function(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }

    pub fn args(&self) -> impl Iterator<Item = Expr> {
        self.inner
            .children()
            .find_map(|child| {
                if child.kind() == SyntaxKind::ArgsNode {
                    Some(child.children().filter_map(Expr::cast))
                } else {
                    None
                }
            })
            .into_iter()
            .flatten()
    }
}

#[derive(Debug)]
pub struct SuperMethodExpr {
    inner: SyntaxNode,
}

impl SuperMethodExpr {
    fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::SuperMethodExprNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
    }

    pub fn method_name(&self) -> Option<Identifier> {
        self.inner.children_with_tokens().find_map(try_as_ident)
    }
}

#[derive(Debug)]
pub enum Expr {
    ParenExpr(ParenExpr),
    UnaryOp(UnaryOp),
    BinOp(BinOp),
    Primary(Primary),
    Call(CallExpr),
    SuperMethod(SuperMethodExpr),
}

impl Expr {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        Some(match inner.kind() {
            ExprNode => Self::cast(inner.first_child()?)?,
            ParenExprNode => Self::ParenExpr(ParenExpr::cast(inner)?),
            UnaryOpNode => Self::UnaryOp(UnaryOp::cast(inner)?),
            BinOpNode => Self::BinOp(BinOp::cast(inner)?),
            PrimaryExprNode => Self::Primary(Primary::cast(inner)?),
            CallExprNode => Self::Call(CallExpr::cast(inner)?),
            SuperMethodExprNode => Self::SuperMethod(SuperMethodExpr::cast(inner)?),
            _ => return None,
        })
    }

    pub fn start(&self) -> usize {
        use Expr::*;

        match self {
            ParenExpr(expr) => expr.start(),
            UnaryOp(op) => op.start(),
            BinOp(op) => op.start(),
            Primary(p) => p.start(),
            Call(c) => c.start(),
            SuperMethod(c) => c.start(),
        }
    }

    pub fn end(&self) -> usize {
        use Expr::*;

        match self {
            ParenExpr(expr) => expr.end(),
            UnaryOp(op) => op.end(),
            BinOp(op) => op.end(),
            Primary(p) => p.end(),
            Call(c) => c.end(),
            SuperMethod(c) => c.end(),
        }
    }

    pub fn is_valid_place(&self) -> bool {
        match self {
            Expr::Primary(Primary::Identifier(_)) => true,
            Expr::BinOp(binop) if binop.kind() == BinOpKind::Dot => {
                let mut operands = binop.operands();
                let _lhs = match operands.next() {
                    Some(lhs) => lhs,
                    None => return false,
                };
                matches!(operands.next(), Some(Expr::Primary(Primary::Identifier(_))))
            }
            _ => false,
        }
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn return_position(&self) -> usize {
        self.inner.text_range().end().into()
    }

    pub fn ident(&self) -> Option<Identifier> {
        self.inner.children_with_tokens().find_map(try_as_ident)
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct FunDecl {
    inner: SyntaxNode,
}

impl FunDecl {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::FunDeclNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn return_position(&self) -> usize {
        // the position of `}` if exists.
        // return the end of the node if not exist.
        self.inner
            .children_with_tokens()
            .find_map(|child| match child {
                NodeOrToken::Token(token) if token.kind() == SyntaxKind::BraceCloseToken => {
                    Some(token.text_range().start().into())
                }
                _ => None,
            })
            .unwrap_or_else(|| self.inner.text_range().end().into())
    }

    pub fn ident(&self) -> Option<Identifier> {
        self.inner.children_with_tokens().find_map(try_as_ident)
    }

    pub fn params(&self) -> impl Iterator<Item = Identifier> {
        self.inner
            .children()
            .find_map(|child| {
                if child.kind() == SyntaxKind::FunParamsNode {
                    Some(child.children_with_tokens().filter_map(try_as_ident))
                } else {
                    None
                }
            })
            .into_iter()
            .flatten()
    }

    pub fn body(&self) -> Option<BlockStmt> {
        self.inner.children().find_map(BlockStmt::cast)
    }
}

#[derive(Debug)]
pub struct ClassDecl {
    inner: SyntaxNode,
}

impl ClassDecl {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::ClassDeclNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn return_position(&self) -> usize {
        // the position of `}` if exists.
        // return the end of the node if not exist.
        self.inner
            .children_with_tokens()
            .find_map(|child| match child {
                NodeOrToken::Token(token) if token.kind() == SyntaxKind::BraceCloseToken => {
                    Some(token.text_range().start().into())
                }
                _ => None,
            })
            .unwrap_or_else(|| self.inner.text_range().end().into())
    }

    pub fn ident(&self) -> Option<Identifier> {
        self.inner.children_with_tokens().find_map(try_as_ident)
    }

    pub fn super_class(&self) -> Option<Identifier> {
        self.inner
            .children_with_tokens()
            .filter_map(try_as_ident)
            .nth(1)
    }

    pub fn methods(&self) -> impl Iterator<Item = FunDecl> {
        self.inner.children().filter_map(FunDecl::cast)
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
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

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    inner: SyntaxNode,
}

impl ReturnStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::ReturnStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct DeferStmt {
    inner: SyntaxNode,
}

impl DeferStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::DeferStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn return_position(&self) -> usize {
        // the position of `}` if exists.
        // return the end of the node if not exist.
        self.inner
            .children_with_tokens()
            .find_map(|child| match child {
                NodeOrToken::Token(token) if token.kind() == SyntaxKind::BraceCloseToken => {
                    Some(token.text_range().start().into())
                }
                _ => None,
            })
            .unwrap_or_else(|| self.inner.text_range().end().into())
    }

    pub fn block(&self) -> Option<BlockStmt> {
        self.inner.children().find_map(BlockStmt::cast)
    }
}

#[derive(Debug)]
pub struct BlockStmt {
    inner: SyntaxNode,
}

impl BlockStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::BlockStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn decls(&self) -> impl Iterator<Item = Decl> {
        self.inner.children().filter_map(Decl::cast)
    }
}

#[derive(Debug)]
pub struct IfStmt {
    inner: SyntaxNode,
}

impl IfStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::IfStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
    }

    pub fn cond(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }

    pub fn branches(&self) -> impl Iterator<Item = Stmt> {
        self.inner.children().filter_map(Stmt::cast)
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    inner: SyntaxNode,
}

impl WhileStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::WhileStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
    }

    pub fn cond(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }

    pub fn body(&self) -> Option<Stmt> {
        self.inner.children().find_map(Stmt::cast)
    }
}

#[derive(Debug)]
pub enum ForInit {
    VarDecl(VarDecl),
    Expr(Expr),
}

impl ForInit {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        if inner.kind() == SyntaxKind::ForInitNode {
            let child = inner.first_child()?;
            Some(match child.kind() {
                DeclNode => Self::VarDecl(match Decl::cast(child)? {
                    Decl::VarDecl(decl) => decl,
                    _ => return None,
                }),
                ExprNode => Self::Expr(Expr::cast(child)?),
                _ => return None,
            })
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct ForCond {
    inner: SyntaxNode,
}

impl ForCond {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::ForCondNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct ForIncr {
    inner: SyntaxNode,
}

impl ForIncr {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::ForIncrNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn expr(&self) -> Option<Expr> {
        self.inner.children().find_map(Expr::cast)
    }
}

#[derive(Debug)]
pub struct ForStmt {
    inner: SyntaxNode,
}

impl ForStmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        if inner.kind() == SyntaxKind::ForStmtNode {
            Some(Self { inner })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        self.inner.text_range().start().into()
    }

    pub fn end(&self) -> usize {
        self.inner.text_range().end().into()
    }

    pub fn init(&self) -> Option<ForInit> {
        self.inner.children().find_map(ForInit::cast)
    }

    pub fn cond(&self) -> Option<ForCond> {
        self.inner.children().find_map(ForCond::cast)
    }

    pub fn incr(&self) -> Option<ForIncr> {
        self.inner.children().find_map(ForIncr::cast)
    }

    pub fn body(&self) -> Option<Stmt> {
        self.inner.children().find_map(Stmt::cast)
    }
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(ExprStmt),
    PrintStmt(PrintStmt),
    ReturnStmt(ReturnStmt),
    DeferStmt(DeferStmt),
    BlockStmt(BlockStmt),
    IfStmt(IfStmt),
    WhileStmt(WhileStmt),
    ForStmt(ForStmt),
}

impl Stmt {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        if inner.kind() == SyntaxKind::StmtNode {
            let child = inner.first_child()?;
            Some(match child.kind() {
                ExprStmtNode => Self::ExprStmt(ExprStmt::cast(child)?),
                PrintStmtNode => Self::PrintStmt(PrintStmt::cast(child)?),
                ReturnStmtNode => Self::ReturnStmt(ReturnStmt::cast(child)?),
                DeferStmtNode => Self::DeferStmt(DeferStmt::cast(child)?),
                BlockStmtNode => Self::BlockStmt(BlockStmt::cast(child)?),
                IfStmtNode => Self::IfStmt(IfStmt::cast(child)?),
                WhileStmtNode => Self::WhileStmt(WhileStmt::cast(child)?),
                ForStmtNode => Self::ForStmt(ForStmt::cast(child)?),
                _ => return None,
            })
        } else {
            None
        }
    }

    pub fn start(&self) -> usize {
        use Stmt::*;

        match self {
            ExprStmt(stmt) => stmt.start(),
            PrintStmt(stmt) => stmt.start(),
            ReturnStmt(stmt) => stmt.start(),
            DeferStmt(stmt) => stmt.start(),
            BlockStmt(stmt) => stmt.start(),
            IfStmt(stmt) => stmt.start(),
            WhileStmt(stmt) => stmt.start(),
            ForStmt(stmt) => stmt.start(),
        }
    }

    pub fn return_position(&self) -> usize {
        use Stmt::*;

        match self {
            ExprStmt(stmt) => stmt.inner.text_range().end().into(),
            PrintStmt(stmt) => stmt.inner.text_range().end().into(),
            ReturnStmt(stmt) => stmt.inner.text_range().end().into(),
            DeferStmt(stmt) => stmt.inner.text_range().end().into(),
            BlockStmt(stmt) => stmt.inner.text_range().end().into(),
            IfStmt(stmt) => stmt.inner.text_range().end().into(),
            WhileStmt(stmt) => stmt.inner.text_range().end().into(),
            ForStmt(stmt) => stmt.inner.text_range().end().into(),
        }
    }
}

#[derive(Debug)]
pub enum Decl {
    VarDecl(VarDecl),
    FunDecl(FunDecl),
    ClassDecl(ClassDecl),
    Stmt(Stmt),
}

impl Decl {
    pub fn cast(inner: SyntaxNode) -> Option<Self> {
        use SyntaxKind::*;

        if inner.kind() == DeclNode {
            let child = inner.first_child()?;
            Some(match child.kind() {
                VarDeclNode => Self::VarDecl(VarDecl::cast(child)?),
                FunDeclNode => Self::FunDecl(FunDecl::cast(child)?),
                ClassDeclNode => Self::ClassDecl(ClassDecl::cast(child)?),
                StmtNode => Self::Stmt(Stmt::cast(child)?),
                _ => return None,
            })
        } else {
            None
        }
    }

    pub fn return_position(&self) -> usize {
        use Decl::*;

        match self {
            VarDecl(decl) => decl.return_position(),
            FunDecl(decl) => decl.return_position(),
            ClassDecl(decl) => decl.return_position(),
            Stmt(decl) => decl.return_position(),
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

mod lexer;
mod parser;

use logos::Logos;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Logos, num_derive::FromPrimitive)]
#[repr(u16)]
pub enum SyntaxKind {
    #[error]
    Error,

    #[regex(r"[ \t\n\f]+")]
    WhitespaceToken,
    #[regex(r"//[^\n]*\n")]
    CommentToken,

    // sigils
    #[token("(")]
    ParenOpenToken,
    #[token(")")]
    ParenCloseToken,
    #[token("{")]
    BraceOpenToken,
    #[token("}")]
    BraceCloseToken,
    #[token(",")]
    CommaToken,
    #[token(".")]
    DotToken,
    #[token("-")]
    MinusToken,
    #[token("+")]
    PlusToken,
    #[token(";")]
    SemicolonToken,
    #[token("/")]
    SlashToken,
    #[token("*")]
    StarToken,
    #[token("!")]
    BangToken,
    #[token("!=")]
    BangEqualToken,
    #[token("=")]
    EqualToken,
    #[token("==")]
    EqualEqualToken,
    #[token(">")]
    GreaterToken,
    #[token(">=")]
    GreaterEqualToken,
    #[token("<")]
    LessToken,
    #[token("<=")]
    LessEqualToken,

    #[regex("[a-zA-Z][a-zA-Z0-9]*")]
    IdentifierToken,
    // not supporting escape sequence
    #[regex(r#""[^"]*""#)]
    StringLiteralToken,
    #[regex("[0-9]+")]
    NumberToken,

    // keywords
    #[token("and")]
    AndToken,
    #[token("class")]
    ClassToken,
    #[token("else")]
    ElseToken,
    #[token("false")]
    FalseToken,
    #[token("for")]
    ForToken,
    #[token("fun")]
    FunToken,
    #[token("if")]
    IfToken,
    #[token("nil")]
    NilToken,
    #[token("or")]
    OrToken,
    #[token("print")]
    PrintToken,
    #[token("return")]
    ReturnToken,
    #[token("super")]
    SuperToken,
    #[token("this")]
    ThisToken,
    #[token("true")]
    TrueToken,
    #[token("var")]
    VarToken,
    #[token("while")]
    WhileToken,

    // nodes
    // expressions
    UnaryOpNode,
    BinOpNode,
    PrimaryExprNode,
    ExprNode,

    // declarations
    ClassDeclNode,
    FunDeclNode,
    VarDeclNode,
    DeclNode,

    // statements
    ExprStmtNode,
    ForInitNode,
    ForCondNode,
    ForIncrNode,
    ForStmtNode,
    IfStmtNode,
    PrintStmtNode,
    ReturnStmtNode,
    WhileStmtNode,
    BlockStmtNode,
    StmtNode,

    // program root
    RootNode,
}

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

impl SyntaxKind {
    pub fn is_trivial(&self) -> bool {
        use SyntaxKind::*;

        matches!(self, WhitespaceToken | CommentToken | Error)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Language {}

impl rowan::Language for Language {
    type Kind = SyntaxKind;

    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        use num::FromPrimitive;

        SyntaxKind::from_u16(raw.0).unwrap()
    }

    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

pub type SyntaxToken = rowan::SyntaxToken<Language>;
pub type SyntaxNode = rowan::SyntaxNode<Language>;

pub fn parse(input: &str) -> SyntaxNode {
    parser::parser(input).parse()
}

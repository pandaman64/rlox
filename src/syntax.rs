mod lexer;
mod parser;

use logos::Logos;

pub use self::parser::SyntaxError;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Logos, num_derive::FromPrimitive)]
#[repr(u16)]
pub enum SyntaxKind {
    // errors
    #[error]
    Error,

    // trivias
    #[regex(r"[ \t\r\n\f]+")]
    WhitespaceToken,
    #[regex(r"//[^\r\n]*\r?\n?")]
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

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    IdentifierToken,
    // not supporting escape sequence
    #[regex(r#""[^"]*"?"#)]
    StringLiteralToken,
    #[regex(r"[0-9]+(\.[0-9]+)?")]
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
    #[token("defer")]
    DeferToken,
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
    ParenExprNode,
    UnaryOpNode,
    BinOpNode,
    PrimaryExprNode,
    ArgsNode,
    CallExprNode,
    SuperMethodExprNode,
    ExprNode,

    // declarations
    ClassDeclNode,
    FunParamsNode,
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
    DeferStmtNode,
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

    pub fn is_keyword(&self) -> bool {
        self.to_keyword_str().is_some()
    }

    pub fn to_keyword_str(&self) -> Option<&'static str> {
        use SyntaxKind::*;

        match self {
            AndToken => Some("and"),
            ClassToken => Some("class"),
            ElseToken => Some("else"),
            FalseToken => Some("false"),
            ForToken => Some("for"),
            FunToken => Some("fun"),
            IfToken => Some("if"),
            NilToken => Some("nil"),
            OrToken => Some("or"),
            PrintToken => Some("print"),
            ReturnToken => Some("return"),
            DeferToken => Some("defer"),
            SuperToken => Some("super"),
            ThisToken => Some("this"),
            TrueToken => Some("true"),
            VarToken => Some("var"),
            WhileToken => Some("while"),
            _ => None,
        }
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
pub type NodeOrToken = rowan::NodeOrToken<SyntaxNode, SyntaxToken>;

pub fn parse(input: &str) -> (SyntaxNode, Vec<SyntaxError>) {
    parser::parser(input).parse()
}

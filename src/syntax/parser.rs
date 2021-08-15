use std::{collections::HashMap, iter::Peekable, ops::Range};

use once_cell::sync::Lazy;
use rowan::GreenNodeBuilder;

use super::{lexer::lex, SyntaxKind, SyntaxNode};

#[derive(Debug)]
pub enum SyntaxError {
    UnexpectedToken {
        expected: SyntaxKind,
        got: SyntaxKind,
        position: usize,
    },
    UnexpectedEof {
        expected: SyntaxKind,
    },
    UnrecognizedToken {
        position: usize,
    },
    UnterminatedStringLiteral {
        position: usize,
    },
    ExpectNode {
        name: &'static str,
        position: usize,
    },
    ExpectIdentifier {
        got: SyntaxKind,
        position: usize,
    },
}

pub struct Parser<'i, I: Iterator<Item = (SyntaxKind, &'i str, Range<usize>)>> {
    builder: GreenNodeBuilder<'static>,
    input: Peekable<I>,
    errors: Vec<SyntaxError>,
    position: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// if a binary operator is left-associative, right precedence has higher binding power
enum BindingPower {
    Zero,
    // = is right associative
    AssignmentRight,
    AssignmentLeft,
    // or is left associative
    OrLeft,
    OrRight,
    // and is left associative
    AndLeft,
    AndRight,
    // ==, != are left associative (non-associativity might be better)
    EqualityLeft,
    EqualityRight,
    // <, >, <=, >=, are left associative
    ComparisonLeft,
    ComparisonRight,
    // +, - are left associative
    TermLeft,
    TermRight,
    // *, / are left associative
    FactorLeft,
    FactorRight,
    // !, - are prefix operators
    PrefixRight,
    // ., () have the same associativity, and . is right associative
    CallLeft,
    CallRight,
}

static BINOP_BINDING_POWER: Lazy<HashMap<SyntaxKind, (BindingPower, BindingPower)>> =
    Lazy::new(|| {
        use BindingPower::*;
        use SyntaxKind::*;

        // consider using phf
        let mut map = HashMap::with_capacity(14);
        map.insert(EqualToken, (AssignmentLeft, AssignmentRight));
        map.insert(OrToken, (OrLeft, OrRight));
        map.insert(AndToken, (AndLeft, AndRight));
        map.insert(EqualEqualToken, (EqualityLeft, EqualityRight));
        map.insert(BangEqualToken, (EqualityLeft, EqualityRight));
        map.insert(LessToken, (ComparisonLeft, ComparisonRight));
        map.insert(GreaterToken, (ComparisonLeft, ComparisonRight));
        map.insert(LessEqualToken, (ComparisonLeft, ComparisonRight));
        map.insert(GreaterEqualToken, (ComparisonLeft, ComparisonRight));
        map.insert(PlusToken, (TermLeft, TermRight));
        map.insert(MinusToken, (TermLeft, TermRight));
        map.insert(StarToken, (FactorLeft, FactorRight));
        map.insert(SlashToken, (FactorLeft, FactorRight));
        map.insert(DotToken, (CallLeft, CallRight));
        map
    });

impl<'i, I> Parser<'i, I>
where
    I: Iterator<Item = (SyntaxKind, &'i str, Range<usize>)>,
{
    fn bump(&mut self) {
        if let Some((token, slice, range)) = self.input.next() {
            // report errors
            match token {
                SyntaxKind::Error => self.errors.push(SyntaxError::UnrecognizedToken {
                    position: range.start,
                }),
                SyntaxKind::StringLiteralToken if !slice.ends_with('"') => {
                    self.errors.push(SyntaxError::UnterminatedStringLiteral {
                        position: range.start,
                    })
                }
                _ => {}
            }

            self.position = range.end;
            self.builder.token(token.into(), slice);
        }
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        fn is_trivial(&(kind, _, _): &(SyntaxKind, &str, Range<usize>)) -> bool {
            kind.is_trivial()
        }

        while self.input.peek().map_or_else(|| false, is_trivial) {
            self.bump();
        }

        self.input.peek().map(|(kind, _, _)| *kind)
    }

    fn expect(&mut self, expected: SyntaxKind) {
        match self.peek() {
            Some(token) if token == expected => self.bump(),
            Some(got) => {
                if expected == SyntaxKind::IdentifierToken && got.is_keyword() {
                    // if expected token is an identifier and get a keyword, emit a special error
                    // and continue parsing as if it succeeds.
                    self.errors.push(SyntaxError::ExpectIdentifier {
                        got,
                        position: self.position,
                    });
                    self.bump();
                } else {
                    self.errors.push(SyntaxError::UnexpectedToken {
                        expected,
                        got,
                        position: self.position,
                    });
                }
            }
            None => self.errors.push(SyntaxError::UnexpectedEof { expected }),
        }
    }

    // Crafting interpreter uses a jump table for matches in this function
    fn parse_expr(&mut self, bp: BindingPower) {
        use BindingPower::*;
        use SyntaxKind::*;

        let checkpoint = match self.peek() {
            None => return,
            Some(token) => {
                self.builder.start_node(ExprNode.into());
                let checkpoint = self.builder.checkpoint();
                match token {
                    MinusToken | BangToken => {
                        self.builder.start_node(UnaryOpNode.into());
                        self.bump();
                        self.parse_expr(PrefixRight);
                        self.builder.finish_node();
                    }
                    ParenOpenToken => {
                        self.builder.start_node(ExprNode.into());
                        self.bump();
                        self.parse_expr(Zero);
                        self.expect(ParenCloseToken);
                        self.builder.finish_node();
                    }
                    IdentifierToken | StringLiteralToken | NumberToken | NilToken | TrueToken
                    | FalseToken => {
                        self.builder.start_node(PrimaryExprNode.into());
                        self.bump();
                        self.builder.finish_node();
                    }
                    _ => {
                        self.errors.push(SyntaxError::ExpectNode {
                            name: "expression",
                            position: self.position,
                        });
                        self.bump();
                    }
                }
                checkpoint
            }
        };

        loop {
            match self.peek() {
                None => break,
                Some(
                    kind
                    @
                    (EqualToken | OrToken | AndToken | EqualEqualToken | BangEqualToken
                    | LessToken | GreaterToken | LessEqualToken | GreaterEqualToken
                    | PlusToken | MinusToken | StarToken | SlashToken | DotToken),
                ) => {
                    let &(lbp, rbp) = BINOP_BINDING_POWER.get(&kind).unwrap();
                    if lbp < bp {
                        break;
                    }

                    self.builder.start_node_at(checkpoint, BinOpNode.into());
                    self.bump();
                    self.parse_expr(rbp);
                    self.builder.finish_node();
                }
                Some(ParenOpenToken) => {
                    let lbp = CallLeft;
                    if lbp < bp {
                        break;
                    }

                    self.builder.start_node_at(checkpoint, CallExprNode.into());
                    self.bump();
                    self.builder.start_node(ArgsNode.into());
                    while let Some(next) = self.peek() {
                        if next == ParenCloseToken {
                            break;
                        }

                        self.parse_expr(BindingPower::Zero);
                        if matches!(self.peek(), Some(CommaToken)) {
                            self.bump();
                        }
                    }
                    self.builder.finish_node();
                    self.expect(ParenCloseToken);
                    self.builder.finish_node();
                }
                _ => break,
            }
        }

        self.builder.finish_node();
    }

    fn parse_block_stmt(&mut self) {
        use SyntaxKind::*;

        self.builder.start_node(BlockStmtNode.into());
        self.expect(BraceOpenToken);
        while let Some(next) = self.peek() {
            if next == BraceCloseToken {
                break;
            }
            self.parse_decl();
        }
        self.expect(BraceCloseToken);
        self.builder.finish_node();
    }

    fn parse_stmt(&mut self) {
        use SyntaxKind::*;

        match self.peek() {
            None => {}
            Some(token) => {
                self.builder.start_node(StmtNode.into());
                match token {
                    PrintToken => {
                        self.builder.start_node(PrintStmtNode.into());
                        self.bump();
                        self.parse_expr(BindingPower::Zero);
                        self.expect(SemicolonToken);
                        self.builder.finish_node();
                    }
                    ReturnToken => {
                        self.builder.start_node(ReturnStmtNode.into());
                        self.bump();
                        if matches!(self.peek(), Some(SemicolonToken)) {
                            self.bump();
                        } else {
                            self.parse_expr(BindingPower::Zero);
                            self.expect(SemicolonToken);
                        }
                        self.builder.finish_node();
                    }
                    BraceOpenToken => self.parse_block_stmt(),
                    IfToken => {
                        self.builder.start_node(IfStmtNode.into());
                        self.bump();
                        self.expect(ParenOpenToken);
                        self.parse_expr(BindingPower::Zero);
                        self.expect(ParenCloseToken);
                        self.parse_stmt();
                        if matches!(self.peek(), Some(ElseToken)) {
                            self.bump();
                            self.parse_stmt();
                        }
                        self.builder.finish_node();
                    }
                    WhileToken => {
                        self.builder.start_node(WhileStmtNode.into());
                        self.bump();
                        self.expect(ParenOpenToken);
                        self.parse_expr(BindingPower::Zero);
                        self.expect(ParenCloseToken);
                        self.parse_stmt();
                        self.builder.finish_node();
                    }
                    ForToken => {
                        self.builder.start_node(ForStmtNode.into());
                        self.bump();
                        self.expect(ParenOpenToken);

                        self.builder.start_node(ForInitNode.into());
                        match self.peek() {
                            // no initializer
                            Some(SemicolonToken) => self.expect(SemicolonToken),
                            // variable declaration
                            Some(VarToken) => self.parse_decl(),
                            // expression
                            _ => {
                                self.parse_expr(BindingPower::Zero);
                                self.expect(SemicolonToken);
                            }
                        }
                        self.builder.finish_node();

                        // condition
                        self.builder.start_node(ForCondNode.into());
                        match self.peek() {
                            // no condition
                            Some(SemicolonToken) => {}
                            _ => self.parse_expr(BindingPower::Zero),
                        }
                        self.builder.finish_node();
                        self.expect(SemicolonToken);

                        // increment
                        self.builder.start_node(ForIncrNode.into());
                        match self.peek() {
                            Some(ParenCloseToken) => {}
                            _ => self.parse_expr(BindingPower::Zero),
                        }
                        self.builder.finish_node();
                        self.expect(ParenCloseToken);

                        self.parse_stmt();

                        self.builder.finish_node();
                    }
                    // expression statements
                    _ => {
                        self.builder.start_node(ExprStmtNode.into());
                        self.parse_expr(BindingPower::Zero);
                        self.expect(SemicolonToken);
                        self.builder.finish_node();
                    }
                }
                self.builder.finish_node();
            }
        }
    }

    fn parse_decl(&mut self) {
        use SyntaxKind::*;

        match self.peek() {
            None => {}
            Some(token) => {
                self.builder.start_node(DeclNode.into());
                match token {
                    VarToken => {
                        self.builder.start_node(VarDeclNode.into());
                        self.bump();
                        self.expect(IdentifierToken);
                        if matches!(self.peek(), Some(EqualToken)) {
                            self.bump();
                            self.parse_expr(BindingPower::Zero);
                        }
                        self.expect(SemicolonToken);
                        self.builder.finish_node();
                    }
                    FunToken => {
                        self.builder.start_node(FunDeclNode.into());
                        self.bump();
                        self.expect(IdentifierToken);
                        self.expect(ParenOpenToken);
                        self.builder.start_node(FunParamsNode.into());
                        while let Some(next) = self.peek() {
                            if next == ParenCloseToken {
                                break;
                            }

                            self.expect(IdentifierToken);
                            if matches!(self.peek(), Some(CommaToken)) {
                                self.bump();
                            }
                        }
                        self.builder.finish_node();
                        self.expect(ParenCloseToken);
                        self.parse_block_stmt();
                        self.builder.finish_node();
                    }
                    // statements
                    _ => {
                        self.parse_stmt();
                    }
                }
                self.builder.finish_node()
            }
        }
    }

    /// parse a program
    pub fn parse(mut self) -> (SyntaxNode, Vec<SyntaxError>) {
        self.builder.start_node(SyntaxKind::RootNode.into());
        while self.peek().is_some() {
            self.parse_decl();
        }
        self.builder.finish_node();

        let root = SyntaxNode::new_root(self.builder.finish());
        let errors = self.errors;

        (root, errors)
    }
}

pub fn parser(input: &str) -> Parser<'_, impl Iterator<Item = (SyntaxKind, &str, Range<usize>)>> {
    Parser {
        builder: GreenNodeBuilder::default(),
        input: lex(input).peekable(),
        errors: vec![],
        position: 0,
    }
}

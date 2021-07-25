use std::{collections::HashMap, iter::Peekable, ops::Range};

use once_cell::sync::Lazy;
use rowan::GreenNodeBuilder;

use super::{lexer::lex, SyntaxKind, SyntaxNode};

pub struct Parser<'i, I: Iterator<Item = (SyntaxKind, &'i str, Range<usize>)>> {
    builder: GreenNodeBuilder<'static>,
    input: Peekable<I>,
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
        if let Some((token, slice, _range)) = self.input.next() {
            self.builder.token(token.into(), slice)
        }
    }

    fn peek(&mut self) -> Option<SyntaxKind> {
        fn is_trivial(&(kind, _, _): &(SyntaxKind, &str, Range<usize>)) -> bool {
            kind.is_trivial()
        }

        while self.input.next_if(is_trivial).is_some() {}

        self.input.peek().map(|(kind, _, _)| *kind)
    }

    // Crafting interpreter uses a jump table for matches in this function
    fn parse_expr(&mut self, bp: BindingPower) {
        use BindingPower::*;
        use SyntaxKind::*;

        let checkpoint = self.builder.checkpoint();
        match self.peek() {
            None => return,
            Some(MinusToken | BangToken) => {
                self.builder.start_node(UnaryOpNode.into());
                self.bump();
                self.parse_expr(PrefixRight);
                self.builder.finish_node();
            }
            Some(ParenOpenToken) => {
                self.builder.start_node(ExprNode.into());
                self.bump();
                self.parse_expr(Zero);
                if matches!(self.peek(), Some(ParenCloseToken)) {
                    self.bump();
                }
                self.builder.finish_node();
            }
            Some(
                IdentifierToken | StringLiteralToken | NumberToken | NilToken | TrueToken
                | FalseToken,
            ) => {
                self.builder.start_node(PrimaryExprNode.into());
                self.bump();
                self.builder.finish_node();
            }
            _ => todo!("syntax error"),
        }

        loop {
            match self.peek() {
                None => return,
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
                Some(ParenOpenToken) => todo!("function call"),
                _ => break,
            }
        }
    }

    pub fn parse(mut self) -> SyntaxNode {
        self.builder.start_node(SyntaxKind::ExprNode.into());
        self.parse_expr(BindingPower::Zero);
        self.builder.finish_node();

        SyntaxNode::new_root(self.builder.finish())
    }
}

pub fn parser(input: &str) -> Parser<'_, impl Iterator<Item = (SyntaxKind, &str, Range<usize>)>> {
    Parser {
        builder: GreenNodeBuilder::default(),
        input: lex(input).peekable(),
    }
}

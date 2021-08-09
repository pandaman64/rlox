use std::ops::Range;

use super::SyntaxKind;
use logos::Logos;

pub fn lex(input: &str) -> impl Iterator<Item = (SyntaxKind, &str, Range<usize>)> + '_ {
    let mut lexer = SyntaxKind::lexer(input);
    std::iter::from_fn(move || {
        let kind = lexer.next()?;
        let span = lexer.span();
        let slice = lexer.slice();

        Some((kind, slice, span))
    })
}

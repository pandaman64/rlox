use std::ops::Range;

use crate::trace_available;

use super::SyntaxKind;
use logos::Logos;

pub fn lex(input: &str) -> impl Iterator<Item = (SyntaxKind, &str, Range<usize>)> + '_ {
    let mut lexer = SyntaxKind::lexer(input);
    std::iter::from_fn(move || {
        let kind = lexer.next()?;
        let span = lexer.span();
        let slice = lexer.slice();
        if trace_available() {
            eprintln!("{:?}", (kind, slice, span.clone()));
        }

        Some((kind, slice, span))
    })
}

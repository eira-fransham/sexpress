#![feature(dotdoteq_in_patterns, inclusive_range_syntax, ascii_ctype)]

extern crate toolshed;

use toolshed::Arena;
use toolshed::set::Set;

use std::str::FromStr;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Sexp<'arena> {
    Quote(&'arena Sexp<'arena>),
    Cons(&'arena Sexp<'arena>, &'arena Sexp<'arena>),
    String(&'arena str),
    Symbol(&'arena str),
    Int(i64),
    Float(f64),
    Nil,
}

impl<'a> Sexp<'a> {
    pub fn len(&self) -> Option<usize> {
        match *self {
            Sexp::Cons(_, rest) => Some(1 + rest.len().unwrap_or(0)),
            _ => None,
        }
    }
}

#[inline(always)]
fn between_inclusive(a: u8, b: u8, c: u8) -> bool {
    a.wrapping_sub(b) <= c.wrapping_sub(b)
}

fn is_identifier_start_char(c: u8) -> bool {
    between_inclusive(c, b'a', b'z') ||
        // We start at < to get <=>?@
        between_inclusive(c, b'<', b'Z') || between_inclusive(c, b'#', b'&') || c == b'-'
        || c == b'/' || c == b'+' || c == b'*' || c == b'~' || c == b'_' || c == b'^'
        || c == b':' || c == b'!'
}

fn is_bracket(c: u8) -> bool {
    c == b'[' || c == b']' || c == b'(' || c == b')' || c == b'{' || c == b'}'
}

fn is_identifier_char(c: u8) -> bool {
    is_identifier_start_char(c) || c == b'.' || between_inclusive(c, b'0', b'9')
}

static NIL: Sexp = Sexp::Nil;

pub fn parse<'a, 'b>(arena: &'a Arena, intern: &'b mut Set<&'a str>, input: &'a [u8]) -> Result<Sexp<'a>, &'static str> {
    parse_inner(&mut 0, arena, intern, input)
}

pub fn parse_many<'a, 'b>(arena: &'a Arena, intern: &'b mut Set<&'a str>, input: &'a [u8]) -> Result<Sexp<'a>, &'static str> {
    let mut counter = 0;

    // TODO: Smallvec?
    let mut output: Vec<Sexp> = vec![];

    macro_rules! skip_whitespace {
        () => {
            while counter < input.len() && input[counter].is_ascii_whitespace() {
                counter += 1;
            }
        }
    }

    loop {
        if counter >= input.len() {
            break;
        }

        let current = parse_inner(&mut counter, arena, intern, input)?;
        output.push(current);

        skip_whitespace!();
    }

    let output = arena.alloc_many(output);

    static NIL: Sexp = Sexp::Nil;

    if let Some(fin) = output.last() {
        let last = Sexp::Cons(fin, &NIL);

        let out: Sexp = output[..output.len() - 1]
            .into_iter()
            .rev()
            .fold(last, |last, cur| Sexp::Cons(cur, arena.alloc(last)));

        Ok(out)
    } else {
        Ok(NIL)
    }
}

fn parse_inner<'a, 'b>(
    counter: &'b mut usize,
    arena: &'a Arena,
    intern: &'b mut Set<&'a str>,
    input: &'a [u8],
) -> Result<Sexp<'a>, &'static str> {
    use std::str;

    macro_rules! assert_end_of_token {
        () => {
            if *counter < input.len() &&
                !input[*counter].is_ascii_whitespace() &&
                !is_bracket(input[*counter])
            {
                return Err("Invalid token");
            }
        }
    }

    macro_rules! skip_whitespace {
        () => {
            while *counter < input.len() && input[*counter].is_ascii_whitespace() {
                *counter += 1;
            }
        }
    }

    if *counter >= input.len() {
        return Err("Unexpected EOF");
    }

    skip_whitespace!();

    match input[*counter] {
        // Cons
        b'(' => {
            *counter += 1;
            skip_whitespace!();

            // TODO: Smallvec?
            let mut output: Vec<Sexp> = vec![];

            loop {
                if *counter >= input.len() {
                    return Err("Unexpected EOF");
                } else if input[*counter] == b')' {
                    *counter += 1;
                    break;
                }

                let current = parse_inner(counter, arena, intern, input)?;
                output.push(current);

                skip_whitespace!();
            }

            let output = arena.alloc_many(output);

            if let Some(fin) = output.last() {
                let last = Sexp::Cons(fin, &NIL);

                let out: Sexp = output[..output.len() - 1]
                    .into_iter()
                    .rev()
                    .fold(last, |last, cur| Sexp::Cons(cur, arena.alloc(last)));

                Ok(out)
            } else {
                Ok(NIL)
            }
        }
        // String
        b'"' => {
            *counter += 1;
            let start = *counter;

            while *counter < input.len() && input[*counter] != b'"' {
                if !input[*counter].is_ascii() {
                    return Err("Non-ascii character in string");
                }

                *counter += 1;
            }

            // We explicitly check for ascii characters only
            let string = unsafe { str::from_utf8_unchecked(&input[start..*counter]) };

            let string = if let Some(&interned_string) = intern.get(string) {
                interned_string
            } else {
                // TODO: Make own `interner` hashset reimplementation
                intern.insert(arena, string);
                string
            };

            *counter += 1;

            Ok(Sexp::String(string))
        }
        // Number (float or int)
        b'0'..=b'9' => {
            let mut is_float = false;
            let start = *counter;

            while *counter < input.len() {
                if input[*counter] == b'.' {
                    is_float = true;
                    *counter += 1;
                } else if between_inclusive(input[*counter], b'0', b'9') {
                    *counter += 1;
                } else {
                    break;
                }
            }

            assert_end_of_token!();

            // We only advance if the bytes are in `.0123456789` so we know that it is valid UTF8
            if is_float {
                f64::from_str(unsafe { str::from_utf8_unchecked(&input[start..*counter]) })
                    .map(Sexp::Float)
                    .map_err(|_| "Invalid float")
            } else {
                i64::from_str(unsafe { str::from_utf8_unchecked(&input[start..*counter]) })
                    .map(Sexp::Int)
                    .map_err(|_| "Invalid int")
            }
        }
        b'\'' => {
            *counter += 1;

            let next_token = parse_inner(counter, arena, intern, input)?;

            Ok(Sexp::Quote(arena.alloc(next_token)))
        }
        // Symbol
        a if is_identifier_start_char(a) => {
            let start = *counter;
            *counter += 1;

            while *counter < input.len() && is_identifier_char(input[*counter]) {
                *counter += 1;
            }

            assert_end_of_token!();

            // We know that it's a valid ascii string because we used `is_identifier_char`
            Ok(Sexp::Symbol(unsafe {
                str::from_utf8_unchecked(&input[start..*counter])
            }))
        }
        _ => return Err("Unexpected token"),
    }
}

#[cfg(test)]
mod tests {
    use super::{is_identifier_char, is_identifier_start_char, parse, parse_many, Arena, Sexp};

    macro_rules! ident_start {
        () => {
            (b'a'..=b'z')
                .chain(b'A'..=b'Z')
                .chain(b"!@#$%^&*-_=+<>?:~/".into_iter().cloned())
        }
    }

    macro_rules! ident_rest {
        () => {
            ident_start!().chain(::std::iter::once(b'.')).chain(b'0'..=b'9')
        }
    }

    #[test]
    fn parses_files() {
        let arena = Arena::new();
        let mut intern = Default::default();
        let files = [(include_bytes!("./filetests/prime-decomposition.lisp"), 3)];

        for &(file, expected_len) in &files {
            assert_eq!(
                parse_many(&arena, &mut intern, file).expect("Failed to parse").len(),
                Some(expected_len)
            );
        }
    }

    #[test]
    fn parses_everything() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(
            parse(
                &arena,
                &mut intern,
                b"( set!( get-x-ref myStruct )
                        2 \"test\" '() '(1 2 3))"
            ),
            Ok(Sexp::Cons(
                &Sexp::Symbol("set!"),
                &Sexp::Cons(
                    &Sexp::Cons(
                        &Sexp::Symbol("get-x-ref"),
                        &Sexp::Cons(&Sexp::Symbol("myStruct"), &Sexp::Nil)
                    ),
                    &Sexp::Cons(
                        &Sexp::Int(2),
                        &Sexp::Cons(
                            &Sexp::String("test"),
                            &Sexp::Cons(
                                &Sexp::Quote(&Sexp::Nil),
                                &Sexp::Cons(
                                    &Sexp::Quote(&Sexp::Cons(
                                        &Sexp::Int(1),
                                        &Sexp::Cons(
                                            &Sexp::Int(2),
                                            &Sexp::Cons(&Sexp::Int(3), &Sexp::Nil)
                                        )
                                    )),
                                    &Sexp::Nil,
                                ),
                            )
                        )
                    )
                )
            ))
        );
    }

    #[test]
    fn parses_string() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(parse(&arena, &mut intern, b"\"Test!\""), Ok(Sexp::String("Test!")));

        assert_eq!(
            parse(&arena, &mut intern, b"\"(((}{{}{}{}}}}\""),
            Ok(Sexp::String("(((}{{}{}{}}}}"))
        );
    }

    #[test]
    fn parses_ident() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(
            parse(&arena, &mut intern, b"testing-a-thing"),
            Ok(Sexp::Symbol("testing-a-thing")),
        );
        assert_eq!(
            parse(&arena, &mut intern, b"has-number-10"),
            Ok(Sexp::Symbol("has-number-10")),
        );
        assert_eq!(parse(&arena, &mut intern, b"10-fails").map_err(|_| ()), Err(()),);
    }

    #[test]
    fn parses_numbers() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(parse(&arena, &mut intern, b"2"), Ok(Sexp::Int(2)),);
        assert_eq!(parse(&arena, &mut intern, b"10"), Ok(Sexp::Int(10)),);
        assert_eq!(parse(&arena, &mut intern, b"10."), Ok(Sexp::Float(10.)),);
        assert_eq!(parse(&arena, &mut intern, b"10.5"), Ok(Sexp::Float(10.5)),);
        assert_eq!(parse(&arena, &mut intern, b"10.5.6.7.8").map_err(|_| ()), Err(()),);
    }

    #[test]
    fn parses_ident_start() {
        use std::collections::HashSet;

        let ident_chars = ident_start!().collect::<HashSet<_>>();

        for c in 0..255 {
            if ident_chars.contains(&c) {
                assert!(
                    is_identifier_start_char(c),
                    "{} should be an identifier char but is not",
                    c as char
                );
            } else {
                assert!(
                    !is_identifier_start_char(c),
                    "{} should not be an identifier char but is",
                    c as char
                );
            }
        }
    }

    #[test]
    fn parses_idents() {
        use std::collections::HashSet;

        let ident_chars = ident_rest!().collect::<HashSet<_>>();

        for c in 0..255 {
            if ident_chars.contains(&c) {
                assert!(
                    is_identifier_char(c),
                    "{} should be an identifier char but is not",
                    c as char
                );
            } else {
                assert!(
                    !is_identifier_char(c),
                    "{} should not be an identifier char but is",
                    c as char
                );
            }
        }
    }

}

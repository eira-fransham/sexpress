#![feature(dotdoteq_in_patterns, inclusive_range_syntax, ascii_ctype, test,
           conservative_impl_trait)]

extern crate toolshed;

use toolshed::Arena;
use toolshed::set::Set;

use std::str::FromStr;
use std::borrow::Cow;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Sexp<'arena> {
    Quote(&'arena Sexp<'arena>),
    Metaquote(&'arena Sexp<'arena>),
    Unquote(&'arena Sexp<'arena>),
    // The last element is usally nil
    List(&'arena [Sexp<'arena>], &'arena Sexp<'arena>),
    String(&'arena str),
    Symbol(&'arena str),
    Int(i64),
    Float(f64),
    Nil,
}

impl<'a> Sexp<'a> {
    pub fn len(&self) -> Option<usize> {
        match *self {
            Sexp::List(arr, el) => Some(arr.len() + el.len().unwrap_or(0)),
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

pub fn parse<'a, 'b>(
    arena: &'a Arena,
    intern: &'b mut Set<&'a str>,
    input: &'a [u8],
) -> Result<Sexp<'a>, Cow<'static, str>> {
    parse_inner(&mut 0, arena, intern, input)
}

pub struct ParseManyIterator<'im: 'mu, 'mu> {
    inner: ParseMany<'im>,
    arena: &'im Arena,
    intern: &'mu mut Set<'im, &'im str>,
}

impl<'im, 'mu> Iterator for ParseManyIterator<'im, 'mu> {
    type Item = Result<Sexp<'im>, Cow<'static, str>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next(self.arena, &mut self.intern)
    }
}

pub struct ParseMany<'a> {
    counter: usize,
    err: bool,
    input: &'a [u8],
}

impl<'im> ParseMany<'im> {
    pub fn into_iter<'mu>(self, arena: &'im Arena, intern: &'mu mut Set<'im, &'im str>) -> ParseManyIterator<'im, 'mu> {
        ParseManyIterator {
            inner: self,
            arena,
            intern,
        }
    }

    pub fn next<'inner, 'mu>(
        &mut self,
        arena: &'inner Arena,
        intern: &'mu mut Set<'inner, &'inner str>,
    ) -> Option<Result<Sexp<'inner>, Cow<'static, str>>> where 'im: 'inner {
        while self.counter < self.input.len() && self.input[self.counter].is_ascii_whitespace() {
            self.counter += 1;
        }

        if self.err || self.counter >= self.input.len() {
            None
        } else {
            match parse_inner(&mut self.counter, arena, intern, self.input) {
                Ok(sexp) => Some(Ok(sexp)),
                Err(err) => {
                    self.err = true;

                    Some(Err(err))
                }
            }
        }
    }
}

pub fn parse_many(input: &[u8]) -> ParseMany {
    ParseMany {
        counter: 0,
        err: false,
        input,
    }
}

fn parse_inner<'a, 'b>(
    counter: &'b mut usize,
    arena: &'a Arena,
    intern: &'b mut Set<&'a str>,
    input: &'a [u8],
) -> Result<Sexp<'a>, Cow<'static, str>> {
    use std::str;

    macro_rules! assert_end_of_token {
        () => {
            if *counter < input.len() &&
                !input[*counter].is_ascii_whitespace() &&
                !is_bracket(input[*counter])
            {
                return Err(format!(
                    "Unexpected token {:?} at character {}",
                    char::from(input[*counter]),
                    counter
                ).into());
            }
        }
    }

    macro_rules! skip_whitespace {
        () => {
            while *counter < input.len()  {
                match input[*counter] {
                    // Comment
                    b';' => {
                        while *counter < input.len() && input[*counter] != b'\n' {
                            *counter += 1;
                        }

                        *counter += 1;
                    }
                    other if other.is_ascii_whitespace() => {
                        *counter += 1;
                    }
                    _ => break,
                }
            }
        }
    }

    if *counter >= input.len() {
        return Err(format!("Unexpected EOF at {}", counter).into());
    }

    loop {
        skip_whitespace!();

        match input[*counter] {
            // Cons
            b'(' => {
                *counter += 1;

                skip_whitespace!();

                // TODO: Smallvec?
                let mut output: Vec<Sexp> = vec![];
                let mut last = &NIL;

                loop {
                    if *counter >= input.len() {
                        return Err("Unexpected EOF".into());
                    } else if input[*counter] == b')' {
                        *counter += 1;
                        break;
                    } else if input[*counter] == b'.' {
                        *counter += 1;

                        assert_end_of_token!();
                        skip_whitespace!();

                        last = arena.alloc(parse_inner(counter, arena, intern, input)?);

                        skip_whitespace!();

                        if input[*counter] == b')' {
                            *counter += 1;
                            break;
                        } else {
                            return Err("Invalid cons".into());
                        }
                    }

                    let current = parse_inner(counter, arena, intern, input)?;
                    output.push(current);

                    skip_whitespace!();
                }

                if output.is_empty() {
                    return Ok(Sexp::Nil);
                } else {
                    let output = arena.alloc_many(output);

                    return Ok(Sexp::List(output, last));
                }
            }
            // String
            b'"' => {
                *counter += 1;
                let start = *counter;
                let mut backslash_locs = vec![];

                while *counter < input.len() && input[*counter] != b'"' {
                    let cur = input[*counter];
                    if !cur.is_ascii() {
                        return Err("Non-ascii character in string".into());
                    } else if cur.is_ascii_control() && !(cur == b'\n' || cur == b'\r') {
                        return Err("Ascii control character in string".into());
                    } else if cur == b'\\' {
                        // TODO: Handle this properly
                        backslash_locs.push(*counter - start);
                        *counter += 1;

                        if input[*counter] != b'"' && input[*counter] != b'\\' {
                            return Err(format!(
                                "Invalid escape: \\{}",
                                std::char::from_u32(input[*counter] as _).expect(
                                    "We know this is ascii because of the check above, Q.E.D."
                                )
                            ).into());
                        }
                    }

                    *counter += 1;
                }

                let bytes = &input[start..*counter];

                let string = if backslash_locs.is_empty() {
                    unsafe { str::from_utf8_unchecked(bytes) }
                } else {
                    let mut without_backslashes = bytes.to_vec();

                    for loc in backslash_locs.into_iter().rev() {
                        without_backslashes.remove(loc);
                    }

                    arena.alloc_string(unsafe {
                        String::from_utf8_unchecked(without_backslashes)
                    })
                };

                let string = if let Some(&interned_string) = intern.get(string) {
                    interned_string
                } else {
                    // TODO: Make own `interner` hashset reimplementation
                    intern.insert(arena, string);
                    string
                };

                *counter += 1;

                return Ok(Sexp::String(string));
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

                let string = unsafe { str::from_utf8_unchecked(&input[start..*counter]) };

                // We only advance if the bytes are in `.0123456789` so we know that it is valid
                // UTF8
                return if is_float {
                    f64::from_str(string)
                        .map(Sexp::Float)
                        .map_err(|_| format!("Invalid float: {:?}", string).into())
                } else {
                    i64::from_str(string)
                        .map(Sexp::Int)
                        .map_err(|_| format!("Invalid int: {:?}", string).into())
                };
            }
            // Quote
            b'\'' => {
                *counter += 1;

                let next_token = parse_inner(counter, arena, intern, input)?;

                return Ok(Sexp::Quote(arena.alloc(next_token)));
            }
            // MetaQuote
            b'`' => {
                *counter += 1;

                let next_token = parse_inner(counter, arena, intern, input)?;

                return Ok(Sexp::Metaquote(arena.alloc(next_token)));
            }
            // Unquote
            b',' => {
                *counter += 1;

                let next_token = parse_inner(counter, arena, intern, input)?;

                return Ok(Sexp::Unquote(arena.alloc(next_token)));
            }
            // Symbol
            a if is_identifier_start_char(a) => {
                let start = *counter;
                *counter += 1;

                while *counter < input.len() && is_identifier_char(input[*counter]) {
                    *counter += 1;
                }

                assert_end_of_token!();

                let string = unsafe { str::from_utf8_unchecked(&input[start..*counter]) };
                let string = if let Some(&interned_string) = intern.get(string) {
                    interned_string
                } else {
                    // TODO: Make own `interner` hashset reimplementation
                    intern.insert(arena, string);
                    string
                };

                // We know that it's a valid ascii string because we used `is_identifier_char`
                return Ok(Sexp::Symbol(string));
            }
            other => {
                return Err(format!(
                    "Unexpected token {:?} at character {}",
                    char::from(other),
                    counter
                ).into());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    extern crate memmap;
    extern crate test;

    use super::{is_identifier_char, is_identifier_start_char, parse, parse_many, Arena, Sexp};
    use self::test::Bencher;

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

    #[bench]
    fn parses_huge(b: &mut Bencher) {
        use self::memmap::Mmap;
        use std::fs::File;

        let map = unsafe {
            Mmap::map(&File::open(concat!(
                env!("CARGO_MANIFEST_DIR"),
                "/src/filetests/huge.lisp"
            )).expect("Can't open file"))
        }.expect("Can't open file");
        let slice = map.as_ref();

        b.iter(|| {
            let mut parser = parse_many(slice);

            loop {
                let arena = Arena::new();
                let mut intern = Default::default();

                if let Some(result) = parser.next(&arena, &mut intern) {
                    result.expect("Failed");
                } else {
                    break;
                }
            }
        });
    }

    #[test]
    fn parses_files() {
        let arena = Arena::new();
        let mut intern = Default::default();
        let files = [
            (
                &include_bytes!("./filetests/prime-decomposition.lisp")[..],
                3,
            ),
            (&include_bytes!("./filetests/combined.lisp")[..], 312),
        ];

        for &(file, expected_len) in &files {
            assert_eq!(
                parse_many(file)
                    .into_iter(&arena, &mut intern)
                    .map(|sexp| sexp.expect("Failed to parse"))
                    .count(),
                expected_len
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
                b";; This is a test comment
( set!( get-x-ref myStruct ; So is this
                        ) 2 \"test\\\\\" '() '(1 2 3))"
            ),
            Ok(Sexp::List(
                &[
                    Sexp::Symbol("set!"),
                    Sexp::List(
                        &[Sexp::Symbol("get-x-ref"), Sexp::Symbol("myStruct")],
                        &Sexp::Nil
                    ),
                    Sexp::Int(2),
                    Sexp::String("test\\"),
                    Sexp::Quote(&Sexp::Nil),
                    Sexp::Quote(&Sexp::List(
                        &[Sexp::Int(1), Sexp::Int(2), Sexp::Int(3)],
                        &Sexp::Nil
                    )),
                ],
                &Sexp::Nil,
            ))
        );
    }

    #[test]
    fn parses_string() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(
            parse(&arena, &mut intern, b"\"Test!\""),
            Ok(Sexp::String("Test!"))
        );

        assert_eq!(
            parse(&arena, &mut intern, b"\"(((}{{}{}{}}}}\""),
            Ok(Sexp::String("(((}{{}{}{}}}}"))
        );

        assert_eq!(
            parse(&arena, &mut intern, br#""\\\"\"\\""#),
            Ok(Sexp::String(r#"\""\"#))
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
        assert_eq!(
            parse(&arena, &mut intern, b"10-fails").map_err(|_| ()),
            Err(()),
        );
    }

    #[test]
    fn parses_numbers() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(parse(&arena, &mut intern, b"2"), Ok(Sexp::Int(2)),);
        assert_eq!(parse(&arena, &mut intern, b"10"), Ok(Sexp::Int(10)),);
        assert_eq!(parse(&arena, &mut intern, b"10."), Ok(Sexp::Float(10.)),);
        assert_eq!(parse(&arena, &mut intern, b"10.5"), Ok(Sexp::Float(10.5)),);
        assert_eq!(
            parse(&arena, &mut intern, b"10.5.6.7.8").map_err(|_| ()),
            Err(()),
        );
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

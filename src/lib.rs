#![feature(dotdoteq_in_patterns, inclusive_range_syntax, ascii_ctype, test,
           conservative_impl_trait)]

extern crate smallvec;
extern crate toolshed;

use toolshed::Arena;
use toolshed::map::Map;
use smallvec::SmallVec;

use std::fmt::{self, Display, Formatter};
use std::str::FromStr;
use std::borrow::Cow;
use std::marker::PhantomData;

type Intern<'arena> = Map<'arena, &'arena str, &'arena str>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Sexp<'arena, I = i64, F = f64> {
    Quote(&'arena Sexp<'arena>),
    Metaquote(&'arena Sexp<'arena>),
    Unquote(&'arena Sexp<'arena>),
    // The last element is usally nil
    List(&'arena [Sexp<'arena>], &'arena Sexp<'arena>),
    String(&'arena str),
    Symbol(&'arena str),
    Int(I),
    Float(F),
    Nil,
}

impl<'a, I: Display, F: Display> Display for Sexp<'a, I, F> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        use Sexp::*;

        match *self {
            Quote(el) => write!(f, "'{}", el),
            Metaquote(el) => write!(f, "`{}", el),
            Unquote(el) => write!(f, ",{}", el),
            List(xs, last) => {
                write!(f, "(")?;
                if let Some((first, rest)) = xs.split_first() {
                    write!(f, "{}", first)?;
                    for x in rest {
                        write!(f, " {}", x)?;
                    }
                }

                if *last != Nil {
                    write!(f, " . {}", last)?;
                }

                write!(f, ")")
            }
            String(val) => write!(f, "{:?}", val),
            Symbol(sym) => write!(f, "{}", sym),
            Int(ref n) => write!(f, "{}", n),
            Float(ref n) => write!(f, "{}", n),
            Nil => write!(f, "()"),
        }
    }
}

impl<'a, I, F> Sexp<'a, I, F> {
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

pub fn parse<
    'sexp,
    'intern: 'sexp,
    'mu,
    I: FromStr,
    F: FromStr,
    AP: Into<ArenaPair<'sexp, 'intern>>,
>(
    arena: AP,
    intern: &'mu mut Intern<'intern>,
    input: &'intern [u8],
) -> Result<Sexp<'sexp>, Cow<'static, str>> {
    parse_inner(&mut 0, arena, intern, input)
}

pub struct ParseManyIterator<'intern: 'sexp, 'sexp: 'mu, 'mu, I, F> {
    inner: ParseMany<'intern, I, F>,
    arena: ArenaPair<'sexp, 'intern>,
    intern: &'mu mut Intern<'intern>,
}

impl<'intern, 'im, 'mu, I: FromStr, F: FromStr> Iterator
    for ParseManyIterator<'intern, 'im, 'mu, I, F> {
    type Item = Result<Sexp<'im, I, F>, Cow<'static, str>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next(self.arena, &mut self.intern)
    }
}

pub struct ParseMany<'a, I, F> {
    counter: usize,
    err: bool,
    input: &'a [u8],
    _out_types: PhantomData<(I, F)>,
}

impl<'intern, I: FromStr, F: FromStr> ParseMany<'intern, I, F> {
    pub fn into_iter<'sexp, 'mu, AP: Into<ArenaPair<'sexp, 'intern>>>(
        self,
        arena: AP,
        intern: &'mu mut Map<'intern, &'intern str, &'intern str>,
    ) -> ParseManyIterator<'intern, 'sexp, 'mu, I, F>
    where
        'intern: 'sexp,
    {
        ParseManyIterator {
            inner: self,
            arena: arena.into(),
            intern,
        }
    }

    pub fn next<'sexp, 'mu, AP: Into<ArenaPair<'sexp, 'intern>>>(
        &mut self,
        arena: AP,
        intern: &'mu mut Intern<'intern>,
    ) -> Option<Result<Sexp<'sexp, I, F>, Cow<'static, str>>>
    where
        'intern: 'sexp,
    {
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

#[derive(Copy, Clone)]
pub struct ArenaPair<'im_sexp, 'im_intern>(&'im_sexp Arena, &'im_intern Arena);

impl<'a> From<&'a Arena> for ArenaPair<'a, 'a> {
    fn from(other: &'a Arena) -> Self {
        ArenaPair(other, other)
    }
}

impl<'a, 'b> From<(&'a Arena, &'b Arena)> for ArenaPair<'a, 'b> {
    fn from(other: (&'a Arena, &'b Arena)) -> Self {
        ArenaPair(other.0, other.1)
    }
}

pub fn parse_many<I, F>(input: &[u8]) -> ParseMany<I, F> {
    ParseMany {
        counter: 0,
        err: false,
        _out_types: PhantomData,
        input,
    }
}

fn parse_inner<
    'sexp,
    'mu,
    'intern: 'sexp,
    I: FromStr,
    F: FromStr,
    AP: Into<ArenaPair<'sexp, 'intern>>,
>(
    counter: &'mu mut usize,
    arenas: AP,
    intern: &'mu mut Map<'intern, &'intern str, &'intern str>,
    input: &'intern [u8],
) -> Result<Sexp<'sexp, I, F>, Cow<'static, str>> {
    use std::str;

    let arenas = arenas.into();
    let ArenaPair(sexp_arena, intern_arena) = arenas;

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

                let mut output: SmallVec<[Sexp; 16]> = SmallVec::new();
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

                        last = sexp_arena.alloc(parse_inner(counter, arenas, intern, input)?);

                        skip_whitespace!();

                        if input[*counter] == b')' {
                            *counter += 1;
                            break;
                        } else {
                            return Err("Invalid cons".into());
                        }
                    }

                    let current = parse_inner(counter, arenas, intern, input)?;
                    output.push(current);

                    skip_whitespace!();
                }

                if output.is_empty() {
                    return Ok(Sexp::Nil);
                } else {
                    let output = sexp_arena.alloc_many(if output.spilled() {
                        Cow::from(output.into_vec())
                    } else {
                        Cow::from(&output[..])
                    });

                    return Ok(Sexp::List(output, last));
                }
            }
            // String
            b'"' => {
                *counter += 1;
                let start = *counter;
                let mut backslash_locs = SmallVec::<[usize; 32]>::new();

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
                    let string = unsafe { str::from_utf8_unchecked(bytes) };
                    if let Some(interned_string) = intern.get(string) {
                        interned_string
                    } else {
                        let string = intern_arena.alloc_str(&string);
                        intern.insert(intern_arena, string, string);
                        string
                    }
                } else {
                    use std::ptr;

                    let string = unsafe { str::from_utf8_unchecked(bytes) };
                    if let Some(interned_string) = intern.get(&string[..]) {
                        interned_string
                    } else {
                        let mut without_backslashes = intern_arena.alloc_byte_slice_mut(bytes);

                        for &loc in backslash_locs.iter().rev() {
                            unsafe {
                                ptr::copy(
                                    &without_backslashes[loc + 1],
                                    &mut without_backslashes[loc],
                                    without_backslashes.len() - loc,
                                );
                            }
                        }

                        let without_backslashes = &without_backslashes
                            [..without_backslashes.len() - backslash_locs.len()];

                        let without_backslashes =
                            unsafe { str::from_utf8_unchecked(without_backslashes) };
                        intern.insert(intern_arena, string, without_backslashes);
                        without_backslashes
                    }
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
                    FromStr::from_str(string)
                        .map(Sexp::Float)
                        .map_err(|_| format!("Invalid float: {:?}", string).into())
                } else {
                    FromStr::from_str(string)
                        .map(Sexp::Int)
                        .map_err(|_| format!("Invalid int: {:?}", string).into())
                };
            }
            // Quote
            b'\'' => {
                *counter += 1;

                let next_token = parse_inner(counter, arenas, intern, input)?;

                return Ok(Sexp::Quote(sexp_arena.alloc(next_token)));
            }
            // MetaQuote
            b'`' => {
                *counter += 1;

                let next_token = parse_inner(counter, arenas, intern, input)?;

                return Ok(Sexp::Metaquote(sexp_arena.alloc(next_token)));
            }
            // Unquote
            b',' => {
                *counter += 1;

                let next_token = parse_inner(counter, arenas, intern, input)?;

                return Ok(Sexp::Unquote(sexp_arena.alloc(next_token)));
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
                let string = if let Some(interned_string) = intern.get(string) {
                    interned_string
                } else {
                    intern.insert(intern_arena, string, string);
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

    #[test]
    fn displays_correctly() {
        let cases = [
            "((hello 2 5) world . 10)",
            "('(hello 2 5) 'world . '10)",
            "(`(,hello 2 5) `world . `10)",
        ];
        let arena = Arena::new();
        let mut intern = Default::default();

        for case in &cases {
            assert_eq!(
                &format!(
                    "{}",
                    parse::<i64, f64, _>(&arena, &mut intern, case.as_bytes())
                        .expect("Formatting failed")
                ),
                case
            );
        }
    }

    #[bench]
    fn parses_huge(b: &mut Bencher) {
        use std::fs::File;
        use std::io::Read;

        let mut out = vec![];
        File::open(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/filetests/huge.lisp"
        )).expect("Can't open file")
            .read_to_end(&mut out)
            .expect("Failed");
        let slice = out.as_ref();

        let intern_arena = Arena::new();
        let mut intern = Default::default();
        let sexp_arena = Arena::new();

        b.iter(|| {
            let mut parser = parse_many::<i64, f64>(slice);

            loop {
                if let Some(result) = parser.next((&sexp_arena, &intern_arena), &mut intern) {
                    result.expect("Failed");
                } else {
                    break;
                }

                unsafe { sexp_arena.clear() };
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
                parse_many::<i64, f64>(file)
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
            parse::<i64, f64, _>(
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
            parse::<i64, f64, _>(&arena, &mut intern, b"\"Test!\""),
            Ok(Sexp::String("Test!"))
        );

        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"\"(((}{{}{}{}}}}\""),
            Ok(Sexp::String("(((}{{}{}{}}}}"))
        );

        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, br#""\\\"\"\\""#),
            Ok(Sexp::String(r#"\""\"#))
        );
    }

    #[test]
    fn parses_ident() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"testing-a-thing"),
            Ok(Sexp::Symbol("testing-a-thing")),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"has-number-10"),
            Ok(Sexp::Symbol("has-number-10")),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"10-fails").map_err(|_| ()),
            Err(()),
        );
    }

    #[test]
    fn parses_numbers() {
        let arena = Arena::new();
        let mut intern = Default::default();

        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"2"),
            Ok(Sexp::Int(2)),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"10"),
            Ok(Sexp::Int(10)),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"10."),
            Ok(Sexp::Float(10.)),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"10.5"),
            Ok(Sexp::Float(10.5)),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"10.5.6.7.8").map_err(|_| ()),
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

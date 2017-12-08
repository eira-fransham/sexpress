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
    UnquoteSplicing(&'arena Sexp<'arena>),
    // The last element is usally nil
    List(&'arena [Sexp<'arena>], &'arena Sexp<'arena>),
    Bool(bool),
    Char(char),
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
            UnquoteSplicing(el) => write!(f, ",{}", el),
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
            Bool(b) => write!(f, r"#{}", if b { 't' } else { 'f' }),
            Char(c) => write!(f, r"#\{}", c),
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
        between_inclusive(c, b'<', b'Z') || between_inclusive(c, b'$', b'&') || c == b'/'
        || c == b'*' || c == b'~' || c == b'_' || c == b'^' || c == b':' || c == b'!'
}

fn is_bracket(c: u8) -> bool {
    c == b'[' || c == b']' || c == b'(' || c == b')' || c == b'{' || c == b'}'
}

fn is_identifier_char(c: u8) -> bool {
    is_identifier_start_char(c) || c == b'.' || c == b'#' || c == b'+' || c == b'-'
        || between_inclusive(c, b'0', b'9')
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

    macro_rules! is_end_of_token {
        ($counter:expr) => {
            $counter >= input.len() ||
                input[$counter].is_ascii_whitespace() ||
                is_bracket(input[$counter])
        }
    }

    macro_rules! assert_end_of_token {
        () => {
            if !is_end_of_token!(*counter) {
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

    macro_rules! make_ident {
        ($start:expr, $end:expr) => {
            {
                let string = str::from_utf8_unchecked(&input[$start..$end]);

                Ok(Sexp::Symbol(string))
            }
        }
    }

    if *counter >= input.len() {
        return Err(format!("Unexpected EOF at {}", counter).into());
    }

    skip_whitespace!();

    static TRIPLE_DOT: &[u8] = b"...";

    match input[*counter] {
        b'.' => {
            if input[*counter..*counter + TRIPLE_DOT.len()].eq_ignore_ascii_case(TRIPLE_DOT) {
                *counter += TRIPLE_DOT.len();
                Ok(Sexp::Symbol(unsafe {
                    str::from_utf8_unchecked(TRIPLE_DOT)
                }))
            } else {
                Err("Unexpected `.`".into())
            }
        }
        // String
        b'"' => {
            *counter += 1;
            let start = *counter;
            let mut backslash_locs = SmallVec::<[(usize, Option<u8>); 32]>::new();

            while *counter < input.len() && input[*counter] != b'"' {
                let cur = input[*counter];
                if !cur.is_ascii() {
                    return Err("Non-ascii character in string".into());
                } else if cur.is_ascii_control() && !(cur == b'\n' || cur == b'\r') {
                    return Err("Ascii control character in string".into());
                } else if cur == b'\\' {
                    // TODO: Handle this properly
                    backslash_locs.push((
                        *counter - start,
                        if input[*counter + 1] == b'n' {
                            Some(b'\n')
                        } else if input[*counter] == b'"' || input[*counter] == b'\\' {
                            None
                        } else {
                            return Err(format!(
                                "Invalid escape: \\{}",
                                std::char::from_u32(input[*counter] as _)
                                    .expect("We check non-ASCII above, Q.E.D.")
                            ).into());
                        },
                    ));

                    *counter += 1;
                }

                *counter += 1;
            }

            let bytes = &input[start..*counter];

            let string = if backslash_locs.is_empty() {
                unsafe { str::from_utf8_unchecked(bytes) }
            } else {
                use std::ptr;

                let string = unsafe { str::from_utf8_unchecked(bytes) };
                if let Some(interned_string) = intern.get(&string[..]) {
                    interned_string
                } else {
                    let mut without_backslashes = intern_arena.alloc_byte_slice_mut(bytes);

                    for &(loc, replace) in backslash_locs.iter().rev() {
                        unsafe {
                            ptr::copy(
                                &without_backslashes[loc + 1],
                                &mut without_backslashes[loc],
                                without_backslashes.len() - loc,
                            );
                        }

                        if let Some(replace) = replace {
                            without_backslashes[loc] = replace;
                        }
                    }

                    let without_backslashes =
                        &without_backslashes[..without_backslashes.len() - backslash_locs.len()];

                    let without_backslashes =
                        unsafe { str::from_utf8_unchecked(without_backslashes) };
                    intern.insert(intern_arena, string, without_backslashes);
                    without_backslashes
                }
            };

            *counter += 1;

            Ok(Sexp::String(string))
        }
        // Char/boolean/vector
        b'#' => {
            *counter += 1;

            static NEWLINE_STR: &[u8] = b"newline";
            static SPACE_STR: &[u8] = b"space";

            match input[*counter] {
                b'\\' => {
                    use std::char;

                    *counter += 1;

                    if input[*counter..*counter + NEWLINE_STR.len()]
                        .eq_ignore_ascii_case(NEWLINE_STR)
                    {
                        *counter += NEWLINE_STR.len();
                        assert_end_of_token!();
                        Ok(Sexp::Char('\n'))
                    } else if input[*counter..*counter + SPACE_STR.len()]
                        .eq_ignore_ascii_case(SPACE_STR)
                    {
                        *counter += SPACE_STR.len();
                        assert_end_of_token!();
                        Ok(Sexp::Char(' '))
                    } else if input[*counter].is_ascii_whitespace() {
                        Err("Empty character escape".into())
                    } else {
                        let c = input[*counter];

                        *counter += 1;

                        assert_end_of_token!();

                        char::from_u32(c as _)
                            .map(Sexp::Char)
                            .ok_or_else(|| format!("Non-ASCII character with code {}", c).into())
                    }
                }
                b'(' => {
                    unimplemented!();
                }
                b't' => {
                    *counter += 1;
                    Ok(Sexp::Bool(true))
                }
                b'f' => {
                    *counter += 1;
                    Ok(Sexp::Bool(false))
                }
                other => Err(format!(
                    "Invalid character following hash: {:?}",
                    ::std::char::from_u32(other as _)
                ).into()),
            }
        }
        // Quote
        b'\'' => {
            *counter += 1;

            let next_token = parse_inner(counter, arenas, intern, input)?;

            Ok(Sexp::Quote(sexp_arena.alloc(next_token)))
        }
        // MetaQuote
        b'`' => {
            *counter += 1;

            let next_token = parse_inner(counter, arenas, intern, input)?;

            Ok(Sexp::Metaquote(sexp_arena.alloc(next_token)))
        }
        // Unquote
        b',' => {
            *counter += 1;

            let make_sexp = if input[*counter] == b'@' {
                *counter += 1;

                Sexp::UnquoteSplicing
            } else {
                Sexp::Unquote
            };

            let next_token = parse_inner(counter, arenas, intern, input)?;

            Ok(make_sexp(sexp_arena.alloc(next_token)))
        }
        // Cons
        a @ b'(' | a @ b'[' => {
            let matching = if a == b'(' { b')' } else { b']' };

            *counter += 1;

            skip_whitespace!();

            let mut output: SmallVec<[Sexp; 16]> = SmallVec::new();
            let mut last = &NIL;

            loop {
                if *counter >= input.len() {
                    return Err("Unexpected EOF".into());
                } else if input[*counter] == matching {
                    *counter += 1;
                    break;
                } else if input[*counter] == b'.' && input[*counter + 1] != b'.' {
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
                Ok(Sexp::Nil)
            } else {
                let output = sexp_arena.alloc_many(if output.spilled() {
                    Cow::from(output.into_vec())
                } else {
                    Cow::from(&output[..])
                });

                Ok(Sexp::List(output, last))
            }
        }
        // Symbol
        a if is_identifier_start_char(a) => {
            let start = *counter;
            *counter += 1;

            while *counter < input.len() && is_identifier_char(input[*counter]) {
                *counter += 1;
            }

            assert_end_of_token!();

            unsafe { make_ident!(start, *counter) }
        }
        // Number (float or int)
        b'0'..=b'9' | b'+' | b'-' => {
            let mut is_float = false;
            let start = *counter;

            *counter += 1;

            while *counter < input.len() {
                if input[*counter] == b'.' {
                    is_float = true;
                    *counter += 1;
                } else if between_inclusive(input[*counter], b'0', b'9') {
                    *counter += 1;
                } else if is_identifier_char(input[*counter]) {
                    // Yeah, we have to do this ident hack because `1+` and `1-` are too common as
                    // functions in scheme (even though the spec specifically disallows idents that
                    // could be mistaken for numbers, only explicitly allowing `+` and `-`)

                    while *counter < input.len() && is_identifier_char(input[*counter]) {
                        *counter += 1;
                    }

                    assert_end_of_token!();

                    return unsafe { make_ident!(start, *counter) };
                } else {
                    break;
                }
            }

            assert_end_of_token!();

            let string = unsafe { str::from_utf8_unchecked(&input[start..*counter]) };

            static PLUS: &str = "+";
            static MINUS: &str = "-";

            if string == PLUS {
                return Ok(Sexp::Symbol(PLUS));
            } else if string == MINUS {
                return Ok(Sexp::Symbol(MINUS));
            }

            // We only advance if the bytes are in `.0123456789` so we know that it is valid
            // UTF8
            if is_float {
                FromStr::from_str(string)
                    .map(Sexp::Float)
                    .map_err(|_| format!("Invalid float: {:?}", string).into())
            } else {
                FromStr::from_str(string)
                    .map(Sexp::Int)
                    .map_err(|_| format!("Invalid int: {:?}", string).into())
            }
        }
        other => Err(format!(
            "Unexpected token {:?} at character {}",
            char::from(other),
            counter
        ).into()),
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
                .chain(b"!@$%^&*_=<>?:~/".into_iter().cloned())
        }
    }

    macro_rules! ident_rest {
        () => {
            ident_start!().chain([b'.', b'#', b'+', b'-'].iter().cloned()).chain(b'0'..=b'9')
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

        b.iter(|| {
            let mut parser = parse_many::<i64, f64>(slice);
            let sexp_arena = Arena::new();

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
            (&include_bytes!("./filetests/combined.lisp")[..], 236),
        ];

        println!("{}", unsafe { ::std::str::from_utf8_unchecked(&files[1].0[88179 - 200..88179 + 200]) });

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
            parse::<i64, f64, _>(&arena, &mut intern, b"+1.0+").map_err(|_| ()),
            Ok(Sexp::Symbol("+1.0+")),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"+").map_err(|_| ()),
            Ok(Sexp::Symbol("+")),
        );
        assert_eq!(
            parse::<i64, f64, _>(&arena, &mut intern, b"-").map_err(|_| ()),
            Ok(Sexp::Symbol("-")),
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

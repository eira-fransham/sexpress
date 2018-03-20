#![feature(ascii_ctype, test, conservative_impl_trait)]

extern crate smallvec;
extern crate toolshed;

use smallvec::SmallVec;

use toolshed::Arena;
use std::fmt::{self, Display, Formatter};
use std::str::FromStr;
use std::borrow::Cow;
use std::marker::PhantomData;

#[macro_export]
macro_rules! sexp {
    ({@sym $i:expr}) => {
        $crate::Sexp::Symbol($i)
    };
    ({@str $i:expr}) => {
        $crate::Sexp::String($i)
    };
    ({@int $i:expr}) => {
        $crate::Sexp::Int($i)
    };
    ({@flt $i:expr}) => {
        $crate::Sexp::Float($i)
    };
    ({@chr $i:expr}) => {
        $crate::Sexp::Char($i)
    };
    // For if the other parsing is ambiguous
    ({$($other:tt)*}) => {
        sexp!($($other)*)
    };
    (()) => {
        $crate::Sexp::Nil($crate::BracketStyle::Round)
    };
    ([]) => {
        $crate::Sexp::Nil($crate::BracketStyle::Square)
    };
    ((quote $inner:tt)) => {
        $crate::Sexp::Quote(&sexp!($inner))
    };
    ((metaquote $inner:tt)) => {
        $crate::Sexp::Metaquote(&sexp!($inner))
    };
    ((unquote $inner:tt)) => {
        $crate::Sexp::Unquote(&sexp!($inner))
    };
    ((unquote-splicing $inner:tt)) => {
        $crate::Sexp::UnquoteSplicing(&sexp!($inner))
    };
    ((cons $({$first:tt})* . $last:tt)) => {
        $crate::Sexp::List($crate::BracketStyle::Round, &[$(sexp!({$inner}),)*], &sexp!($last))
    };
    ([cons $({$first:tt})* . $last:tt]) => {
        $crate::Sexp::List($crate::BracketStyle::Square, &[$(sexp!({$inner}),)*], &sexp!($last))
    };
    (($($inner:tt)+)) => {
        $crate::Sexp::List($crate::BracketStyle::Round, &[$(sexp!($inner),)*], &$crate::NIL)
    };
    ([$($inner:tt)+]) => {
        $crate::Sexp::List($crate::BracketStyle::Square, &[$(sexp!($inner),)*], &$crate::NIL)
    };
}

#[cfg(feature = "debug-output")]
macro_rules! debug {
    (@preamble) => {{
        use std::sync::atomic::{AtomicUsize, Ordering};
        static DEBUG: AtomicUsize = AtomicUsize::new(0);
        print!(
            "{:04} {}:{},{}",
            DEBUG.fetch_add(1, Ordering::Relaxed),
            file!(),
            line!(),
            column!(),
        );
    }};
    () => {
        debug!(@preamble);
        println!();
    };
    ($first:expr $(, $rest:expr)* $(,)*) => {
        debug!(@preamble);
        print!(" (");
        print!("{} = {:?}", stringify!($first), $first);
        $(print!(", {} = {:?}", stringify!($rest), $rest);)*
        print!(")");
        println!();
    };
}

#[cfg(not(feature = "debug-output"))]
macro_rules! debug {
    ($($any:tt)*) => {}
}

// TODO: Braces
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BracketStyle {
    Round,
    Square,
}

impl BracketStyle {
    #[inline]
    pub fn from_open_unchecked(open: u8) -> Self {
        match open {
            b'(' => BracketStyle::Round,
            _ => BracketStyle::Square,
        }
    }

    #[inline]
    pub fn chars(&self) -> (char, char) {
        match *self {
            BracketStyle::Round => ('(', ')'),
            BracketStyle::Square => ('[', ']'),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Sexp<'arena, I = i64, F = f64> {
    Quote(&'arena Sexp<'arena>),
    Metaquote(&'arena Sexp<'arena>),
    Unquote(&'arena Sexp<'arena>),
    UnquoteSplicing(&'arena Sexp<'arena>),
    // The last element is usally nil
    List(BracketStyle, &'arena [Sexp<'arena>], &'arena Sexp<'arena>),
    Bool(bool),
    Char(char),
    String(&'arena str),
    Symbol(&'arena str),
    Int(I),
    Float(F),
    Nil(BracketStyle),
}

impl<'a, I: Display, F: Display> Display for Sexp<'a, I, F> {
    fn fmt(&self, f: &mut Formatter) -> Result<(), fmt::Error> {
        use Sexp::*;

        match *self {
            Quote(el) => write!(f, "'{}", el),
            Metaquote(el) => write!(f, "`{}", el),
            Unquote(el) => write!(f, ",{}", el),
            UnquoteSplicing(el) => write!(f, ",@{}", el),
            List(style, xs, last) => {
                let (open, close) = style.chars();
                try!(write!(f, "{}", open));
                if let Some((first, rest)) = xs.split_first() {
                    try!(write!(f, "{}", first));
                    for x in rest {
                        try!(write!(f, " {}", x));
                    }
                }

                if *last != NIL {
                    try!(write!(f, " . {}", last));
                }

                write!(f, "{}", close)
            }
            Bool(b) => write!(f, r"#{}", if b { 't' } else { 'f' }),
            Char(c) => write!(f, r"#\{}", c),
            String(val) => write!(f, "{:?}", val),
            Symbol(sym) => write!(f, "{}", sym),
            Int(ref n) => write!(f, "{}", n),
            Float(ref n) => write!(f, "{}", n),
            Nil(style) => {
                let (open, close) = style.chars();
                write!(f, "{}{}", open, close)
            }
        }
    }
}

impl<'a, I, F> Sexp<'a, I, F> {
    pub fn len(&self) -> Option<usize> {
        match *self {
            Sexp::List(_, arr, el) => Some(arr.len() + el.len().unwrap_or(0)),
            _ => None,
        }
    }
}

#[inline(always)]
fn between_inclusive(a: u8, b: u8, c: u8) -> bool {
    a.wrapping_sub(b) <= c.wrapping_sub(b)
}

#[inline(always)]
fn is_identifier_start_char(c: u8) -> bool {
    between_inclusive(c, b'a', b'z') ||
        // We start at < to get <=>?@
        between_inclusive(c, b'<', b'Z') || between_inclusive(c, b'$', b'&') || c == b'/'
        || c == b'*' || c == b'~' || c == b'_' || c == b'^' || c == b':' || c == b'!'
}

#[inline(always)]
fn is_bracket(c: u8) -> bool {
    c == b'[' || c == b']' || c == b'(' || c == b')' || c == b'{' || c == b'}'
}

#[inline(always)]
fn is_identifier_char(c: u8) -> bool {
    is_identifier_start_char(c) || c == b'.' || c == b'#' || c == b'+' || c == b'-'
        || between_inclusive(c, b'0', b'9')
}

static NIL: Sexp = Sexp::Nil(BracketStyle::Round);

pub fn parse<'a, I: FromStr, F: FromStr>(
    arena: &'a Arena,
    input: &'a [u8],
) -> Result<Sexp<'a>, Cow<'static, str>> {
    parse_inner(&mut 0, &mut Default::default(), arena, input)
}

pub struct ParseManyIterator<'a, I, F> {
    inner: ParseMany<'a, I, F>,
    arena: &'a Arena,
}

impl<'a, I: FromStr, F: FromStr> Iterator for ParseManyIterator<'a, I, F> {
    type Item = Result<Sexp<'a, I, F>, Cow<'static, str>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next(self.arena)
    }
}

pub struct ParseMany<'a, I, F> {
    counter: usize,
    err: bool,
    input: &'a [u8],
    cache: Vec<usize>,
    _out_types: PhantomData<(I, F)>,
}

impl<'input, I: FromStr, F: FromStr> ParseMany<'input, I, F> {
    pub fn into_iter<'a>(self, arena: &'a Arena) -> ParseManyIterator<'a, I, F>
    where
        'input: 'a,
    {
        ParseManyIterator {
            inner: self,
            arena: arena,
        }
    }

    pub fn next<'a>(
        &mut self,
        arena: &'a Arena,
    ) -> Option<Result<Sexp<'a, I, F>, Cow<'static, str>>>
    where
        'input: 'a,
    {
        while self.counter < self.input.len() && self.input[self.counter].is_ascii_whitespace() {
            self.counter += 1;
        }

        if self.err || self.counter >= self.input.len() {
            None
        } else {
            match parse_inner(&mut self.counter, &mut self.cache, arena, self.input) {
                Ok(sexp) => Some(Ok(sexp)),
                Err(err) => {
                    self.err = true;

                    Some(Err(err))
                }
            }
        }
    }
}

#[inline]
pub fn parse_many<I, F>(input: &[u8]) -> ParseMany<I, F> {
    ParseMany {
        counter: 0,
        err: false,
        cache: Default::default(),
        _out_types: PhantomData,
        input,
    }
}

fn parse_inner<'a, 'mu, I: FromStr, F: FromStr>(
    counter: &'mu mut usize,
    loc_cache: &mut Vec<usize>,
    arena: &'a Arena,
    input: &'a [u8],
) -> Result<Sexp<'a, I, F>, Cow<'static, str>> {
    use std::str;

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

    static PLUS: &str = "+";
    static MINUS: &str = "-";

    match input[*counter] {
        // String
        b'"' => {
            const VALID_ESCAPES: &[u8] = &[b'\'', b'"', b'n', b'\\'];

            *counter += 1;
            let start = *counter;

            let escape_locs = loc_cache;

            while *counter < input.len() && input[*counter] != b'"' {
                let cur = input[*counter];
                if !cur.is_ascii() {
                    return Err("Non-ascii character in string".into());
                } else if cur.is_ascii_control() && cur != b'\n' && cur != b'\r' {
                    return Err("Ascii control character in string".into());
                } else if cur == b'\\' {
                    *counter += 1;

                    escape_locs.push(*counter - start);

                    if !VALID_ESCAPES.contains(&input[*counter]) {
                        return Err(format!(
                            "Invalid escape: \\{}",
                            std::char::from_u32(input[*counter] as _)
                                .expect("We check non-ASCII above, Q.E.D.")
                        ).into());
                    }
                }

                *counter += 1;
            }

            let bytes = &input[start..*counter];

            let string = if escape_locs.is_empty() {
                unsafe { str::from_utf8_unchecked(bytes) }
            } else {
                use std::ptr;

                debug!();
                let mut without_backslashes = arena.require(bytes.len());

                let mut last_loc = 0;
                let mut out_loc = 0;

                for &loc in escape_locs.iter() {
                    let copy_len = loc - last_loc - 1;
                    unsafe {
                        ptr::copy(
                            &bytes[last_loc],
                            without_backslashes.offset(out_loc as _),
                            copy_len,
                        );
                    }

                    out_loc += copy_len;

                    if bytes[loc] == b'n' {
                        unsafe { *without_backslashes.offset(out_loc as _) = b'\n' };
                    } else {
                        unsafe { *without_backslashes.offset(out_loc as _) = bytes[loc] };
                    }

                    out_loc += 1;

                    last_loc = loc + 1;
                }

                escape_locs.clear();

                let without_backslashes =
                    unsafe { std::slice::from_raw_parts(without_backslashes, out_loc) };

                let without_backslashes = unsafe { str::from_utf8_unchecked(without_backslashes) };
                without_backslashes
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

                    if &input[*counter..*counter + NEWLINE_STR.len()] == NEWLINE_STR {
                        *counter += NEWLINE_STR.len();
                        assert_end_of_token!();
                        Ok(Sexp::Char('\n'))
                    } else if &input[*counter..*counter + SPACE_STR.len()] == SPACE_STR {
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

            let next_token = try!(parse_inner(counter, loc_cache, arena, input));

            debug!();
            Ok(Sexp::Quote(arena.alloc(next_token)))
        }
        // MetaQuote
        b'`' => {
            *counter += 1;

            let next_token = try!(parse_inner(counter, loc_cache, arena, input));

            debug!();
            Ok(Sexp::Metaquote(arena.alloc(next_token)))
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

            let next_token = try!(parse_inner(counter, loc_cache, arena, input));

            debug!();
            Ok(make_sexp(arena.alloc(next_token)))
        }
        // Cons
        a @ b'(' | a @ b'[' => {
            let style = BracketStyle::from_open_unchecked(a);
            let (_, matching) = style.chars();
            let matching = matching as u8;

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
                } else if input[*counter] == b'.' && is_end_of_token!(*counter + 1) {
                    *counter += 1;

                    assert_end_of_token!();
                    skip_whitespace!();

                    debug!();
                    last = arena.alloc(try!(parse_inner(counter, loc_cache, arena, input)));

                    skip_whitespace!();

                    if is_bracket(input[*counter]) {
                        *counter += 1;
                        break;
                    } else {
                        debug!(counter, unsafe {
                            str::from_utf8_unchecked(&input[*counter - 20..*counter + 20])
                        });
                        return Err("Invalid cons".into());
                    }
                }

                let current = try!(parse_inner(counter, loc_cache, arena, input));
                output.push(current);

                skip_whitespace!();
            }

            if output.is_empty() {
                Ok(Sexp::Nil(style))
            } else {
                debug!(output);
                let output = arena.alloc_many(if output.spilled() {
                    Cow::from(output.into_vec())
                } else {
                    Cow::from(&output[..])
                });

                Ok(Sexp::List(style, output, last))
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
        b'+' if is_end_of_token!(*counter + 1) => {
            *counter += 1;
            Ok(Sexp::Symbol(PLUS))
        }
        b'-' if is_end_of_token!(*counter + 1) => {
            *counter += 1;
            Ok(Sexp::Symbol(MINUS))
        }
        // Number (float or int)
        b'.' | b'0'..=b'9' | b'+' | b'-' => {
            let mut is_float = input[*counter] == b'.';
            let start = *counter;

            *counter += 1;

            while *counter < input.len() {
                if input[*counter] == b'.' && !is_float {
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

                    return if &input[start..*counter] == b"." {
                        Err("Unexpected `.`".into())
                    } else {
                        // We check `is_identifier_char` so this is safe.
                        unsafe { make_ident!(start, *counter) }
                    };
                } else {
                    break;
                }
            }

            assert_end_of_token!();

            // We only advance if the bytes are in `.0123456789` so we know that it is valid
            // UTF8
            let string = unsafe { str::from_utf8_unchecked(&input[start..*counter]) };

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
        let test_string = r#"
(([hello 2 5] '`[] world . 10)
 ('(hello 2 5) [0 . [1 . (2 . 3)]] 'world . '10)
 (`(,hello 2 5) `world . `10)
 (`(,@(test 1 2 3) ,@'() 5) `world . `10))
"#;
        let arena = Arena::new();

        assert_eq!(
            format!(
                "{}",
                parse::<i64, f64>(&arena, test_string.as_bytes()).expect("Formatting failed")
            ),
            test_string.replace('\n', " ").replace("  ", " ").trim()
        );
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

        b.iter(|| {
            let mut parser = parse_many::<i64, f64>(slice);
            let arena = Arena::new();

            loop {
                if let Some(result) = parser.next(&arena) {
                    result.expect("Failed");
                } else {
                    break;
                }

                unsafe { arena.clear() };
            }
        });
    }

    #[bench]
    fn parses_huge_string(b: &mut Bencher) {
        use std::fs::File;
        use std::io::Read;

        let mut out = vec![];
        File::open(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/filetests/huge-string.lisp"
        )).expect("Can't open file")
            .read_to_end(&mut out)
            .expect("Failed");
        let slice = out.as_ref();

        b.iter(|| {
            let mut parser = parse_many::<i64, f64>(slice);
            let arena = Arena::new();

            loop {
                if let Some(result) = parser.next(&arena) {
                    result.expect("Failed");
                } else {
                    break;
                }

                unsafe { arena.clear() };
            }
        });
    }
    #[bench]
    fn display_huge(b: &mut Bencher) {
        use std::fs::File;
        use std::io::{self, Read, Write};

        let mut out = vec![];
        File::open(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/src/filetests/huge.lisp"
        )).expect("Can't open file")
            .read_to_end(&mut out)
            .expect("Failed");
        let slice = out.as_ref();
        let parser = parse_many::<i64, f64>(slice);
        let arena = Arena::new();

        let massive = parser
            .into_iter(&arena)
            .collect::<Result<Vec<_>, _>>()
            .expect("Parsing failed");

        let mut out = io::sink();

        b.iter(|| {
            for i in &massive {
                let _ = writeln!(out, "{}", i);
            }
        });
    }

    #[test]
    fn parses_files() {
        let arena = Arena::new();
        let files = [
            (
                &include_bytes!("./filetests/prime-decomposition.lisp")[..],
                3,
            ),
            (&include_bytes!("./filetests/combined.lisp")[..], 236),
        ];

        for &(file, expected_len) in &files {
            assert_eq!(
                parse_many::<i64, f64>(file)
                    .into_iter(&arena)
                    .map(|sexp| sexp.expect("Failed to parse"))
                    .count(),
                expected_len
            );
        }
    }

    #[test]
    fn parses_everything() {
        let arena = Arena::new();

        assert_eq!(
            parse::<i64, f64>(
                &arena,
                b";; This is a test comment
( set!( get-x-ref myStruct ; So is this
                        ) 2 \"test\\\\\" '() '(1 2 3))"
            ),
            Ok(sexp!(
                ({@sym "set!"} ({@sym "get-x-ref"}
                                {@sym "myStruct"})
                 {@int 2}
                 {@str "test\\"}
                 (quote ())
                 (quote ({@int 1}
                         {@int 2}
                         {@int 3})))
            ))
        );
    }

    #[test]
    fn parses_string() {
        let arena = Arena::new();

        assert_eq!(
            parse::<i64, f64>(&arena, b"\"Test!\""),
            Ok(Sexp::String("Test!"))
        );

        assert_eq!(
            parse::<i64, f64>(&arena, b"\"(((}{{}{}{}}}}\""),
            Ok(Sexp::String("(((}{{}{}{}}}}"))
        );

        assert_eq!(
            parse::<i64, f64>(&arena, br#""\\\"\"\\\n""#),
            Ok(Sexp::String("\\\"\"\\\n"))
        );
    }

    #[test]
    fn parses_ident() {
        let arena = Arena::new();

        assert_eq!(
            parse::<i64, f64>(&arena, b"testing-a-thing"),
            Ok(Sexp::Symbol("testing-a-thing")),
        );
        assert_eq!(
            parse::<i64, f64>(&arena, b"has-number-10"),
            Ok(Sexp::Symbol("has-number-10")),
        );
        assert_eq!(
            parse::<i64, f64>(&arena, b"+1.0+"),
            Ok(Sexp::Symbol("+1.0+")),
        );
        assert_eq!(
            parse::<i64, f64>(&arena, b"1.2.3.4"),
            Ok(Sexp::Symbol("1.2.3.4")),
        );
        assert_eq!(parse::<i64, f64>(&arena, b"+"), Ok(Sexp::Symbol("+")),);
        assert_eq!(parse::<i64, f64>(&arena, b"-"), Ok(Sexp::Symbol("-")),);
    }

    #[test]
    fn parses_bool() {
        let arena = Arena::new();

        assert_eq!(parse::<i64, f64>(&arena, b"#t"), Ok(Sexp::Bool(true)),);
        assert_eq!(parse::<i64, f64>(&arena, b"#f"), Ok(Sexp::Bool(false)),);
    }

    #[test]
    fn parses_numbers() {
        let arena = Arena::new();

        assert_eq!(parse::<i64, f64>(&arena, b"2"), Ok(Sexp::Int(2)),);
        assert_eq!(parse::<i64, f64>(&arena, b"10"), Ok(Sexp::Int(10)),);
        assert_eq!(parse::<i64, f64>(&arena, b"10."), Ok(Sexp::Float(10.)),);
        assert_eq!(parse::<i64, f64>(&arena, b"10.5"), Ok(Sexp::Float(10.5)),);
        assert!(
            parse::<i64, f64>(
                &arena,
                b"10000000000000000000000000000000000000000000000000000000",
            ).is_err()
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

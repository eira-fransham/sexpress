# Sexpress

Seductively fast s-expression parser.

A basic parser for s-expressions that can handle quotes, metaquotes/unquote/
unquote-splicing, strings, atoms, chars and numbers (64-bit by default but you
can plug in bignums if you like). It doesn't currently handle complex escape
sequences, it only allows escaping double quotes, backslashes and newlines and
fails on anything else. In general it should parse any scheme code that doesn't
use syntax objects or vectors.

Almost everything is done in-place and no allocation is done if possible. All
elements are stored in an arena so allocation is simply bumping a pointer for
most cases. Strings and symbols are interned (strings with backslashes are
interned before escaping so no allocation is done if the backslash'd string is
already interned) to reduce memory use and allocation frequency. Lists are
stored as a pair, with the first element being all list elements except the last
one and the second element being the `cdr` of the innermost cons cell. For
example, `(1 2 3 4 5)` is equivalent to `(1 . (2 . (3 . (4 . (5 . ())))))`, but
is stored as `([1, 2, 3, 4, 5], nil)`, and `(1 2 . 3)` is equivalent to `(1 . (2
. 3))` but is stored as `([1, 2], 3)`. This gives excellent cache efficiency and
reduces the load on the allocator compared to a linked list. The array is built
in a `SmallVec` before being copied onto the heap, so lists of a size up to 16
are built on the stack instead of the heap. Another alternative would be to have
a single `Vec` that gets reused, which would give better worst-case performance,
but it's more complexity for unsure gain.

The throughput of the parser is approximately 130mb/s when reading from memory,
the only other s-expression parser that I could find that prided itself on speed
and that also had comparable benchmarks (the also-filthily-named
[sexpistol](https://github.com/aarongough/sexpistol)) reached 4.1mb/s.

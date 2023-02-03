You need better trace visibility for debugging. Rather than functions being
parsers, have parser builders that take some debuggable thing to print in the
tracer along with the function.

Right now you have some dangerous combos, e.g. count(any(thing)) will never
finish. You should automatically detect that kind of cycle and kill it (and
hopefully warn at build time????).

Whoa, there's some *very* weird behavior with the parser: it isn't
whitespace-aware, so mashing operators together into a single string works!
Maybe the `ascii_str` function should have a way of ensuring it's the end of
the string... `peek(whitespace())` perhaps? Probably don't build that in
automatically, make it part of what an `operator` function does. Same for IDs,
but not for keys in dictionaries. Actually, you need some kind of `trailers()`
parser, since e.g. `}>]),:` are all valid trailing characters.

I think you can significantly improve performance by rewriting ErrorCollector.
It seems generally better to optimize for parsing correct files rather than
error reporting; it's ok if the error report is some number of ms slower, if we
can parse large trees of correct code quickly (since most code will be correct,
or at least correct from a parsing standpoint). Rather than constantly
allocating and copying `Vec`s, instead you could have a series of Boxes, with
each box pointing to the next error in the linked list. Since all errors in the
list must have equal length, you only need to compare the length of the first
item; then you don't need to constantly allocate and copy vectors.
Additionally, the tail of the list can be a special enum member that doesn't
contain a next pointer, so you can avoid in most cases allocating anything on
the heap at all.

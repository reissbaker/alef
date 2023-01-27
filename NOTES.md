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

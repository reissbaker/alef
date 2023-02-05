You need better trace visibility for debugging. Rather than functions being
parsers, have parser builders that take some debuggable thing to print in the
tracer along with the function.

Right now you have some dangerous combos, e.g. count(any(thing)) will never
finish. You should automatically detect that kind of cycle and kill it (and
hopefully warn at build time????).

Based on the noticeable perf win from moving tracers out of spans (frequently
allocated + copied) into ParseContext, I suspect making Spans as small as
possible will result in similar perf boosts. Right now, spans include the
pointer to the string being parsed... However, I suspect they shouldn't, and
instead we should stick that pointer into the ParseContext, to avoid having to
copy it a bunch. ParseContexts are never modified or copied; we just pass the
pointer to one around everywhere. We can keep the span methods the same, and
instead just have them all take a ctx param that is expected to include the
input string.

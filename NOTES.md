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
input string. UPDATE: nope this actually was slower! No idea why.

I think the chains of `.or` calls is slowing down the parser vs nom, because
you need to do error handling and chaining even if the set of calls eventually
succeeds. An alternate option is to do lazy, CPS-style `.or` calls, where
basically the wrapper object takes what to try next, and passes errors forward
to the next thing. If you're the last one in the chain, only then do you chain
all the errors together. Basically this is your HList thing; you can't iterate
per se (there's no way to define the variable to loop through), but you can
tell it to run the parse chain for you. This mandates that everything in the
chain outputs the same type, but we already have that restriction on `.or`.
(Technically you could use Either enums to work around this, but oh boy would
that get cumbersome to use as a consumer as the number of parsers grows). Okay,
some experimentation reveals that you're basically gonna need `dyn Parser<'a,
O>` vecs, so let's first refactor Parser so it actually can be used as a trait
object.

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

It would be neat to be super-comptime-aware like Zig. Typechecker should detect
if a function is pure, and if it is, it should pre-compute returns for any
invocation of the function with static inputs. And if there are closures used
inside the fn, their purity should be a result of only the subset of variables
they close over, and if specifically those variables are known at compile time,
you can generate a single function at compile time and pass the fn pointer
around instead of creating the bound closure at runtime every time. Maybe only
worth it for AOT and not for interpreter mode; interpreter mode tradeoff is
fastest-possible start time with correctness typechecking, at the cost of worse
runtime perf and less runtime optimization.

Your fs module in the stdlib should have an fs.read vs an fs.import btw. The
import version imports the bytes into the binary, and allows them to be used at
compile time for better optimizations, and means you don't need to ship the
file along with the binary. Import can only be called on static or
comptime-known paths; it can't be called on runtime paths. The downside of
course is you can't change the file without recompiling: it's useful for
config, but not for reading user-provided data. fs.import should also work in
interpreter mode, but all it does is mean that the file can't change after
being loaded by the interpreter.

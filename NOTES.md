## Parsing
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

## Syntax + semantics
Dot-access should actually only be for fields -- trait functions should use a
colon, like Lua. That way you resolve awkward ambiguities around a lambda field
vs a trait function: the lambda field is accessed via `.`, whereas the trait
function is accessed via the `:` (which resolves awkward ambiguities for type
inference where there are objects in your codebase that have a field name
that's the same as a trait function name, which without distinction would force
all modules aware of those objects to disambiguate between field access and
trait invocation). But that change would make dictionary syntax potentially
ambiguous: does `{ a: b }` mean you're making a dictionary with a key of `a`
set to the value `b`, or does it mean you're invoking macro `b` on argument
`a`? Yes, if `b` *isn't* a macro, the result isn't ambiguous (it's obviously
not a macro invocation!), but if `b` is a macro, it might be valid to put a
macro into a dictionary; after all, we want to support things like runtime
printing a macro, so a macro should be able to get set to a value -- and thus it
should be able to be placed in a dictionary. To resolve this ambiguity, use
`:=` as the separator between keys and values in dicts: we don't allow an `=`
operator, so `:=` is unambiguously a special separator.

In fact! To make things even more homoiconic, you could have the dict syntax
rewrite itself to something like:

```
{dict
  {= a b}
}
```

And so `:=` actually is a nearly-valid trait macro invocation! (Technically
it's still syntax sugar and invalid-as-regular-code, since there isn't a
containing `{}` around the set.) Could you change that? E.g. is this actually
bad?

```
{dict
  {a := b}
  {test := "hello"}
}
```

Actually I think this is great. It means changing from a struct to a dict is
simple: just change the macro getting invoked to `make`. Makes named args for
functions transparently supported too: just pass a struct!

Any function or macro defined in a file should actually be considered to be
defined on that file's trait. Thus `:` always has the same semantics, whether
it's on explicit traits or on the file's trait.

Refactor steps:

- [x] Make the `:=` sequence the separator for dicts
- [x] Add support for `:` trait references
- [ ] Add the `=` operator
- [ ] Add the `dict` macro in the IR parser to generate dicts using ordinary
  macro syntax
- [ ] Remove special-cased dict parsing from the first-pass AST parser, and
  remove the wrapper for it from the IR parser

## Comptime
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

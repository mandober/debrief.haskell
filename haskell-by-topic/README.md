# Haskell by Topic

- [Haskell and category theory](./haskell-and-category-theory/README.md)
- [Monad Transformers](./monad-transformers/README.md)


## Haskell quick tips

### Ignore-all pattern

When you just want to reveal the data ctor, and you are not interested in its arguments, use the `{}` pattern to ignore them all. So, instead of writing, e.g. `Term _ _ _`, just write `Term{}`. (Presumably) `{}` denotes an empty record.

The `{}` need not be glued to the ctor - separating it with a space is fine too, i.e. `Term {}` - despite the fact that the latter is almost never seen in the wild for some reason.

There are situations when using `{}` instead of explicitly writing the ignore pattern (`_`) for each parameter makes a difference, but it is unclear when exactly, or even if particular language constructs (pattern matching on lhs? in `case`? in `let`?) influence it. Reasonably, it shouldn't matter where the matching is done, but it seems that sometimes a pattern containing `{}` fails when a series of ignore patterns passes (!), regardless of parenthesis.

Parenthesizing the pattern is mentioned due to strange occurrences: that the pattern in the fragment `eval (Term _ _ _)` succeeds is nothing unusual, but we'd like to shorten it, so we write `eval Term{}` - and it succeeds as well (witness!), despite the missing parethesis! (meh, mediocre).

The exact place where the pattern occurs is sometimes (!) also important: when nested inside anoter pattern, then it's usually fine: `App v (Term{})` instead of `App v @(Term _ _ _)` seems to always work. The problems are the outer pattern and *matching after a function name vs data ctor name*, i.e. function's parameters cannot be replaced by `{}`, only data ctor's.

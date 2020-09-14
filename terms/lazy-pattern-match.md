# Lazy pattern match

https://wiki.haskell.org/Lazy_pattern_match

The tilde sigil, `~`, is prepended to a pattern to make it irrefutable - such pattern will match anything, it will let the match succeed allowing the evaluation to proceed, but when the match is needed, if it had not succeeded without the ~, it won't succeed then so an error is issued. Therefore, a lazy pattern match must be applied to a pattern that would succeed without it, it's purpose is just to postpone the matching.

```hs
-- The lazy pattern match on a pair as in
f ~(a,b) = g a b
-- can be translated to
f p = g (fst p) (snd p)
```

These are all lazy pattern matches:

```hs
let (a,b) = p
f ~(a,b) = ...
case p of ~(a,b) -> ...
(\ ~(a,b) -> ... )
```

*The let matches the top-most constructor lazily*. This seems to be quite arbitrary but this is how it is defined. That is, if you want to match constructors lazily in two levels then you have to write:

```hs
let (a, ~(b,c)) = p
f ~(a, ~(b,c)) = ...
case p of ~(a, ~(b,c)) -> ...
(\ ~(a, ~(b,c)) -> ... )
```

The lazy pattern match on a pair as in:

```hs
f ~(a,b) = g a b
-- can be translated to
f p = g (fst p) (snd p)
```

Generally, a lazy pattern match is translated to calling corresponding record field accessors.

```hs
-- strict pattern match
f (a,b) = g a b
-- lazy pattern match
f ~(a,b) = g a b
```

The key difference between strict pattern match and lazy pattern match is that the strict pattern match requires to check for the pair constructor before `g` can be evaluated. In contrast to that, the lazy pattern match allows to defer the pair constructor match to the evaluation of `g a b`. If the function `g` can generate something without looking at its arguments then `f` can generate something as well before matching the pair constructor.

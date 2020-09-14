# Bottom

https://wiki.haskell.org/Bottom

The term "bottom" refers to a computation which never completes successfully, including computations that fail due to an error, or because they diverge (keep on looping indefinitely). In fact, in Haskell there is no telling whether a computation failed due to an error or because it diverged.

The symbol for bottom is `⊥`, in Unicode 0x22A5, in HTML `&perp;`, in LaTeX `\bot`, in ASCII `_|_`.

Bottom is a member of any and all Haskell types - even of the trivial `unit` type, `()`, as well as of any type equivalent to unit, i.e. a type that has a nullary single data ctor, like `data Unit = Unit`.

The bottom type is needed due to the *halting problem* - determining whether a computation is gonna halt or not is undecidable.

Note: some languages with dependent type systems, such as *Epigram*, can statically enforce termination, based on the type for particular programs, such as those using induction.

```hs
-- bottom is expressed by bottom or undefined
bottom = bottom

-- or by using the error function
bottom = error "Non-terminating computation!"

-- or the undefined function
undefined = error "Prelude.undefined"

-- Gofer defines bottom as
undefined | False = undefined

-- the type of bottom is arbitrary
-- defaulting to the most general type
undefined :: a
```

Since bottom is an inhabitant of every type, thus a value of any and all types, it can be used wherever a value of some particular type is needed.

This can be useful sometimes:

```hs
-- as a @TODO annotation
foo = undefined

-- dispatching to a type class instance
print (sizeOf (undefined :: Int))

-- using/probing laziness
print (head (1 : undefined))
```

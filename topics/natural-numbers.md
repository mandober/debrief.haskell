# Natural numbers

## Natural numbers in mathematics

In math, the set of the natural numbers, `ℕ`, is the fundamental number set upon which all the other numeric sets are built (ℤ, ℚ, ℝ, ℂ, ℍ, 𝕆), but the issue of the concrete definition of the naturals is elided, justified by the claim that if push comes to shove, they can always be defined.

  A.N. This is an instance of the "meh, let logicians handle the foundations" motto, that many a mathematician holds dear, having better things to do than fiddle with such technicalities. Also, math is magical: we can just say 'fiat naturals' and enjoy some commonly assumed definition of the naturals suitable for the current context, be it a crude definition in terms of sets, or a logical one in terms of the Peano axioms. Unfortunatelly, such technicalities cannot be elided in CS, so we need to come up with a precise definition - and the one based on the Peano axioms seems to fit nicely in a higher-order language like Haskell, which, like math, grants us some magical 'declarative' capabilities that save us from having to fiddle with minutia.


The naturals are canonically defined using the Peano axioms which produce a unary number encoding. Peano naturals arise from the 5 axioms (originally 9, but four of them deal with the equality relation which is now builtin), of which the first two, for our purpose, make the heart of the matter:

```hs
                  n ∈ ℕ
----- [ZERO]    -------- [SUCC]
Z ∈ ℕ           S n ∈ ℕ
```

The interpretation flows naturally:
- `Z` is a natural number.
- If `n` is a natural number than so is `S n`.

So, `Z` is just an arbitrary shape, a character, a label, a glyph. Writing `Z` denotes the first natural number that CS-tist call 'zero', as opposed to many mathematicians that call it 'one'. This also highlights the fact that 'Z' is "not" short for 'zero' but only an arbitrary symbol that might as well be `❋`, `0`, `O`, `|`, or any other symbol, whatsoever. The only really important thing is that the two symbols, `Z` and `S`, are distinct.


<details><summary>Interlude: The Socratic method</summary>

## Dialectico-dyslexical take on the matter

- "Hmmm...", wandered Socrates, "is `Z` than fit at all to be the unit of the operation Danaoi call addition...".
- "Quidquid id est, Timeo Danaos et dona ferentes!", blurted out Peano tourettesickly.
- "...which necessarily needs the unit element?", concluded Socrates the mortal, ignoring Peano's interruption, addressing Cicero.
Cicero, aequo pulsat pede: - "Yes, but only if one interprets `Z` as the number zero and allows the set of the naturals to contain it; only than would `Z` be the additive identity". However, if one interprets `Z` as the number one, than one can still define the naturals using Peano axioms, but not the addition".
- Although it doesn't seem right, it must be so - it's like not being able to define division on nats regardless of whether they start at 0 or not.
- so we cannot define addition at all? It doesn't feel right... maybe some peculiar version of addition, like monus, that doesn't require identity?
- ἓν οἶδα ὅτι οὐδὲν οἶδα...

https://sr.wikipedia.org/wiki/%D0%9A%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D1%98%D0%B0:%D0%9B%D0%B0%D1%82%D0%B8%D0%BD%D1%81%D0%BA%D0%B5_%D0%B8%D0%B7%D1%80%D0%B5%D0%BA%D0%B5

</details>



- `n ∈ ℕ => S n ∈ ℕ`
  - If `n` denotes a natural number, than so does `S n`.
  - prefixed to a natural number `n`, `S` increments `n`, producing another natural number, `S n`; e.g. if `S S Z` is a natural, then so is `S S S Z`.

The symbol `S` is a meaningless character, might've been ★; but, unlike `Z` it has function-like property: prefixed to a natural number, `S` increments it, producing another natural number. This manner of application is exactly mimicked by the Haskell's data ctors - data ctors are functions indeed, but they don't reduce their arguments (if they have any at all); instead, they just attach themselves (in front of) the arg. This manner of application is completely syntactical and deterministic, so GHC does it automatically - whenever we define a new type, the application of a data ctor need not be explicitly writen; the GHC takes care of it.

# Tuples

N|type ctor   | type params        | type                 | name    | arity
-|------------|--------------------|----------------------|---------|---------
0| ()         | none               | ()                   |empty    | nullary
1| N/A        | N/A                | N/A                  |single   | unary
2| (,)        | a b                | (a,b)                |double   | binary
3| (,,)       | a b c              | (a,b,c)              |triple   | ternary
4| (,,,)      | a b c d            | (a,b,c,d)            |quadruple| quaternary
5| (,,,,)     | a b c d e          | (a,b,c,d,e)          |quintuple| quinary
6| (,,,,,)    | a b c d e f        | (a,b,c,d,e,f)        |sextuple | sexary
7| (,,,,,,)   | a b c d e f g      | (a,b,c,d,e,f,g)      |septuple | senary
8| (,,,,,,,)  | a b c d e f g h    | (a,b,c,d,e,f,g,h)    |octuple  | octary
9| (,,,,,,,,) | a b c d e f g h i  | (a,b,c,d,e,f,g,h,i)  |nonuple  | nonary
n| (,…,)      | α₁ α₂ … αₙ         | (α₁ α₂, …, αₙ)        | n-tuple | n-ary

10 decuple

## Pair

The **pair type** where `C τ₀ … τₙ` is instantiated as `(,) τ₀ τ₁`, also writen in the infix form as `(τ₀, τ₁)`, and in the surface lang writen as, e.g. `(α, β)`, or `(bool, nat)`, etc. The type ctor of a pair type is `,`, but we can also have **tuple types** that are somewhat complicated to denote since each tuple has its own type ctor whose form depends on a tuple's arity. The meta-type-ctor `C`, that is, its form `C τ₀ τ₁ … τₙ`, may be instantiated as:
- `C τ₁ τ₂`    : `(,) α β`   i.e. (α, β)
- `C τ₁ τ₂ τ₃` : `(,) α β γ` i.e. (α, β, γ)
- etc.

An `n`-tuple has `n` components and its type ctor has `n - 1` comma symbols. The extreme case of tuples is the empty tuple, `()` that signifies the *top* type, also called *unit*, whose sole term is also denoted by `()`; so `() : ()`.

There is no unary tuple.

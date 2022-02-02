# 4.1.2 Syntax of Types

The syntax for Haskell type expressions

```js bnf
type   := btype                               (base type)
        | btype '->' type                     (function type)

btype  := atype                               (type application)
        | btype atype                         (type application)

atype  := gtycon
        | tyvar
        | '(' type ')'                        (parenthesised constructor)
        | '[' type ']'                        (list type)
        | '(' type₁ , … , typeₖ ')'           (tuple type, k ≥ 2)

gtycon := qtycon
        | '()'                                (unit type)
        | '[]'                                (list constructor)
        | '(->)'                              (function constructor)
        | '(,)'                               (2-tuple constructor)
        | '(,,)'                              (3-tuple constructor)
        | '('  ','…  ')'                      (tupling constructors)
```

Just as data values are built using data constructors, type values are built from type constructors.


Main forms of type expressions:

1. **Type variables**
  - tyvar is denoted `α` (in the meta language)
  - the kind of tyvars is determined by the context (Haskell2010)
  - the kind of tyvars can be given manually (GHC2021)

2. **Type constructors**
  - their general form is `C α₁ … αₙ` (in the meta language)

  * Nullary type ctors
    - `C`
    - have the kind `Type`
    - may be divided into base (builtin) types and library types
    - base types (type constants) include
      - `Char`
      - `Int`
      - `Word`
      - `Float`
      - `Double`
      - `Integer`
      - `()`
    - library types include
      - `Bool`
      - `Ordering`

  * Unary type ctors
    - `C α`
    - have the kind `Type → Type`
    - e.g. `Maybe`, `IO`, `[]`

  * Binary type ctors
    - `C α₁ α₂`
    - have the kind `Type → (Type → Type)`
    - e.g. `(→)`, `(,)`, `Either`

  * N-ary type ctors
    - `C α₁ … αₙ`
    - have the kind `κ → κ` where `κ` is a kind function or `Type`

  * declarations like `data T …`
    - add the type constructor named `T` to the type vocabulary.
    - the kind of `T` is determined by kind inference.
    - special syntax is provided for certain builtin type ctors.

  * Saturatation of type ctors
    - saturated type ctors have the kind `Type`
    - unsaturated type ctors have the kind other then `Type`
      - For example, the type ctor `Maybe` itself has kind `Type → Type` 
        meaning the type arg it expects must have the kind `Type`.
      - The declaration of `Maybe` is `data Maybe a …` so Maybe 
        takes one type as its arg (since it has one tyvar, `a`).
      - Applying `Maybe` to a type arg `Int`, i.e. as `Maybe Int`, 
        makes the resulting type saturated, 
        i.e. `Maybe Int` has the kind `Type`.

                Maybe :: Type → Type
                  Int :: Type
            Maybe Int :: Type

  * Special and builtin type ctors
    - unit type, `()`, has kind `Type`
      - unit is the trivial type, considered a base type.
      - it denotes a nullary-tuple type (there is no 1-tuple)
      - it has only one value, also denoted `()`
    - function type `(→)` has kind `Type → Type → Type`
    - list type `[]` has kind `Type → Type`
    - tuple types are written as `(,)`, `(,,)`, etc.

3. **Type application**
  - if `t₁` is a type of kind `κ₁ → κ₂` and `t₂` is a type of kind `κ₁`, then `t₁ t₂` is a type expression of kind `κ₂`.

4. **Parenthesized type**
  - having form `(t)`, is identical to the type `t`.



Special syntax is provided to allow certain type expressions to be written in a more traditional style:

* A function type has the form `t1 -> t2`, which is equivalent to the type `(->) t1 t2`. Function arrows associate to the right.
* A tuple type has the form `(t1, … , tk)` where k ≥ 2, which is equivalent to the type `(,…,) t1 … tk` where there are k−1 commas between the parenthesis. It denotes the type of k-tuples with the first component of type t1, the second component of type t2, and so on.
* A list type has the form [t], which is equivalent to the type `[] t`. It denotes the type of lists with elements of type `t`.

> These special syntactic forms always denote the built-in type constructors for functions, tuples, and lists, regardless of what is in scope. In a similar way, the prefix type constructors `(->)`, `[]`, `()`, `(,)`, and so on, always denote the built-in type constructors; they *cannot be qualified*, nor mentioned in import or export lists (hence the special production, `gtycon`).

Although the list and tuple types have special syntax, their semantics is the same as the equivalent user-defined algebraic data types.

Notice that expressions and types have a consistent syntax. If `ti` is the type of expression or pattern `ei`, then the expressions `(\ e1 -> e2)`, `[e1]`, and `(e1,e2)` have the types `(t1 -> t2)`, `[t1]`, and `(t1,t2)`, respectively.

With one exception (that of the *distinguished type variable in a class declaration*), the type variables in a Haskell type expression are all assumed to be universally quantified; there is no explicit syntax for universal quantification [4]. For example, the type expression a -> a denotes the type ∀ a. a  →  a. For clarity, however, we often write quantification explicitly when discussing the types of Haskell programs. When we write an explicitly quantified type, the scope of the ∀ extends as far to the right as possible; for example, ∀ a. a  →  a means ∀ a. (a  →  a).

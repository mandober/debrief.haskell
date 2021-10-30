# Types: Summary

- primitive types: Int, Integer (GMP), Float, Double, Char (names in PascalCase)
- primitives are unlifted types - they do not contain bottom value i.e. ⊥
- all the other types are lifted: each type has a bottom value (`undefined`)
- bottom denotes non-terminating function, infinite loop
- a function `a -> Bool` may return 3 values: `True`, `False` and `undefined`

- polymorphic types have type params, e.g. `a -> b`
- type params are named in camelCase, usually a single letter
- a type param, like `a`, may be replaced only with types of `*` kind
- there's a lang ext to enable types of other kinds as well, like `* -> * -> *`

- class groups types by a set of common properties
- class declares but may also define default impl
- classes follow algebraic structures from math: Monoid, Semigroup, etc.
- class prescribes type constraints: `(^) :: (Integral b) => a -> b -> a`
- this may be considered as a superclass/subclass relation

- fixity determines associativity (neutral, left, right) and precedence (0-10)

- `->` is a type ctor for functions, `. --> .` of kind `*`
- `->` is infixr: `a -> b -> c` is actually `a -> (b -> c)`
- function types are stated in the signatures, `foo :: -> a (a -> b) -> b`
- funcs can share a signature: `(+), (-), (*) :: a -> a -> a`


- Tuple type ctor is `,`: `(,) a b == (a,b)`
- `() :: ()` is the empty tuple called unit
- there is no singleton tuple

- `IO` is a monad that manages side-effects (print to stdout)


- use `data` keyword to define a type
- use `class` keyword to define a type class
- use `instance` keyword to implement class' functions

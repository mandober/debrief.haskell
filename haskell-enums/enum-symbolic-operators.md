# Symbolic operators

The restrictions on names of items.

ASCII
- alphabetic set := { a-zA-Z }
- alphanumeric set := { a-zA-Z0-9 }
- underscore set := { _ }
- punctuation set (22) := { `~ ! @ # $ % ^ & * - = + | / : ? \ , < . >` }
- special set := { : - ! ~ }
- reserved set := { : | ^ @ = \ }
- reserved combo set := { ..  ::  =>  <-  -> }

Individual glyphs:
- `,` module exports/imports list
- `,` list and tuple elements separator
- `,` record fields separator
- `:` builtin list `cons` function/operator
- `_` type hole (at type level)
- `_` partial type hole (at part of the type)
- `_` value hole (at term level)


- `:` (colon) is reserved: symbolic data ctor names must start with a colon
- `-`, `!`, `~` are operators with a special meaning in some circumstances
- `:` reserved as initial symbol for symbolic data ctor names, `:+`, `:@`
- reserved symbols (cannot be used): `: = @ \ | ^`
  - `:` cons function
  - `=` patter match
  - `@` as-pattern
  - `\` escape char
  - `|` sum type
- reserved symbolic pairs (cannot be used): `..  ::  <-  ->  =>`
  - `::` type of operator
  - `->` function ctor
  - `=>` constraint operator
  - `<-` slurp operator
  - `..` part of module import syntax, record syntax




## Data constructors

Data constructors are usually defined:
- in prefix positions as alphabetic chars starting with an uppercased letter.
- can be defined in infix positions:
  - ...| a :+: (List a)
  - ...| a `Cons` (List a)


---


Items with names:
- module name (dir name, file name)
- variable
- function
- class

Item             | Alpha name         | Symbolic name  [not certain]
-----------------|--------------------|--------------|
function         | lower              | 


- operator          : ✔
- type ctors        : ✔
- data ctors        : ✔ must lead with (:)
- class names       : ✔
- let bindings      : ✔
- where clauses     : ✔
- module names      : ?
- named patterns    : ???


* Symbolic operators for data ctors must start with a colon

## ASCII operators

* fn ctor: as ASCII `->` or `-->` (as unicode `→` or `⟶`)
* constraint: `=>` (unicode: not: ⇒, ⟹, ⟾)
* `<-` ⟵ 
* `-` subtraction, negation, negative literal number
* `~` 
* `<>` in GHC.Base: class Semigroup a where (<>) :: a -> a -> a; infixr 6 <>

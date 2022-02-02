# Keywords - HaskellWiki

https://wiki.haskell.org/Keywords


-   [1 !][3]
-   [2 '][4]
-   [3 ''][5]
-   [4 \-][6]
-   [5 \--][7]
-   [6 \-<][8]
-   [7 \-<<][9]
-   [8 \->][10]
-   [9 ::][11]
-   [10 ;][12]
-   [11 <-][13]
-   [12 ,][14]
-   [13 \=][15]
-   [14 \=>][16]
-   [15 \>][17]
-   [16 ?][18]
-   [17 #][19]
-   [18 \*][20]
-   [19 @][21]
-   [20 \[|, |\]][22]
-   [21 \\][23]
-   [22 \_][24]
-   [23 \`][25]
-   [24 {, }][26]
-   [25 {-, -}][27]
-   [26 |][28]
-   [27 ~][29]
-   [28 as][30]
-   [29 case, of][31]
-   [30 class][32]
-   [31 data][33]
-   [32 data family][34]
-   [33 data instance][35]
-   [34 default][36]
-   [35 deriving][37]
-   [36 deriving instance][38]
-   [37 do][39]
-   [38 forall][40]
-   [39 foreign][41]
-   [40 hiding][42]
-   [41 if, then, else][43]
-   [42 import][44]
-   [43 infix, infixl, infixr][45]
-   [44 instance][46]
-   [45 let, in][47]
-   [46 mdo][48]
-   [47 module][49]
-   [48 newtype][50]
-   [49 proc][51]
-   [50 qualified][52]
-   [51 rec][53]
-   [52 type][54]
-   [53 type family][55]
-   [54 type instance][56]
-   [55 where][57]

## !

Whenever a data [constructor][58] is applied, each argument to the constructor is evaluated if and only if the corresponding type in the algebraic data[type][59] declaration has a strictness flag, denoted by an exclamation point. For example:

 data STList a 
         \= STCons a !(STList a)  \-- the second argument to STCons will be 
                                 \-- evaluated before STCons is applied
         | STNil

to illustrate the difference between strict versus lazy constructor application, consider the following:

 stList \= STCons 1 undefined
 lzList \= (:)    1 undefined
 stHead (STCons h \_) \= h \-- this evaluates to undefined when applied to stList
 lzHead (h : \_)      \= h \-- this evaluates to 1 when applied to lzList

! is also used in the ["bang patterns"][60] (GHC extension), to indicate strictness in patterns:

## '

-   Character literal: `'a'`
-   [Template Haskell][61]: Name of a (value) variable or data constructor: `'length`, `'Left`
-   (in types, GHC specific) Promoted data constructor: `'True`

## ''

-   [Template Haskell][62]: Name of a type constructor or class: `''Int`, `''Either`, `''Show`

## \-

This operator token is magic/irregular in the sense that

is parsed as the negative integer -1, rather than as an operator [section][63], as it would be for any other operator:

(++ "foo") :: String \-> String

It is syntactic sugar for the `negate` function in Prelude. See [unary operator][64]. If you want the section, you can use the `subtract` function or `(+(-1))`.

## \--

Starts a single-line comment, unless immediately followed by an operator character other than `-`:

main \= print "hello world" \-- this is a comment
\--this is a comment as well
\---this too
foobar \--+ this\_is\_the\_second\_argument\_of\_the\_dash\_dash\_plus\_operator

The multi-line variant for comments is `{- comment -}`.

## \-<

[Arrow notation][65]

## \-<<

[Arrow notation][66]

## \->

-   The function type constructor:

-   In lambda functions:

-   To denote alternatives in case statements:

case Just 3 of
    Nothing \-> False
    Just x  \-> True

or with LambdaCase:

(\\ case 1 \-> 0
      ; \_ \-> 1 )

or with MultiWayIf:

if | 1 \== 0    \-> 1 
   | 1 \== 2    \-> 2 
   | otherwise \-> 3

-   On the kind level (GHC specific):

ghci\> :kind (\->)
(\->) :: \* \-> \* \-> \*

-   [Functional dependencies][67]

\-- This examples assumes that each type 'c' can "contain" only one type
\--  i.e. type 'c' uniquely determines type 'elt'
class Contains c elt | c \-> elt where
   ...

-   [View patterns][68]

## ::

Read as "has type":

"Length has type list-of-'a' to Int"

Or "has kind" (GHC specific):

## ;

-   Statement separator in an explicit block (see [layout][69])

## <-

-   In do-notation, "draw from":

do x <- getChar
   putChar x

-   In list comprehension generators, "in":

\[ (x,y) | x <- \[1..10\], y <- \['a'..'z'\] \]

-   In [pattern guards][70], "matches":

f x y | Just z <- g x \= True
      | otherwise     \= False

## ,

Separator in lists, tuples, records.

\[1,2,3\]
(1,2,3)
Point {x \= 1, y \= 2}

In list comprehensions before generators, "and" (the first comma after `|`):

\[ (x,y) | x <- \[1..10\], y <- \['a'..'z'\], x \> 42 \]

In list comprehensions before Boolean tests, "when" (the second comma after `|`):

\[ (x,y) | x <- \[1..10\], y <- \['a'..'z'\], x \> 42 \]

In guards inside case expressions, "and when":

case \[1,3,9\] of xs | (x:ys) <- xs, (y:\_) <- ys, let z\=x+1, z /= y \-> \[x,y,z\]

In module import and export lists:

module MyModule
  ( MyData (C1,C2)
  , myFun ) where

import MyModule (MyData (C1,C2), myFun)

## \=

Used in definitions.

Also in pattern-matching records:

case point of
  Point {x \= x0, y \= y0} \-> f x0 y0

## \=>

Used to indicate instance contexts, for example:

sort :: Ord a \=> \[a\] \-> \[a\]

## \>

In a Bird's style [Literate Haskell file][71], the > character is used to introduce a code line.

comment line

\> main \= print "hello world"

## ?

-   **The [GHC Users Guide][72] has [a section about Implicit Parameters][73].**

ghci\> :t ?foo ++ "bar"
?foo ++ "bar" :: (?foo::\[Char\]) \=> \[Char\]

## #

-   [MagicHash][74]

## \*

-   Is an ordinary operator name on the value level

-   On the [kind][75] level: The kind of boxed types (GHC-specific)

## @

-   Patterns of the form `var@pat` are called as-patterns, and allow one to use `var` as a name for the value being matched by `pat`. For example:

     case e of { xs@(x:rest) \-> if x\==0 then rest else xs }

    \-- is equivalent to:

     let { xs \= e } in
       case xs of { (x:rest) \-> if x\==0 then rest else xs }

-   [Visible type applications][76]

## \[|, |\]

-   [Template Haskell][77]
    -   Expression quotation: `[| print 1 |]`
    -   Declaration quotation: `[d| main = print 1 |]`
    -   Type quotation: `[t| Either Int () |]`
    -   Pattern quotation: `[p| (x,y) |]`
    -   [Quasiquotation][78]: `[nameOfQuasiQuoter| ... |]`

## \\

The backslash "\\" is used

-   in multiline strings

-   in lambda functions

## \_

Patterns of the form \_ are wildcards and are useful when some part of a pattern is not referenced on the right-hand-side. It is as if an identifier not used elsewhere were put in its place. For example,

 case e of { \[x,\_,\_\]  \->  if x\==0 then True else False }

is equivalent to:

 case e of { \[x,y,z\]  \->  if x\==0 then True else False }

## \`

A function enclosed in back ticks "\`" can be used as an infix operator.

is the same as

## {, }

-   Explicit block (disable [layout][79]), possibly with ";" .

-   Record update notation

changePrice :: Thing \-> Price \-> Thing
changePrice x new \= x { price \= new }

-   Comments (see below)

## {-, -}

Everything between "{-" followed by a space and "-}" is a block comment.

## |

The "pipe" is used in several places

-   Data type definitions, "or"

data Maybe a \= Just a | Nothing

-   List comprehensions, "for" (as in, "list of `a*a` for `a` in `[1..]`)

squares \= \[a\*a | a <- \[1..\]\]

-   Guards, "when"

safeTail x | null x    \= \[\]
           | otherwise \= tail x

-   [Functional dependencies][80], "where"

class Contains c elt | c \-> elt where
   ...

## ~

-   Lazy pattern bindings. Matching the pattern `~pat` against a value always succeeds, and matching will only diverge when one of the variables bound in the pattern is used.

f1, f2 :: Maybe Int \-> String
f1 x \= case x of 
    Just n \-> "Got it"
f2 x \= case x of
    ~(Just n) \-> "Got it"

(+++), (++++) :: (a \-> b) \-> (c \-> d) \-> (a, c) \-> (b, d) 
(f +++ g) ~(x, y) \= (f x, g y)
(f ++++ g) (x, y) \= (f x, g y)

Then we have:

f1 Nothing
Exception: Non\-exhaustive patterns in case

f2 Nothing
"Got it"

(const 1 +++ const 2) undefined
(1,2)

(const 1 ++++ const 2) undefined
Exception: Prelude.undefined

For more details see [the Haskell Wikibook][81].

-   Equality constraints. Assert that two types in a context must be the same:

example :: F a ~ b \=> a \-> b

Here the type "F a" must be the same as the type "b", which allows one to constrain polymorphism (especially where type families are involved), but to a lesser extent than functional dependencies. See [Type Families][82].

## as

Renaming module imports. Like `qualified` and `hiding`, `as` is not a reserved word but may be used as function or variable name.

import qualified Data.Map as M

main \= print (M.empty :: M.Map Int ())

## case, of

A case expression has the general form

 case e of { p1 match1 ; ... ; pn matchn }

where each `match`i is of the general form

| g1 \-> e1
  ...
| gm \-> em
    where decls

Each alternative consists of patterns `p`i and their matches, `match`i. Each `match`i in turn consists of a sequence of pairs of guards `g`ij and bodies `e`ij (expressions), followed by optional bindings (`decls`i) that scope over all of the guards and expressions of the alternative. An alternative of the form

is treated as shorthand for:

  pat | True \-> exp
    where decls

A case expression must have at least one alternative and each alternative must have at least one body. Each body must have the same type, and the type of the whole expression is that type.

A case expression is evaluated by pattern matching the expression `e` against the individual alternatives. The alternatives are tried sequentially, from top to bottom. If `e` matches the pattern in the alternative, the guards for that alternative are tried sequentially from top to bottom, in the environment of the case expression extended first by the bindings created during the matching of the pattern, and then by the `decls`i  in the `where` clause associated with that alternative. If one of the guards evaluates to `True`, the corresponding right-hand side is evaluated in the same environment as the guard. If all the guards evaluate to `False`, matching continues with the next alternative. If no match succeeds, the result is \_|\_.

## class

A [class declaration][83] introduces a new type class and the overloaded operations that must be supported by any type that is an instance of that class.

  class Num a  where
    (+)    :: a \-> a \-> a
    negate :: a \-> a

## data

The [data][84] declaration is how one introduces new algebraic data [types][85] into Haskell. For example:

data Set a \= NilSet 
           | ConsSet a (Set a)

Another example, to create a datatype to hold an [abstract syntax tree][86] for an expression, one could use:

 data Exp \= Ebin   Operator Exp Exp 
          | Eunary Operator Exp 
          | Efun   FunctionIdentifier \[Exp\] 
          | Eid    SimpleIdentifier

where the [types][87] `Operator, FunctionIdentifier` and `SimpleIdentifier` are defined elsewhere.

See the page on [types][88] for more information, links and examples.

## data family

Declares a datatype family (see [type families][89]). GHC language extension.

## data instance

Declares a datatype family instance (see [type families][90]). GHC language extension.

## default

Ambiguities in the class Num are most common, so Haskell provides a way to resolve them---with a default declaration:

Only one default declaration is permitted per module, and its effect is limited to that module. If no default declaration is given in a module then it assumed to be:

  default (Integer, Double)

## deriving

data and newtype declarations contain an optional deriving form. If the form is included, then derived instance declarations are automatically generated for the datatype in each of the named classes.

[Derived instances][91] provide convenient commonly-used operations for user-defined datatypes. For example, derived instances for datatypes in the class Eq define the operations == and /=, freeing the programmer from the need to define them.

data T \= A
       | B
       | C
       deriving (Eq, Ord, Show)

In the case of newtypes, GHC extends this mechanism to [Cunning Newtype Deriving][92].

## deriving instance

Standalone deriving (GHC language extension).

{-# LANGUAGE StandaloneDeriving #-}
data A \= A

deriving instance Show A

## do

Syntactic sugar for use with monadic expressions. For example:

 do { x ; result <- y ; foo result }

is shorthand for:

 x \>> 
 y \>>= \\result \->
 foo result

## forall

This is a GHC/Hugs extension, and as such is not portable Haskell 98/2010. It is only a reserved word within types.

Type variables in a Haskell type expression are all assumed to be universally quantified; there is no explicit syntax for universal quantification, in standard Haskell 98/2010. For example, the type expression `a -> a` denotes the type `forall a. a ->a`. For clarity, however, we often write quantification explicitly when discussing the types of Haskell programs. When we write an explicitly quantified type, the scope of the forall extends as far to the right as possible; for example,

means

GHC [introduces][93] a `forall` keyword, allowing explicit quantification, for example, to encode [existential types][94]:

data Foo \= forall a. MkFoo a (a \-> Bool)
         | Nil

MkFoo :: forall a. a \-> (a \-> Bool) \-> Foo
Nil   :: Foo

\[MkFoo 3 even, MkFoo 'c' isUpper\] :: \[Foo\]

## foreign

A keyword for the [Foreign Function Interface][95] (commonly called the FFI) that introduces either a `foreign import` declaration, which makes a function from a non-Haskell library available in a Haskell program, or a `foreign export` declaration, which allows a function from a Haskell module to be called in non-Haskell contexts.

## hiding

When importing modules, without introducing a name into scope, entities can be excluded by using the form

hiding (import1 , ... , importn )

which specifies that all entities exported by the named module should be imported except for those named in the list.

For example:

import Prelude hiding (lookup,filter,foldr,foldl,null,map)

## if, then, else

A conditional expression has the form:

and returns the value of e2 if the value of e1 is True, e3 if e1 is False, and \_|\_ otherwise.

 max a b \= if a \> b then a else b

## import

[Modules][96] may reference other modules via explicit import declarations, each giving the name of a module to be imported and specifying its entities to be imported.

For example:

  module Main where
    import A
    import B
    main \= A.f \>> B.f

  module A where
    f \= ...

  module B where
    f \= ...

See also [as][97], [hiding][98] , [qualified][99] and the page [Import][100]

## infix, infixl, infixr

A [fixity declaration][101] gives the fixity and binding precedence of one or more operators. The integer in a fixity declaration must be in the range 0 to 9. A fixity declaration may appear anywhere that a [type signature][102] appears and, like a type signature, declares a property of a particular operator.

There are three kinds of fixity, non-, left- and right-associativity (infix, infixl, and infixr, respectively), and ten precedence levels, 0 to 9 inclusive (level 0 binds least tightly, and level 9 binds most tightly).

  module Bar where
    infixr 7 \`op\`
    op \= ...

## instance

An instance declaration declares that a type is an instance of a class and includes the definitions of the overloaded operations - called class methods - instantiated on the named type.

  instance Num Int  where
    x + y       \=  addInt x y
    negate x    \=  negateInt x

## let, in

Let expressions have the general form:

let { d1 ; ... ; dn } in e

They introduce a nested, lexically-scoped, mutually-recursive list of declarations (let is often called [let rec][103] in other languages). The scope of the declarations is the expression e and the right hand side of the declarations.

Within `do`\-blocks or list comprehensions `let { d1 ; ... ; dn }` without `in` serves to introduce local bindings.

## [mdo][104]

The recursive `do` keyword enabled by -fglasgow-exts.

## module

Taken from: [A Gentle Introduction to Haskell, Version 98][105]

Technically speaking, a module is really just one big declaration which begins with the keyword module; here's an example for a module whose name is Tree:

module Tree ( Tree(Leaf,Branch), fringe ) where

data Tree a                \= Leaf a | Branch (Tree a) (Tree a) 

fringe :: Tree a \-> \[a\]
fringe (Leaf x)            \= \[x\]
fringe (Branch left right) \= fringe left ++ fringe right

## newtype

The `newtype` declaration is how one introduces a renaming for an algebraic data [type][106] into Haskell. This is different from `type` below, as a `newtype` requires a new [constructor][107] as well. As an example, when writing a compiler one sometimes further qualifies `Identifier`s to assist in type safety checks:

newtype SimpleIdentifier = SimpleIdentifier Identifier
newtype FunctionIdentifier = FunctionIdentifier Identifier

Most often, one supplies [smart constructors][108] and [destructors][109] for these to ease working with them.

See the page on [types][110] for more information, links and examples.

For the differences between `newtype` and `data`, see [Newtype][111].

## proc

proc (arrow abstraction) is a kind of lambda, except that it constructs an arrow instead of a function.

[Arrow notation][112]

## qualified

Used to import a module, but not introduce a name into scope. For example, Data.Map exports lookup, which would clash with the Prelude version of lookup, to fix this:

import qualified Data.Map

f x \= lookup x \-- use the Prelude version
g x \= Data.Map.lookup x \-- use the Data.Map version

Of course, Data.Map is a bit of a mouthful, so qualified also allows the use of as.

import qualified Data.Map as M

f x \= lookup x \-- use Prelude version
g x \= M.lookup x \-- use Data.Map version

## rec

The [rec][113] keyword can be used when the `-XDoRec` flag is given; it allows recursive bindings in a do-block.

{-# LANGUAGE DoRec #-}
justOnes \= do { rec { xs <- Just (1:xs) }
              ; return (map negate xs) }

## type

The `type` declaration is how one introduces an alias for an algebraic data [type][114] into Haskell. As an example, when writing a compiler one often creates an alias for identifiers:

This allows you to use `Identifer` wherever you had used `String` and if something is of type `Identifier` it may be used wherever a `String` is expected.

See the page on [types][115] for more information, links and examples.

Some common `type` declarations in the Prelude include:

type FilePath \= String
type String \= \[Char\]
type Rational \= Ratio Integer
type ReadS a \= String \-> \[(a,String)\]
type ShowS \= String \-> String

## type family

Declares a type synonym family (see [type families][116]). GHC language extension.

## type instance

Declares a type synonym family instance (see [type families][117]). GHC language extension.

## where

Used to introduce a module, instance, class or [GADT][118]:

module Main where

class Num a where
    ...

instance Num Int  where
    ...

data Something a where
   ...

And to bind local variables:

f x \= y
    where y \= x \* 2

g z | z \> 2 \= y
    where y \= x \* 2



[1]: https://wiki.haskell.org/Hoogle "Hoogle"
[2]: http://www.haskell.org/onlinereport/haskell2010/
[3]: https://wiki.haskell.org/Keywords#.21
[4]: https://wiki.haskell.org/Keywords#.27
[5]: https://wiki.haskell.org/Keywords#.27.27
[6]: https://wiki.haskell.org/Keywords#-
[7]: https://wiki.haskell.org/Keywords#--
[8]: https://wiki.haskell.org/Keywords#-.3C
[9]: https://wiki.haskell.org/Keywords#-.3C.3C
[10]: https://wiki.haskell.org/Keywords#-.3E
[11]: https://wiki.haskell.org/Keywords#::
[12]: https://wiki.haskell.org/Keywords#.3B
[13]: https://wiki.haskell.org/Keywords#.3C-
[14]: https://wiki.haskell.org/Keywords#.2C
[15]: https://wiki.haskell.org/Keywords#.3D
[16]: https://wiki.haskell.org/Keywords#.3D.3E
[17]: https://wiki.haskell.org/Keywords#.3E
[18]: https://wiki.haskell.org/Keywords#.3F
[19]: https://wiki.haskell.org/Keywords#.23
[20]: https://wiki.haskell.org/Keywords#.2A
[21]: https://wiki.haskell.org/Keywords#.40
[22]: https://wiki.haskell.org/Keywords#.5B.7C.2C_.7C.5D
[23]: https://wiki.haskell.org/Keywords#.5C
[24]: https://wiki.haskell.org/Keywords#
[25]: https://wiki.haskell.org/Keywords#.60
[26]: https://wiki.haskell.org/Keywords#.7B.2C_.7D
[27]: https://wiki.haskell.org/Keywords#.7B-.2C_-.7D
[28]: https://wiki.haskell.org/Keywords#.7C
[29]: https://wiki.haskell.org/Keywords#.7E
[30]: https://wiki.haskell.org/Keywords#as
[31]: https://wiki.haskell.org/Keywords#case.2C_of
[32]: https://wiki.haskell.org/Keywords#class
[33]: https://wiki.haskell.org/Keywords#data
[34]: https://wiki.haskell.org/Keywords#data_family
[35]: https://wiki.haskell.org/Keywords#data_instance
[36]: https://wiki.haskell.org/Keywords#default
[37]: https://wiki.haskell.org/Keywords#deriving
[38]: https://wiki.haskell.org/Keywords#deriving_instance
[39]: https://wiki.haskell.org/Keywords#do
[40]: https://wiki.haskell.org/Keywords#forall
[41]: https://wiki.haskell.org/Keywords#foreign
[42]: https://wiki.haskell.org/Keywords#hiding
[43]: https://wiki.haskell.org/Keywords#if.2C_then.2C_else
[44]: https://wiki.haskell.org/Keywords#import
[45]: https://wiki.haskell.org/Keywords#infix.2C_infixl.2C_infixr
[46]: https://wiki.haskell.org/Keywords#instance
[47]: https://wiki.haskell.org/Keywords#let.2C_in
[48]: https://wiki.haskell.org/Keywords#mdo
[49]: https://wiki.haskell.org/Keywords#module
[50]: https://wiki.haskell.org/Keywords#newtype
[51]: https://wiki.haskell.org/Keywords#proc
[52]: https://wiki.haskell.org/Keywords#qualified
[53]: https://wiki.haskell.org/Keywords#rec
[54]: https://wiki.haskell.org/Keywords#type
[55]: https://wiki.haskell.org/Keywords#type_family
[56]: https://wiki.haskell.org/Keywords#type_instance
[57]: https://wiki.haskell.org/Keywords#where
[58]: https://wiki.haskell.org/Constructor "Constructor"
[59]: https://wiki.haskell.org/Type "Type"
[60]: https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/bang-patterns.html
[61]: https://wiki.haskell.org/Template_Haskell "Template Haskell"
[62]: https://wiki.haskell.org/Template_Haskell "Template Haskell"
[63]: https://wiki.haskell.org/Section_of_an_infix_operator "Section of an infix operator"
[64]: https://wiki.haskell.org/Unary_operator "Unary operator"
[65]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#arrow-notation
[66]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#arrow-notation
[67]: https://wiki.haskell.org/Functional_dependencies "Functional dependencies"
[68]: https://wiki.haskell.org/View_patterns "View patterns"
[69]: https://wiki.haskell.org/Layout "Layout"
[70]: https://wiki.haskell.org/Pattern_guard "Pattern guard"
[71]: https://wiki.haskell.org/Literate_programming "Literate programming"
[72]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/
[73]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/implicit_parameters.html#
[74]: https://downloads.haskell.org/~ghc/7.6.2/docs/html/users_guide/syntax-extns.html
[75]: https://wiki.haskell.org/Kind "Kind"
[76]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#visible-type-application
[77]: https://wiki.haskell.org/Template_Haskell "Template Haskell"
[78]: https://wiki.haskell.org/Quasiquotation "Quasiquotation"
[79]: https://wiki.haskell.org/Layout "Layout"
[80]: https://wiki.haskell.org/Functional_dependencies "Functional dependencies"
[81]: http://en.wikibooks.org/wiki/Haskell/Laziness#Lazy_pattern_matching
[82]: https://wiki.haskell.org/Type_families#Equality_constraints "Type families"
[83]: http://haskell.org/onlinereport/decls.html#class-decls
[84]: http://haskell.org/onlinereport/decls.html#user-defined-datatypes
[85]: https://wiki.haskell.org/Type "Type"
[86]: https://wiki.haskell.org/Abstract_syntax_tree "Abstract syntax tree"
[87]: https://wiki.haskell.org/Type "Type"
[88]: https://wiki.haskell.org/Type "Type"
[89]: https://wiki.haskell.org/Type_families "Type families"
[90]: https://wiki.haskell.org/Type_families "Type families"
[91]: http://haskell.org/onlinereport/decls.html#derived-decls
[92]: https://wiki.haskell.org/Cunning_Newtype_Deriving "Cunning Newtype Deriving"
[93]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/poly_kinds.html#inferring-dependency-in-user-written-foralls
[94]: http://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/existential_quantification.html
[95]: https://wiki.haskell.org/Foreign_Function_Interface "Foreign Function Interface"
[96]: http://haskell.org/onlinereport/modules.html
[97]: https://wiki.haskell.org/Keywords#as
[98]: https://wiki.haskell.org/Keywords#hiding
[99]: https://wiki.haskell.org/Keywords#qualified
[100]: https://wiki.haskell.org/Import "Import"
[101]: http://haskell.org/onlinereport/decls.html
[102]: https://wiki.haskell.org/Type_signature "Type signature"
[103]: https://en.wikipedia.org/wiki/Let_expression
[104]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-mdo-notation
[105]: http://www.haskell.org/tutorial/modules.html
[106]: https://wiki.haskell.org/Type "Type"
[107]: https://wiki.haskell.org/Constructor "Constructor"
[108]: https://wiki.haskell.org/Smart_constructors "Smart constructors"
[109]: https://wiki.haskell.org/Smart_destructors "Smart destructors"
[110]: https://wiki.haskell.org/Type "Type"
[111]: https://wiki.haskell.org/Newtype "Newtype"
[112]: https://wiki.haskell.org/Arrow_notation "Arrow notation"
[113]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#the-mdo-notation
[114]: https://wiki.haskell.org/Type "Type"
[115]: https://wiki.haskell.org/Type "Type"
[116]: https://wiki.haskell.org/Type_families "Type families"
[117]: https://wiki.haskell.org/Type_families "Type families"
[118]: https://wiki.haskell.org/GADT "GADT"

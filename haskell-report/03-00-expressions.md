# 3. Expressions

https://www.haskell.org/onlinereport/haskell2010/haskellch3.html

Expressions
- let expressions
- lambda abstractions
- conditionals


In this chapter, we describe the syntax and informal semantics of Haskell expressions, including their translations into the Haskell kernel.

Except in the case of let expressions, these translations preserve both the static and dynamic semantics.

Free variables and constructors used in these translations always refer to entities defined by the Prelude. For example, `concatMap` used in the translation of list comprehensions means the concatMap defined by the Prelude, regardless of whether or not the identifier `concatMap` is in scope where the list comprehension is used, and (if it is in scope) what it is bound to. For opposite effect use `RebindableSyntax` langext.


++

Expressions involving infix operators are disambiguated by the fixity of the operator.

Consecutive unparenthesized operators with the same precedence must both be either left or right associative to avoid a syntax error.

Given an unparenthesized expression `x <qop(a,i)> y <qop(b,j)> z` (where `<qop(a,i)>` means an operator with associativity `a` and precedence `i`), parentheses must be added around either `x <qop(a,i)> y` or `y <qop(b,j)> z` when `i = j` unless `a = b = l` or `a = b = r`.

An example algorithm for resolving expressions involving infix operators is given in Section 10.6.

*Negation is the only prefix operator* in Haskell; it has the same precedence as the infix `-` operator defined in the Prelude (see Section 4.4.2).

The grammar is ambiguous regarding the extent of lambda abstractions, let expressions and conditionals. The ambiguity is resolved by the meta-rule that each of these constructs extends as far to the right as possible.

Sample parses:

This                      | Parses as
--------------------------|-------------------------
f x + g y                 | (f x) + (g y)
- f x + y                 | (- (f x)) + y
let { ... } in x + y      | let { ... } in (x + y)
z + let { ... } in x + y  | z + (let { ... } in (x + y))
f x y :: Int              | (f x y) :: Int
\ x -> a+b :: Int         | \ x -> ((a+b) :: Int)


```hs
f x + g y
(f x) + (g y)

- f x + y
(- (f x)) + y

let { ... } in x + y
let { ... } in (x + y)

z + let { ... } in x + y
z + (let { ... } in (x + y))

f x y :: Int
(f x y) :: Int

\x -> a+b :: Int
\ x -> ((a + b) :: Int)
```

For the sake of clarity, the rest of this section will assume that expressions involving infix operators have been resolved according to the fixities of the operators.




### 3.3 Curried Applications and Lambda Abstractions

++

Function application is written e1 e2. Application associates to the left, so the parentheses may be omitted in (f x) y. Because e1 could be a data constructor, partial applications of data constructors are allowed.

Lambda abstractions are written \\ p1 … pn \-> e, where the pi are patterns. An expression such as \\x:xs->x is syntactically incorrect; it may legally be written as \\(x:xs)->x.

The set of patterns must be linear—no variable may appear more than once in the set.

Translation: The following identity holds:

++

Given this translation combined with the semantics of case expressions and pattern matching described in Section [3.17.3](#x8-610003.17.3), if the pattern fails to match, then the result is ⊥.

### 3.4 Operator Applications

++

The form e1 qop e2 is the infix application of binary operator qop to expressions e1 and e2.

The special form \-e denotes prefix negation, the only prefix operator in Haskell, and is syntax for negate (e). The binary \- operator does not necessarily refer to the definition of \- in the Prelude; it may be rebound by the module system. However, unary \- will always refer to the negate function defined in the Prelude. There is no link between the local meaning of the \- operator and unary negation.

Prefix negation has the same precedence as the infix operator \- defined in the Prelude (see Table [4.1](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-820061)). Because e1-e2 parses as an infix application of the binary operator \-, one must write e1(-e2) for the alternative parsing. Similarly, (-) is syntax for (\\ x y -> x-y), as with any infix operator, and does not denote (\\ x -> -x)—one must use negate for that.

Translation: The following identities hold:

++

### 3.5 Sections

++

Sections are written as ( op e ) or ( e op ), where op is a binary operator and e is an expression. Sections are a convenient syntax for partial application of binary operators.

Syntactic precedence rules apply to sections as follows. (op e) is legal if and only if (x op e) parses in the same way as (x op (e)); and similarly for (e op). For example, (⋆a+b) is syntactically invalid, but (+a⋆b) and (⋆(a+b)) are valid. Because (+) is left associative, (a+b+) is syntactically correct, but (+a+b) is not; the latter may legally be written as (+(a+b)). As another example, the expression

is invalid because, by the let/lambda meta-rule (Section [3](#x8-220003)), the expression

parses as

rather than

Because \- is treated specially in the grammar, (- exp) is not a section, but an application of prefix negation, as described in the preceding section. However, there is a subtract function defined in the Prelude such that (subtract exp) is equivalent to the disallowed section. The expression (+ (- exp)) can serve the same purpose.

Translation: The following identities hold:

++

where op is a binary operator, e is an expression, and x is a variable that does not occur free in e.

### 3.6 Conditionals

++

A conditional expression has the form if e1 then e2 else e3 and returns the value of e2 if the value of e1 is True, e3 if e1 is False, and ⊥ otherwise.

Translation: The following identity holds:

++

where True and False are the two nullary constructors from the type Bool, as defined in the Prelude. The type of e1 must be Bool; e2 and e3 must have the same type, which is also the type of the entire conditional expression.

### 3.7 Lists

++

Lists are written \[e1, …, ek\], where k ≥ 1. The list constructor is :, and the empty list is denoted \[\]. Standard operations on lists are given in the Prelude (see Section [6.1.3](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch6.html#x13-1200006.1.3), and Chapter [9](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch9.html#x16-1710009) notably Section [9.1](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch9.html#x16-1720009.1)).

Translation: The following identity holds:

++

where : and \[\] are constructors for lists, as defined in the Prelude (see Section [6.1.3](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch6.html#x13-1200006.1.3)). The types of e1 through ek must all be the same (call it t), and the type of the overall expression is \[t\] (see Section [4.1.2](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-650004.1.2)).

The constructor “:” is reserved solely for list construction; like \[\], it is considered part of the language syntax, and cannot be hidden or redefined. It is a right-associative operator, with precedence level 5 (Section [4.4.2](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-820004.4.2)).

### 3.8 Tuples

++

Tuples are written (e1, …, ek), and may be of arbitrary length k ≥ 2. The constructor for an n\-tuple is denoted by (,…,), where there are n − 1 commas. Thus (a,b,c) and (,,) a b c denote the same value. Standard operations on tuples are given in the Prelude (see Section [6.1.4](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch6.html#x13-1210006.1.4) and Chapter [9](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch9.html#x16-1710009)).

Translation: (e1, …, ek) for k ≥ 2 is an instance of a k\-tuple as defined in the Prelude, and requires no translation. If t1 through tk are the types of e1 through ek, respectively, then the type of the resulting tuple is (t1, …, tk) (see Section [4.1.2](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-650004.1.2)).

### 3.9 Unit Expressions and Parenthesized Expressions

++

The form (e) is simply a parenthesized expression, and is equivalent to e. The unit expression () has type () (see Section [4.1.2](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-650004.1.2)). It is the only member of that type apart from ⊥, and can be thought of as the “nullary tuple” (see Section [6.1.5](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch6.html#x13-1220006.1.5)).

Translation: (e) is equivalent to e.

### 3.10 Arithmetic Sequences

++

The arithmetic sequence \[e1, e2 .. e3\] denotes a list of values of type t, where each of the ei has type t, and t is an instance of class Enum.

Translation: Arithmetic sequences satisfy these identities:

++

where enumFrom, enumFromThen, enumFromTo, and enumFromThenTo are class methods in the class Enum as defined in the Prelude (see Figure [6.1](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch6.html#x13-1270011)).

The semantics of arithmetic sequences therefore depends entirely on the instance declaration for the type t. See Section [6.3.4](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch6.html#x13-1310006.3.4) for more details of which Prelude types are in Enum and their semantics.

### 3.11 List Comprehensions

++

A list comprehension has the form \[ e | q1, …, qn \],n ≥ 1, where the qi qualifiers are either

*   generators of the form p <- e, where p is a pattern (see Section [3.17](#x8-580003.17)) of type t and e is an expression of type \[t\]
*   local bindings that provide new definitions for use in the generated expression e or subsequent boolean guards and generators
*   boolean guards, which are arbitrary expressions of type Bool.

Such a list comprehension returns the list of elements produced by evaluating e in the successive environments created by the nested, depth-first evaluation of the generators in the qualifier list. Binding of variables occurs according to the normal pattern matching rules (see Section [3.17](#x8-580003.17)), and if a match fails then that element of the list is simply skipped over. Thus:

\[ x |  xs   <- \[ \[(1,2),(3,4)\], \[(5,4),(3,2)\] \],    
      (3,x) <- xs \]

yields the list \[4,2\]. If a qualifier is a boolean guard, it must evaluate to True for the previous pattern match to succeed. As usual, bindings in list comprehensions can shadow those in outer scopes; for example:

++

Translation: List comprehensions satisfy these identities, which may be used as a translation into the kernel:

++

where e ranges over expressions, p over patterns, l over list-valued expressions, b over boolean expressions, decls over declaration lists, q over qualifiers, and Q over sequences of qualifiers. ok is a fresh variable. The function concatMap, and boolean value True, are defined in the Prelude.

As indicated by the translation of list comprehensions, variables bound by let have fully polymorphic types while those defined by <- are lambda bound and are thus monomorphic (see Section [4.5.4](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-920004.5.4)).

### 3.12 Let Expressions

Let expressions have the general form let { d1 ; … ; dn } in e, and introduce a nested, lexically-scoped, mutually-recursive list of declarations (let is often called letrec in other languages). The scope of the declarations is the expression e and the right hand side of the declarations. Declarations are described in Chapter [4](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-620004). Pattern bindings are matched lazily; an implicit ~ makes these patterns irrefutable. For example,

++

does not cause an execution-time error until x or y is evaluated.

Translation: The dynamic semantics of the expression let { d1 ; … ; dn } in e0 are captured by this translation: After removing all type signatures, each declaration di is translated into an equation of the form pi \= ei, where pi and ei are patterns and expressions respectively, using the translation in Section [4.4.3](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-830004.4.3). Once done, these identities hold, which may be used as a translation into the kernel:

++

where fix is the least fixpoint operator. Note the use of the irrefutable patterns ~p. This translation does not preserve the static semantics because the use of case precludes a fully polymorphic typing of the bound variables. The static semantics of the bindings in a let expression are described in Section [4.4.3](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-830004.4.3).

### 3.13 Case Expressions

++

A case expression has the general form case e of { p1 match1 ; … ; pn matchn } where each matchi is of the general form

++

(Notice that in the syntax rule for guards, the “|” is a terminal symbol, not the syntactic metasymbol for alternation.) Each alternative pi matchi consists of a pattern pi and its matches, matchi. Each match in turn consists of a sequence of pairs of guards gsij and bodies eij (expressions), followed by optional bindings (declsi) that scope over all of the guards and expressions of the alternative.

A guard has one of the following forms:

*   pattern guards are of the form p <- e, where p is a pattern (see Section [3.17](#x8-580003.17)) of type t and e is an expression type t[1](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskell9.html#fn1x5). They succeed if the expression e matches the pattern p, and introduce the bindings of the pattern to the environment.
*   local bindings are of the form let decls. They always succeed, and they introduce the names defined in decls to the environment.
*   boolean guards are arbitrary expressions of type Bool. They succeed if the expression evaluates to True, and they do not introduce new names to the environment. A boolean guard, g, is semantically equivalent to the pattern guard True <- g.

An alternative of the form pat \-> exp where decls is treated as shorthand for:

++

A case expression must have at least one alternative and each alternative must have at least one body. Each body must have the same type, and the type of the whole expression is that type.

A case expression is evaluated by pattern matching the expression e against the individual alternatives. The alternatives are tried sequentially, from top to bottom. If e matches the pattern of an alternative, then the guarded expressions for that alternative are tried sequentially from top to bottom in the environment of the case expression extended first by the bindings created during the matching of the pattern, and then by the declsi in the where clause associated with that alternative.

For each guarded expression, the comma-separated guards are tried sequentially from left to right. If all of them succeed, then the corresponding expression is evaluated in the environment extended with the bindings introduced by the guards. That is, the bindings that are introduced by a guard (either by using a let clause or a pattern guard) are in scope in the following guards and the corresponding expression. If any of the guards fail, then this guarded expression fails and the next guarded expression is tried.

If none of the guarded expressions for a given alternative succeed, then matching continues with the next alternative. If no alternative succeeds, then the result is ⊥. Pattern matching is described in Section [3.17](#x8-580003.17), with the formal semantics of case expressions in Section [3.17.3](#x8-610003.17.3).

A note about parsing. The expression

  case x of { (a,\_) | let b = not a in b :: Bool -> a }

is tricky to parse correctly. It has a single unambiguous parse, namely

  case x of { (a,\_) | (let b = not a in b :: Bool) -> a }

However, the phrase Bool -> a is syntactically valid as a type, and parsers with limited lookahead may incorrectly commit to this choice, and hence reject the program. Programmers are advised, therefore, to avoid guards that end with a type signature — indeed that is why a guard contains an infixexp not an exp.

### 3.14 Do Expressions

++

A do expression provides a more conventional syntax for monadic programming. It allows an expression such as

  putStr "x: "    >>    
  getLine         >>= \\l ->    
  return (words l)

to be written in a more traditional way as:

  do putStr "x: "    
     l <- getLine    
     return (words l)

Translation: Do expressions satisfy these identities, which may be used as a translation into the kernel, after eliminating empty stmts:

++

The ellipsis "..." stands for a compiler-generated error message, passed to fail, preferably giving some indication of the location of the pattern-match failure; the functions \>>, \>>=, and fail are operations in the class Monad, as defined in the Prelude; and ok is a fresh identifier.

As indicated by the translation of do, variables bound by let have fully polymorphic types while those defined by <- are lambda bound and are thus monomorphic.

### 3.15 Datatypes with Field Labels

A datatype declaration may optionally define field labels (see Section [4.2.1](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-690004.2.1)). These field labels can be used to construct, select from, and update fields in a manner that is independent of the overall structure of the datatype.

Different datatypes cannot share common field labels in the same scope. A field label can be used at most once in a constructor. Within a datatype, however, a field label can be used in more than one constructor provided the field has the same typing in all constructors. To illustrate the last point, consider:

  data S = S1 { x :: Int } | S2 { x :: Int }   -- OK    
  data T = T1 { y :: Int } | T2 { y :: Bool }  -- BAD

Here S is legal but T is not, because y is given inconsistent typings in the latter.

#### 3.15.1 Field Selection

Field labels are used as selector functions. When used as a variable, a field label serves as a function that extracts the field from an object. Selectors are top level bindings and so they may be shadowed by local variables but cannot conflict with other top level bindings of the same name. This shadowing only affects selector functions; in record construction (Section [3.15.2](#x8-520003.15.2)) and update (Section [3.15.3](#x8-540003.15.3)), field labels cannot be confused with ordinary variables.

Translation: A field label f introduces a selector function defined as:

++

where C1 … Cn are all the constructors of the datatype containing a field labeled with f, pij is y when f labels the jth component of Ci or \_ otherwise, and ei is y when some field in Ci has a label of f or undefined otherwise.

#### 3.15.2 Construction Using Field Labels

++

A constructor with labeled fields may be used to construct a value in which the components are specified by name rather than by position. Unlike the braces used in declaration lists, these are not subject to layout; the { and } characters must be explicit. (This is also true of field updates and field patterns.) Construction using field labels is subject to the following constraints:

*   Only field labels declared with the specified constructor may be mentioned.
*   A field label may not be mentioned more than once.
*   Fields not mentioned are initialized to ⊥.
*   A compile-time error occurs when any strict fields (fields whose declared types are prefixed by !) are omitted during construction. Strict fields are discussed in Section [4.2.1](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-710004.2.1).

The expression F {}, where F is a data constructor, is legal whether or not F was declared with record syntax (provided F has no strict fields — see the fourth bullet above); it denotes F ⊥1 … ⊥n, where n is the arity of F.

Translation: In the binding f \= v, the field f labels v.

++

where k is the arity of C.

The auxiliary function pickiC bs d is defined as follows:

If the ith component of a constructor C has the field label f, and if f \= v appears in the binding list bs, then pickiC bs d is v. Otherwise, pickiC bs d is the default value d.

#### 3.15.3 Updates Using Field Labels

++

Values belonging to a datatype with field labels may be non-destructively updated. This creates a new value in which the specified field values replace those in the existing value. Updates are restricted in the following ways:

*   All labels must be taken from the same datatype.
*   At least one constructor must define all of the labels mentioned in the update.
*   No label may be mentioned more than once.
*   An execution error occurs when the value being updated does not contain all of the specified labels.

Translation: Using the prior definition of pick,

++

where {C1,…,Cj} is the set of constructors containing all labels in bs, and ki is the arity of Ci.

Here are some examples using labeled fields:

data T    = C1 {f1,f2 :: Int}    
          | C2 {f1 :: Int,    
                f3,f4 :: Char}

++

The field f1 is common to both constructors in T. This example translates expressions using constructors in field-label notation into equivalent expressions using the same constructors without field labels. A compile-time error will result if no single constructor defines the set of field labels used in an update, such as x {f2 = 1, f3 = 'x'}.

### 3.16 Expression Type-Signatures

++

Expression type-signatures have the form e :: t, where e is an expression and t is a type (Section [4.1.2](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-650004.1.2)); they are used to type an expression explicitly and may be used to resolve ambiguous typings due to overloading (see Section [4.3.4](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-790004.3.4)). The value of the expression is just that of exp. As with normal type signatures (see Section [4.4.1](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-810004.4.1)), the declared type may be more specific than the principal type derivable from exp, but it is an error to give a type that is more general than, or not comparable to, the principal type.

Translation:

++

### 3.17 Pattern Matching

Patterns appear in lambda abstractions, function definitions, pattern bindings, list comprehensions, do expressions, and case expressions. However, the first five of these ultimately translate into case expressions, so defining the semantics of pattern matching for case expressions is sufficient.

#### 3.17.1 Patterns

Patterns have this syntax:

++

The arity of a constructor must match the number of sub-patterns associated with it; one cannot match against a partially-applied constructor.

All patterns must be linear —no variable may appear more than once. For example, this definition is illegal:

f (x,x) = x     -- ILLEGAL; x used twice in pattern

Patterns of the form var@pat are called as-patterns, and allow one to use var as a name for the value being matched by pat. For example,

case e of { xs@(x:rest) -> if x==0 then rest else xs }

is equivalent to:

let { xs = e } in    
  case xs of { (x:rest) -> if x==0 then rest else xs }

Patterns of the form \_ are wildcards and are useful when some part of a pattern is not referenced on the right-hand-side. It is as if an identifier not used elsewhere were put in its place. For example,

case e of { \[x,\_,\_\]  ->  if x==0 then True else False }

is equivalent to:

case e of { \[x,y,z\]  ->  if x==0 then True else False }

#### 3.17.2 Informal Semantics of Pattern Matching

Patterns are matched against values. Attempting to match a pattern can have one of three results: it may fail; it may succeed, returning a binding for each variable in the pattern; or it may diverge (i.e. return ⊥). Pattern matching proceeds from left to right, and outside to inside, according to the following rules:

1.  Matching the pattern var against a value v always succeeds and binds var to v.
2.  Matching the pattern ~apat against a value v always succeeds. The free variables in apat are bound to the appropriate values if matching apat against v would otherwise succeed, and to ⊥ if matching apat against v fails or diverges. (Binding does not imply evaluation.)
    
    Operationally, this means that no matching is done on a ~apat pattern until one of the variables in apat is used. At that point the entire pattern is matched against the value, and if the match fails or diverges, so does the overall computation.
    
3.  Matching the wildcard pattern \_ against any value always succeeds, and no binding is done.
4.  Matching the pattern con pat against a value, where con is a constructor defined by newtype, depends on the value:
    
    *   If the value is of the form con v, then pat is matched against v.
    *   If the value is ⊥, then pat is matched against ⊥.
    
    That is, constructors associated with newtype serve only to change the type of a value.
    
5.  Matching the pattern con pat1 … patn against a value, where con is a constructor defined by data, depends on the value:
    *   If the value is of the form con v1 … vn, sub-patterns are matched left-to-right against the components of the data value; if all matches succeed, the overall match succeeds; the first to fail or diverge causes the overall match to fail or diverge, respectively.
    *   If the value is of the form con′ v1 … vm, where con is a different constructor to con′, the match fails.
    *   If the value is ⊥, the match diverges.
6.  Matching against a constructor using labeled fields is the same as matching ordinary constructor patterns except that the fields are matched in the order they are named in the field list. All fields listed must be declared by the constructor; fields may not be named more than once. Fields not named by the pattern are ignored (matched against \_).
7.  Matching a numeric, character, or string literal pattern k against a value v succeeds if v  \==  k, where \== is overloaded based on the type of the pattern. The match diverges if this test diverges.
    
    The interpretation of numeric literals is exactly as described in Section [3.2](#x8-240003.2); that is, the overloaded function fromInteger or fromRational is applied to an Integer or Rational literal (resp) to convert it to the appropriate type.
    
8.  Matching an as-pattern var@apat against a value v is the result of matching apat against v, augmented with the binding of var to v. If the match of apat against v fails or diverges, then so does the overall match.

Aside from the obvious static type constraints (for example, it is a static error to match a character against a boolean), the following static class constraints hold:

*   An integer literal pattern can only be matched against a value in the class Num.
*   A floating literal pattern can only be matched against a value in the class Fractional.

It is sometimes helpful to distinguish two kinds of patterns. Matching an irrefutable pattern is non-strict: the pattern matches even if the value to be matched is ⊥. Matching a refutable pattern is strict: if the value to be matched is ⊥ the match diverges. The irrefutable patterns are as follows: a variable, a wildcard, N apat where N is a constructor defined by newtype and apat is irrefutable (see Section [4.2.3](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-740004.2.3)), var@apat where apat is irrefutable, or of the form ~apat (whether or not apat is irrefutable). All other patterns are refutable.

Here are some examples:

1.  If the pattern \['a','b'\] is matched against \['x',⊥\], then 'a' fails to match against 'x', and the result is a failed match. But if \['a','b'\] is matched against \[⊥,'x'\], then attempting to match 'a' against ⊥ causes the match to diverge.
2.  These examples demonstrate refutable vs. irrefutable matching:
    
++
    
++
    
++
    
++
    
3.  Consider the following declarations:
    
      newtype N = N Bool    
      data    D = D !Bool
    
    These examples illustrate the difference in pattern matching between types defined by data and newtype:
    
++
    
    Additional examples may be found in Section [4.2.3](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-740004.2.3).
    

Top level patterns in case expressions and the set of top level patterns in function or pattern bindings may have zero or more associated guards. See Section [3.13](#x8-460003.13) for the syntax and semantics of guards.

The guard semantics have an influence on the strictness characteristics of a function or case expression. In particular, an otherwise irrefutable pattern may be evaluated because of a guard. For example, in

f :: (Int,Int,Int) -> \[Int\] -> Int    
f ~(x,y,z) \[a\] | (a == y) = 1

both a and y will be evaluated by \== in the guard.

#### 3.17.3 Formal Semantics of Pattern Matching

The semantics of all pattern matching constructs other than case expressions are defined by giving identities that relate those constructs to case expressions. The semantics of case expressions themselves are in turn given as a series of identities, in Figures [3.1](#x8-610011)–[3.3](#x8-610033). Any implementation should behave so that these identities hold; it is not expected that it will use them directly, since that would generate rather inefficient code.

* * *

++

Figure 3.1: Semantics of Case Expressions, Part 1

* * *

* * *

++

Figure 3.2: Semantics of Case Expressions, Part 2

* * *

* * *

++

Figure 3.3: Semantics of Case Expressions, Part 3

* * *

In Figures [3.1](#x8-610011)–[3.3](#x8-610033): e, e′ and ei are expressions; gi and gsi are guards and sequences of guards respecively; p and pi are patterns; v, x, and xi are variables; K and K′ are algebraic datatype (data) constructors (including tuple constructors); and N is a newtype constructor.

Rule (b) matches a general source-language case expression, regardless of whether it actually includes guards—if no guards are written, then True is substituted for the guards gsi,j in the matchi forms. Subsequent identities manipulate the resulting case expression into simpler and simpler forms.

Rule (h) in Figure [3.2](#x8-610022) involves the overloaded operator \==; it is this rule that defines the meaning of pattern matching against overloaded constants.

These identities all preserve the static semantics. Rules (d), (e), (j), and (q) use a lambda rather than a let; this indicates that variables bound by case are monomorphically typed (Section [4.1.4](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/haskellch4.html#x10-670004.1.4)).

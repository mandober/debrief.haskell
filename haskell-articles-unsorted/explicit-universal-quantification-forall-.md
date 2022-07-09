# 6.11.1. Explicit universal quantification (forall) — Glasgow Haskell Compiler 9.3.20210805 User's Guide

> Allow use of the forall keyword in places where universal quantification
is implicit.

[Glasgow Haskell Compiler](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/index.html)

`ExplicitForAll`[¶](#extension-ExplicitForAll "Permalink to this definition")

<table><colgroup><col> <col></colgroup><tbody><tr><th>Since:</th><td>6.12.1</td></tr></tbody></table>

Allow use of the `forall` keyword in places where universal quantification is implicit.

Haskell type signatures are implicitly quantified. When the language option [`ExplicitForAll`](#extension-ExplicitForAll) is used, the keyword `forall` allows us to say exactly what this means. For example:

means this:

The two are treated identically, except that the latter may bring type variables into scope (see [Lexically scoped type variables](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/scoped_type_variables.html#scoped-type-variables)).

This extension also enables explicit quantification of type and kind variables in [Data instance declarations](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#data-instance-declarations), [Type instance declarations](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#type-instance-declarations), [Closed type families](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#closed-type-families), [Associated instances](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#assoc-inst), and [Rewrite rules](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/rewrite_rules.html#rewrite-rules).

Notes:

*   As well as in type signatures, you can also use an explicit `forall` in an instance declaration:
    
    instance forall a. Eq a \=> Eq \[a\] where ...
    
    Note that the use of `forall`s in instance declarations is somewhat restricted in comparison to other types. For example, instance declarations are not allowed to contain nested `forall`s. See [Formal syntax for instance declaration types](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/instances.html#formal-instance-syntax) for more information.
    
*   If the [`-Wunused-foralls`](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/using-warnings.html#ghc-flag--Wunused-foralls) flag is enabled, a warning will be emitted when you write a type variable in an explicit `forall` statement that is otherwise unused. For instance:
    
    g :: forall a b. (b \-> b)
    
    would warn about the unused type variable a.
    

6.11.1.1. The `forall`\-or-nothing rule[¶](#the-forall-or-nothing-rule "Permalink to this headline")
----------------------------------------------------------------------------------------------------

In certain forms of types, type variables obey what is known as the “`forall`\-or-nothing” rule: if a type has an outermost, explicit, invisible `forall`, then all of the type variables in the type must be explicitly quantified. These two examples illustrate how the rule works:

f  :: forall a b. a \-> b \-> b         \-- OK, \`a\` and \`b\` are explicitly bound
g  :: forall a. a \-> forall b. b \-> b \-- OK, \`a\` and \`b\` are explicitly bound
h  :: forall a. a \-> b \-> b           \-- Rejected, \`b\` is not in scope

The type signatures for `f`, `g`, and `h` all begin with an outermost invisible `forall`, so every type variable in these signatures must be explicitly bound by a `forall`. Both `f` and `g` obey the `forall`\-or-nothing rule, since they explicitly quantify `a` and `b`. On the other hand, `h` does not explicitly quantify `b`, so GHC will reject its type signature for being improperly scoped.

In places where the `forall`\-or-nothing rule takes effect, if a type does _not_ have an outermost invisible `forall`, then any type variables that are not explicitly bound by a `forall` become implicitly quantified. For example:

i :: a \-> b \-> b             \-- \`a\` and \`b\` are implicitly quantified
j :: a \-> forall b. b \-> b   \-- \`a\` is implicitly quantified
k :: (forall a. a \-> b \-> b) \-- \`b\` is implicitly quantified
type L :: forall a \-> b \-> b \-- \`b\` is implicitly quantified

GHC will accept `i`, `j`, and `k`‘s type signatures, as well as `L`‘s kind signature. Note that:

*   `j`‘s signature is accepted despite its mixture of implicit and explicit quantification. As long as a `forall` is not an outermost one, it is fine to use it among implicitly bound type variables.
*   `k`‘s signature is accepted because the outermost parentheses imply that the `forall` is not an outermost `forall`. The `forall`\-or-nothing rule is one of the few places in GHC where the presence or absence of parentheses can be semantically significant!
*   `L`‘s signature begins with an outermost `forall`, but it is a _visible_ `forall`, not an invisible `forall`, and therefore does not trigger the `forall`\-or-nothing rule.

The `forall`\-or-nothing rule takes effect in the following places:

*   Type signature declarations for functions, values, and class methods
*   Expression type annotations
*   Instance declarations
*   [Default method signatures](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/default_signatures.html#class-default-signatures)
*   Type signatures in a [SPECIALIZE pragma](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/pragmas.html#specialize-pragma) or [SPECIALIZE instance pragma](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/pragmas.html#specialize-instance-pragma)
*   [Standalone kind signatures and polymorphic recursion](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/poly_kinds.html#standalone-kind-signatures)
*   Type signatures for [Generalised Algebraic Data Types (GADTs)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/gadt.html#gadt) constructors
*   Type signatures for [Pattern synonyms](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/pattern_synonyms.html#pattern-synonyms)
*   [Data instance declarations](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#data-instance-declarations), [Type instance declarations](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#type-instance-declarations), [Closed type families](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#closed-type-families), and [Associated instances](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/type_families.html#assoc-inst)
*   [Rewrite rules](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/rewrite_rules.html#rewrite-rules) in which the type variables are explicitly quantified

Notes:

*   [Pattern type signatures](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/scoped_type_variables.html#pattern-type-sigs) are a notable example of a place where types do _not_ obey the `forall`\-or-nothing rule. For example, GHC will accept the following:
    
    f (g :: forall a. a \-> b) x \= g x :: b
    
    Furthermore, [Rewrite rules](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/rewrite_rules.html#rewrite-rules) do not obey the `forall`\-or-nothing rule when their type variables are not explicitly quantified:
    
    {-# RULES "f" forall (g :: forall a. a -> b) x. f g x = g x :: b #-}
    
*   GADT constructors are extra particular about their `forall`s. In addition to adhering to the `forall`\-or-nothing rule, GADT constructors also forbid nested `forall`s. For example, GHC would reject the following GADT:
    
    data T where
      MkT :: (forall a. a \-> b \-> T)
    
    Because of the lack of an outermost `forall` in the type of `MkT`, the `b` would be implicitly quantified. In effect, it would be as if one had written `MkT :: forall b. (forall a. a -> b -> T)`, which contains nested `forall`s. See [Formal syntax for GADTs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/gadt_syntax.html#formal-gadt-syntax).


[Source](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/explicit_forall.html)
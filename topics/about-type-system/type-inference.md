# Type inference

- lambda calculi
- untyped lambda calculus
- simply typed lambda calculus
- polymorphic lambda calculus
- System F
- let-binding
- let-polymorphism
- type genralization
- base types
- function types
- monomorphism
- monomorphic types
- polymorphism
- monotypes
- polytypes
- type checking
- type inference
- type unification
- principal type
- the most general unifier (mgu)
- type environment
- substitution


To implement the algorithm for type inference, we'll start with the untyped lambda calculus, and by adding new geatures we'll work our way towards typed LC variants, ending with a variant of System F as the pinnacle (concerning this project; there are more complex lambda calculi built on top of System F).


## Untyped lambda calculus

In any untyped variant of the lambda calculus, there are no types to worry about, we only deal with term-level expressions or term-level syntax.

Lambda calculus is usually defined as having 3 sorts of terms:
- variables
- lambda abstraction
- lambda application

**Variables** are actually *formal paramaters* of a function (lambda abstraction). Parameters may be divided by their occurances within an abstraction into a param's *binding occurance* (within a function's head), and one or more param's *application occurances* (within a function's body).

A **lambda abstraction** is an anonymous unary function. In fact, one of the strict rules of LC is that all abstractions are anonymous functions; also all abstractions take exactly one input parameter and return exactly one value. The fact that all functions are unary is of no particular concern due to currying, and the fact that all functions are nameless is only a problem of inconvenince, which can be eased by introducing term-abbreviations in the form of local let-bindings.

**Lambda application** is the act of applying an abstraction (on the left side) to a lambda term (on the right side), which then becomes an argument to that function. This act is called *beta-reduction* and it results in a reduced, usually simplified, term. The mechanics of application are the same as in mathematics, that is, application is defined in terms of *substitution*. There is, however, a special case: when performing substitutions we must be careful to distinguish between the bound and free variables, in order to avoid a *name capture*.


```hs
data Term = Var String | App Term Term | Lam String Term
```

Lambda `Term`s make a binary tree:
- `Var`: variables are leaves and hold a string (var name)
- `App`: application is represented by internal nodes
- `Abs`: abstraction is a special kind of node whose left child is a variable   
   (or, as a single-child node labelled with a variable)


```
Term := Var | Abs | App

Var := "x" | "y" | …
Abs := "λ" Var "." Term
App := App Term Term

     @            λ
    / \          / \
Term   Term   "a"   Term


(λx.x) y := App (Abs "x" (Var "x")) (Var "y")

       App
       / \
     Abs  Var "y"
     / \
  "x"   Var "x"
```

Rules and conventions:
- application has higher precedence then abstraction
- application is left-associative, `E₁ E₂ E₃` = `(E₁ E₂) E₃`
- application, `App E₁ E₂`, has 2 child nodes, both are lambda `Term`s
- abstraction is right-associative, `λx. λy. λz. E` = `λx.(λy.(λz. E))`
- abstraction, `Abs x E`, has 2 child nodes:
  - the left child, `x`, is a leaf repr a variable, `Var "x"`
  - the right child, `E`, is a node repr a lambda `Term`
- abstraction is prefixed with a `λ`, called a binder
- abstraction's left and right child nodes are separated by dots (`.`)
- optionally, consecutive abstractions, e.g. `λx.λy.λz.E`, can omit all lambdas but the first, and all dots but the last, `λx y z.E`
- abstraction that is the right child of application is enclosed in parens


The `Show` instance for `Term` first identifies each data ctors (`Var`, `App`, `Lam`), by pattern-matching; then, the RHS specifies what to do in each case:
* `Var "x"`: since variables are terminators (leafs), we show it by printing the string it holds, e.g. `"x"`.
* `Abs "x" t`: in case of abstraction, we print its left child as `"λx . "`, then we call `show t` in order to print the right child `t`, which is a `Term`.
* `App t₁ t₂ `: in application, both children are `Term`s, so we recursively call `show` with each child, `show t₁ ++ show t₂`.


The following is a crude instance for `Show` that doesn't make much considerations, treating all the next terms equally, whether they require a special treatment or not. For example, after identifying `App` on the LHS, we could treat specially the next term, if it's `App` or `Abs`, by enclosing it in parens, but if it's `Var` we could leave the parens out, thereby making the entire expression more readable.

```hs
instance Show Term where
  show (Var s)   = s
  show (Abs s t) = "λ" ++ s ++ " . " ++ show t
  show (App x y) = "(" ++ show x ++ ")" ++ " " ++ "(" ++ show y ++ ")"
```

## Parsing

We allow some conveninces from Haskell: we interpret a backslash as a lambda binder, and we can also accept `->` along with a dot as a separator between a lambda's head and body. Variable name have the usual rules for identifiers.

We also introduce abbreviations for tems i nthe form of let bindings, which are purely syntactic abbreviations (macros). The macro `TRUE = \x y -> x` defined the term `TRUE`, such that, in all the subsequent terms, whenever we see the term `TRUE` we must substitute it for its LHS definition, i.e. `(\x y -> x)`. There is an exception to this rule, but we'll try to avoid it altoghether: namely, if a subexpession contains a formal parameter named `TRUE`, then it shadows the eponymous macro (which, after all, looks just like another variable) within its scope.

The parser accepts
- empty lines
- let-definitions (macros)
- proper lambda terms that should be evaluated immediately


```hs
data LambdaLine = Blank | Let String Term | Run Term
```


## Evaluation

We now pattern-match each of the 3 data ctors:
- if the root node is a free variable (`Var`), there's nothing to do
  it's like a lambda epression, `x`
- if the root node is a lambda (`Abs`), there's nothing to do
  it's like a lambda epression, `λx.x`
- if the root node is an `App`, we know it has 2 children.

So the term is `App f a` and we know its two childred, `f` and `a`, are both `Term`s. However, an application makes sense only if the left child, `f`, is an `Abs`; the right child, `a`, is then an argument to the abstraction.

Thus, we must scrutinize the left child of `App` node to make sure that it is an `Abs`, otherwise, we issue an error. If it is indeed an abstraction, then we know we have a `Abs p b`: the left child, `p`, is a String (e.g. `"x"`), representing the abstraction's binder, and the right child, `b`, is a `Term` representing the abstraction's body (e.g. `Var "x"`).

* An example lambda expr `(λx.x) y` in all 3 representations.

```
(λx.x) y
≡
App (Abs "x" (Var "x")) (Var "y")
≡
       App
       / \
     Abs  Var "y"
     / \
  "x"   Var "x"
```

The lambda expr, `(λx.x) y`, has `App` as the root node, whose two children are `Abs` (left) and `Var` (right); if `Abs` wasn't on the left, it would be an error. The `Abs` node also has two children, string `"x"` (left) acting as a formal param, and a `Var "x"` (right) acting as a body, indicating that it represents the identity function `λx.x`. The right child of the `App` node, `Var "y"`, is an arg to this function.


### The `eval` function

The `eval` function takes a `Term` and returns a `Term`. However, it also needs to take a mapping of `String` (key) to `Terms` (value), which is our environment in which we record the values of variables. The simplest type that we can use for the mapping is `[(String, Term)]`, aka an associative list. With that the complete signature of the `eval` function is

```hs
eval :: [(String, Term)] -> Term -> Term
```

Initially, the associative list, `env`, will be empty, `[]`.

As we said, the root node better be an `App f x`, and its left child, `f` better be an `Abs p b`, while its right child `x` can be any `Term`. Examining the term `Abs p b` itelf, `p` repr a formal parameter and `b` stands for the function's body.

Again, with root node determined to be an `App f x`, we first recursively call `eval` on its left child `f` to make sure it is an `Abs`; if it's anything else, we abort since something is wrong (a free variable got in the way).

We have to remember to pass the empty environment with the firt call to `eval`, and then to pass the variable `env` with each recursive call to `eval` therein.

```hs
eval :: [(String, Term)] -> Term -> Term
eval env (App f arg) = case eval env f of
  Abs par body -> eval env (beta env (par, arg) body)
  _            -> error "[eval] App's left child is not Abs."
-- …

beta :: [(String, Term)]        -- env
     -> (String, Term)          -- (param, arg) or param ⟼ arg
     -> Term                    -- body
     -> Term
beta env (param, arg) body = case body of
    Var s   | s == param   -> arg      -- return arg:  (λx.x) a ~> a
            | otherwise    -> body     -- return body: (λx.b) a ~> b
    -- …
```

Right, we have a root node `App f arg`, so we call `eval` passing it the current environment and the left child: `eval env f`. We case the result to see if `f` resolves to an `Abs`; otherwise we issue an errror. If `f` does repr an abstraction `Abs par body`, before we call `eval` on the current environment and (ellipsis), `eval env …`, we have to scrutinize what the `body` term is.

If the term `body` is a `Var s`, we need to compare `p` (it is a `String` repr the paramater of the `Abs`) and `s` (the `String` in `Var s`):
- if `p == s` then we have a lambda application like `(λp.p) y`, i.e. an abstraction whose body is the same as its formal parameter; this type of abstraction always reduces to the argument, `y`, so that is what is returned from `beta`.
- if `p /= s` then we have a lambda application like `(λp.b) a`, where the formal param `p` does not appear in the body at all; this always reduces to the body `b` no matter what the argument `a` is, so we return the body term (which we just established is of the `Var s` form) from `beta`; we could have returned either `body` or `Var s`.


```
(λp.p) a  ~~> a                (λp.b) a ~~> b
                                        
App                            App
  (Abs                          (Abs
    "p"                           "p"
    (Var "p"))                    (Var "b"))
  (Var "a")                     (Var "a")
                                         
       App                       App
       / \                       / \
     Abs  Var "a"             Abs   Var "a"
     / \                      / \
  "p"   Var "p"            "p"   Var "b"
```


Otherwise, we perform a beta-reduction:
if the left child is an (abstraction) `λv.B`,
and the right child is (argument) `E`,
then we traverse the `Abs`'s right subtree, `B`, 
replacing every application occurrence of the parameter `v` 
with the argument `E`, denoted by `[v ⟼ E]B`.



For example, 
if the left child is `λx.x` (abstraction) 
and the right child is `λy.y` (also abstraction), 
then, we have an expression `(λx.x)(λy.y)` 
where the left term (also the left child of App) 
is the abstraction `(λx.x)` acting as a function; 
and the right term (also the right child of App) 
is the abstraction `(λy.y)` acting as an argument.
The `Abs` is a node with 2 children: 
the first Abs has the binder `λx` as its left child (head) 
and its right child is the term `x` (body).
The other term, `λy.y`, is also an Abs such as this one 
but since it acts as an arg, we need not analyze it futher, 
for it will be substituted as a whole.
We then replace every application occurrence of the formal param `x` 
within the body, `x`, with the term argument `λy.y`, 
which reduces to the term `λy.y`.
This substitution is denoted by `[x/λy.y]x`.

Another form to denote substitutions is    
`let 𝒑𝒂𝒓𝒂𝒎 = 𝒂𝒓𝒈 in 𝒃𝒐𝒅𝒚`   
which corresponds to the form   
`[𝒑𝒂𝒓𝒂𝒎/𝒂𝒓𝒈]𝒃𝒐𝒅𝒚`
We use a form of let binding, without the "in" part, to define macros:
`let 𝒏𝒂𝒎𝒆 = 𝒕𝒆𝒓𝒎`


```
(λx.x) y               | App (Abs "x" (Var "x")) (Var "y")
~~> y     via [y/x]x   | ~~> (Var "y")


    App
    / \
  Abs  Var "x"
  /  \
"x"   Var "y"
```

## Free variables

The set of free variables in a given lambda term `M`, denoted `fv(M)` is recursively defined as follows:
- `fv[x] = x`
- `fv[(MN)] = fv(M) ⋃ fv(N)`
- `fv[(λx.M)] = fv(M) \ {x}`

# 3.4. Interactive evaluation

304-interactive-evaluation.md


## Interactive evaluation at the prompt

When you type an expression at the prompt, GHCi immediately evaluates and prints the result:

```bash
ghci> reverse "hello"
"olleh"
ghci> 5+5
10
```

### IO actions at the prompt

GHCi does more than simple expression evaluation at the prompt. If you enter an expression of type `IO a` for some `a`, then GHCi *executes* it as an **IO-computation**.

```bash
ghci> "hello"
"hello"
ghci> putStrLn "hello"
hello
```

This works even if the type of the expression is more general, provided it can be *instantiated* to `IO a`. For example,

```bash
ghci> return True
True
```

Furthermore, GHCi will print the result of the I/O action iff:
- the result type is an instance of `Show`
- the result type is not `()`


For example, remembering that `putStrLn :: String -> IO ()`:

```bash
ghci> putStrLn "hello"
hello
ghci> do { putStrLn "hello"; return "yes" }
hello
"yes"
```

### Using do-notation at the prompt

Actually, *GHCi accepts even statements* rather than just expressions at the prompt. This means you can bind values and functions to names, and use them in future expressions or statements.

The syntax of a statement accepted at the GHCi prompt is exactly the same as the syntax of a statement in a Haskell do-expression. However, there's *no monad overloading* here: statements typed at the prompt must be in the IO monad.

```bash
ghci> x <- return 42
ghci> print x
42
ghci>
```

The statement `x <- return 42` means "execute `return 42` in the IO monad, and bind the result to x". We can then use `x` in future statements, for example to print it as we did above.

`-fprint-bind-result` Turns on printing of binding results in GHCi ghci-stmts (dynamic flag).

If set then GHCi will print the result of a statement iff:
- The statement is not a binding, or it is a monadic binding (`p <- e`) that binds exactly one variable.
- The variable's type is not polymorphic, is not `()`, and is an instance of `Show`


* You can also bind normal non-IO expressions using the *let-statement*:

```bash
ghci> let x = 42
ghci> x
42
ghci>
```

* Another important difference between the two types of binding is that the **monadic bind is strict** (it evaluates `e` in `p <- e`), whereas with the let form, the expression isn't evaluated immediately. let-bindings do not automatically print the value bound, unlike monadic bindings.

```bash
ghci> let x = error "help!"
ghci> print x
*** Exception: help!
ghci>
```


* You can also define functions at the prompt:

```bash
ghci> add a b = a + b
ghci> add 1 2
3
ghci>
```

However, this quickly gets tedious when defining functions with multiple clauses, or groups of mutually recursive functions, because the complete definition has to be given on a single line, using explicit semicolons instead of layout:

```bash
ghci> f op n [] = n ; f op n (h:t) = h `op` f op n t
ghci> f (+) 0 [1..3]
6
ghci>
```

To alleviate this issue, GHCi commands can be split over multiple lines, by wrapping them in `:{` and `:}` (each on a single line of its own):

```bash
ghci> :{
| g op n [] = n
| g op n (h:t) = h `op` g op n t
| :}
ghci> g (*) 1 [1..3]
6
```

* Such multiline commands can be used with any GHCi command, and the layout rule is in effect. The main purpose of *multiline commands* is not to replace module loading but to make definitions in `.ghci`-files (ghci-dot-files) more readable and maintainable.


* Any exceptions raised during the evaluation or execution of the statement are caught and printed by the GHCi command line interface.

* Every new binding shadows any existing bindings of the same name, including entities that are in scope in the current module context.

Warning: Temporary bindings introduced at the prompt only last until the next `:load` or `:reload` command, at which time they will be simply lost. However, they do survive a change of context with `:module` - the temporary bindings just move to the new location.


* To get a list of the bindings currently in scope, use the `:show bindings`

```bash
ghci> :show bindings
x :: Int
ghci>
```

* If you turn on the `+t` option, GHCi will show the type of each variable bound by a statement. For example:

```bash
ghci> :set +t
ghci> let (x:xs) = [1..]
x :: Integer
xs :: [Integer]
```

### Multiline input

Apart from the `:{ ... :}` syntax for multi-line input mentioned above,
GHCi also has a multiline mode, enabled by `:set +m`, `:set +m` in which
GHCi detects automatically when the current statement is unfinished and
allows further lines to be added. A multi-line input is terminated with
an empty line. For example:

```bash
ghci> :set +m
ghci> let x = 42
|
```

Further bindings can be added to this `let` statement, so GHCi indicates
that the next line continues the previous one by changing the prompt.
Note that *layout is in effect*, so to add more bindings to this `let` we
have to line them up:

```bash
ghci> :set +m
ghci> let x = 42
|     y = 3
|
ghci>
```

* Explicit braces and semicolons can be used instead of layout:

```bash
ghci> do {
| putStrLn "hello"
| ;putStrLn "world"
| }
hello
world
ghci>
```

Note that after the closing brace, GHCi knows that the current statement
is finished, so no empty line is required.

* Multiline mode is useful when entering monadic `do` statements:

```bash
ghci> flip evalStateT 0 $ do
| i <- get
| lift $ do
|   putStrLn "Hello World!"
|   print i
|
"Hello World!"
0
ghci>
```

* During a multiline interaction, the user can interrupt and return to the
top-level prompt.

```bash
ghci> do
| putStrLn "Hello, World!"
| ^C
ghci>
```




### Type, class and other declarations

At the GHCi prompt you can also enter any top-level Haskell declaration, including `data`, `type`, `newtype`, `class`, `instance`, `deriving`, and `foreign` declarations.

```bash
ghci> data T = A | B | C deriving (Eq, Ord, Show, Enum)

ghci> [A ..]
[A,B,C]

ghci> :i T
data T = A | B | C      -- Defined at <interactive>:2:6
instance Enum T -- Defined at <interactive>:2:45
instance Eq T -- Defined at <interactive>:2:30
instance Ord T -- Defined at <interactive>:2:34
instance Show T -- Defined at <interactive>:2:39
```


As with ordinary variable bindings, later definitions shadow earlier ones, so you can re-enter a declaration to fix a problem with it or extend it. But *there's a gotcha*: when a new type declaration shadows an older one, there might be other declarations that refer to the old type. The thing to remember is that the old type still exists, and these other declarations still refer to the old type. However, while the old and the new type have the same name, GHCi will treat them as distinct.

```bash
ghci> data T = A | B
ghci> let f A = True; f B = False
ghci> data T = A | B | C
ghci> f A

<interactive>:2:3:
    Couldn't match expected type `main::Interactive.T'
                with actual type `T'
    In the first argument of `f', namely `A'
    In the expression: f A
    In an equation for `it': it = f A
ghci>
```

The old, shadowed, version of `T` is displayed as `main::Interactive.T`
by GHCi in an attempt to distinguish it from the new `T`, which is
displayed as simply `T`.


*Class and type-family* instance declarations are simply added to the list of available instances, with one exception. Since you might want to re-define one, **a class instance replaces any earlier instance with an identical head**. You aren't allowed to re-define a type family instance, since it might not be type safe to do so. Instead, re-define the whole type-family.

```bash
ghci> type family T a b
ghci> type instance T a b = a
ghci> let uc :: a -> T a b; uc = id

ghci> type instance T a b = b

<interactive>:3:15: error:
    Conflicting family instance declarations:
      T a b = a -- Defined at <interactive>:3:15
      T a b = b -- Defined at <interactive>:5:15

-- Darn! We have to re-declare T.

ghci> type family T a b
-- This is a brand-new T, unrelated to the old one
ghci> type instance T a b = b
ghci> uc 'a' :: Int

<interactive>:8:1: error:
    • Couldn't match type ‘Char’ with ‘Int’
      Expected type: Int
        Actual type: Ghci1.T Char b0
    • In the expression: uc 'a' :: Int
      In an equation for ‘it’: it = uc 'a' :: Int
```

### Things in scope at the prompt

When you type an expression at the prompt, what identifiers and types are in scope; GHCi provides a flexible way to control exactly how the context for an expression is constructed:
- the `:load`,`:add`, `:reload` commands
- the `import` declaration
- the `:module` command

* The command `:show imports` shows a summary of which modules contribute to the top-level scope.

* GHCi will tab-complete names that are in scope.


#### The effect of :load on scope

The `:load`, `:add` and `:reload` commands and affect the top-level scope. 
When you start GHCi the prompt looks like this:

```bash
ghci>
```

By default, this means that everything from the module `Prelude` is currently in scope. Should the prompt be set to `%s>` in the `.ghci` configuration file, we would be seeing `Prelude>` displayed. However, it is not the default mechanism due to the large space the prompt can take if more imports are done.

The syntax in the prompt `*module` indicates that it is the full top-level scope of `⟨module⟩` that is contributing to the scope for expressions typed at the prompt.

Without the `*`, just the exports of the module are visible.

For technical reasons, GHCi can only support the `*`-form for modules that are interpreted. Compiled modules and package modules can only contribute their exports to the current scope. To ensure that GHCi loads the interpreted version of a module, add the `*` when loading the module, e.g. `:load *M`.


In general, after a `:load`  command, an automatic import is added to the scope for the most recently loaded "target" module, in a `*`-form if possible.

For example, if you say `:load foo.hs bar.hs` and `bar.hs` contains module `Bar`, then the scope will be set to `*Bar` if `Bar` is interpreted, or if `Bar` is compiled it will be set to `Prelude` and `Bar` (GHCi automatically adds `Prelude` if it isn't present and there aren't any `*`-form modules).

These automatically-added imports can be seen with `:show imports`

```bash
ghci> :load hello.hs
[1 of 1] Compiling Main             ( hello.hs, interpreted )
Ok, modules loaded: Main.
*ghci> :show imports
:module +*Main -- added automatically
*ghci>
```

and the automatically-added import is replaced the next time you use
`:load`, `:add`, `:reload`. It can also be removed by `:module` as
with normal imports.


#### Controlling the scope with import

We are not limited to a single module: GHCi can combine scopes from multiple modules, in any mixture of `*` and non-`*` forms.

> GHCi combines the scopes from all of these modules to form the scope that is in effect at the prompt.

To add modules to the scope, use ordinary Haskell `import` syntax:

```bash
ghci> import System.IO
ghci> hPutStrLn stdout "hello\n"
hello
```


The full Haskell import syntax is supported, including `hiding` and `as` clauses. The prompt shows the modules that are currently imported, but it omits details about `hiding`, `as`, and so on. To see the full story, use `:show imports`.

```bash
ghci> import System.IO
ghci> import Data.Map as Map
ghci Map> :show imports
import Prelude -- implicit
import System.IO
import Data.Map as Map
```

Note that the `Prelude` import is marked as implicit. It can be overridden with an explicit `Prelude` import, just like in a Haskell module.


With multiple modules in scope, especially multiple `*`-form modules, it is likely that name clashes will occur. Haskell specifies that name clashes are only reported when an ambiguous identifier is used, and GHCi behaves in the same way for expressions typed at the prompt.


#### Controlling scope with :module

Another way to manipulate the scope is to use the `:module` command

```
:module + | - *mod1 ... *modn
```

- `+` form of the `module` commands adds modules to the current scope
- `-` form removes them
- Without either `+` or `-`, the current scope is replaced by the set of modules specified

Note that if you use this form and leave out `Prelude`, an implicit `Prelude` import will be added automatically.


The `:module` command provides a way to do two things that cannot be done with ordinary `import` declarations:
- `:module` supports the `*` modifier on modules, which opens the full top-level scope of a module, rather than just its exports.
- Imports can be *removed* from the context, using the syntax `:module -M`. The `import` syntax is cumulative (as in a Haskell module), so this is the only way to subtract from the scope.


#### Qualified names

To make life slightly easier, the GHCi prompt also behaves as if there is an implicit `import qualified` declaration for every module in every package, and every module currently loaded into GHCi. This behaviour can be disabled with the `-fno-implicit-import-qualified` flag.


#### :module and :load

It might seem that `:module` /`import` and `:load` /`:add`  /`:reload` do similar things: you can use both to bring a module into scope. However, there is a very important difference. GHCi is concerned with two sets of modules:

* The set of modules that are currently *loaded*. This set is modified by `:load`, `:add` and `:reload`, and can be shown with `:show modules`.

* The set of modules that are currently *in scope* at the prompt. This set is modified by `import` and `:module`, and it is also modified automatically after `:load`, `:add`, `:reload` as described above. The set of modules in scope can be shown with `:show imports`.


You can add a module to the scope (via `:module` or `import`) only if either
- it is loaded
- it is a module from a package that GHCi knows about

Using `:module` or `import` to try bring into scope a non-loaded module may result in the message `module M is not loaded`.



## CONTINUE FROM HERE

### :main and :run

When a program is compiled and executed, it can use the `getArgs`
function to access the command-line arguments. However, we cannot simply
pass the arguments to the `main` function while we are testing in ghci,
as the `main` function doesn't take its directly.

Instead, we can use the `:main` 
command. This runs whatever `main` is in scope, with any arguments being
treated the same as command-line arguments, e.g.:

```bash
ghci> main = System.Environment.getArgs >>= print
ghci> :main foo bar
["foo","bar"]
```

We can also quote arguments which contains characters like spaces, and
they are treated like Haskell strings, or we can just use Haskell list
syntax:

```bash
ghci> :main foo "bar baz"
["foo","bar baz"]
ghci> :main ["foo", "bar baz"]
["foo","bar baz"]
```

Finally, other functions can be called, either with the `-main-is` flag
or the `:run`  command:

```bash
ghci> foo = putStrLn "foo" >> System.Environment.getArgs >>= print
ghci> bar = putStrLn "bar" >> System.Environment.getArgs >>= print
ghci> :set -main-is foo
ghci> :main foo "bar baz"
foo
["foo","bar baz"]
ghci> :run bar ["foo", "bar baz"]
bar
["foo","bar baz"]
```

### The `it` variable

Whenever an expression (or a non-binding statement, to be precise) is typed at the prompt, GHCi implicitly binds its value to the variable `it`. For example:

```bash
ghci> 1+2
3
ghci> it * 2
6
```

What actually happens is that GHCi typechecks the expression, and if it doesn't have an `IO` type, then it transforms it as follows: an expression `e` turns into:

```bash
let it = e;
print it
```

which is then run as an IO-action.

Hence, the original expression must have a type which is an instance of
the `Show` class, or GHCi will complain:

```bash
ghci> id

<interactive>:1:0:
    No instance for (Show (a -> a))
      arising from use of `print' at <interactive>:1:0-1
    Possible fix: add an instance declaration for (Show (a -> a))
    In the expression: print it
    In a 'do' expression: print it
```

The error message contains some clues as to the transformation happening
internally.

If the expression was instead of type `IO a` for some `a`, then `it`
will be bound to the result of the `IO` computation, which is of type
`a`. eg.:

```bash
ghci> Data.Time.getZonedTime
2017-04-10 12:34:56.93213581 UTC
ghci> print it
2017-04-10 12:34:56.93213581 UTC
```

* The corresponding translation for an IO-typed `e` is

```bash
it <- e
```

Note that `it` is shadowed by the new value each time you evaluate a new expression, and the old value of `it` is lost.

In order to stop the value `it` being bound on each command, the flag `-fno-it` can be set. The `it` variable can be the source of space leaks due to how shadowed declarations are handled by GHCi.

::: {.ghc-flag shortdesc="No longer set the special variable `it`." type="dynamic" reverse="-fno-no-it" category=""}
-fno-it

When this flag is set, the variable `it` will no longer be set to the result of the previously evaluated expression.



### Type defaulting in GHCi

- langext: *ExtendedDefaultRules*
- Type defaulting; in GHCi single: Show class
- Use GHCi's extended default rules in a normal module.
- since: 6.8.1
- Allow defaulting to take place for more than just numeric classes.


Consider this GHCi session:

```bash
ghci> reverse []
```

What should GHCi do? Strictly speaking, the program is ambiguous.
`show (reverse [])` (which is what GHCi computes here) has type
`Show a => String` and how that displays depends on the type `a`. For
example:

```bash
ghci> reverse ([] :: String)
""
ghci> reverse ([] :: [Int])
[]
```

However, it is tiresome for the user to have to specify the type, so
GHCi extends Haskell's type-defaulting rules (Section 4.3.4 of the
Haskell 2010 Report) as follows. The standard rules take each group of
constraints `(C1 a, C2 a, ..., Cn a)` for each type variable `a`, and
defaults the type variable if

1.  The type variable `a` appears in no other constraints
2.  All the classes `Ci` are standard.
3.  At least one of the classes `Ci` is numeric.

At the GHCi prompt, or with GHC if the
`ExtendedDefaultRules`extension"} flag is
given, the types are instead resolved with the following method:

Find all the unsolved constraints. Then:

-   Find those that are of form `(C a)` where `a` is a type variable,
    and partition those constraints into groups that share a common type
    variable `a`.
-   Keep only the groups in which at least one of the classes is an
    **interactive class** (defined below).
-   Now, for each remaining group G, try each type `ty` from the
    default-type list in turn; if setting `a = ty` would allow the
    constraints in G to be completely solved. If so, default `a` to
    `ty`.
-   The unit type `()` and the list type `[]` are added to the start of
    the standard list of types which are tried when doing type
    defaulting.

Note that any multi-parameter constraints `(D a b)` or `(D [a] Int)` do
not participate in the process (either to help or to hinder); but they
must of course be soluble once the defaulting process is complete.

The last point means that, for example, this program: :

    main :: IO ()
    main = print def

    instance Num ()

    def :: (Num a, Enum a) => a
    def = toEnum 0

prints `()` rather than `0` as the type is defaulted to `()` rather than
`Integer`.

The motivation for the change is that it means `IO a` actions default to
`IO ()`, which in turn means that ghci won't try to print a result when
running them. This is particularly important for `printf`, which has an
instance that returns `IO a`. However, it is only able to return
`undefined` (the reason for the instance having this type is so that
printf doesn't require extensions to the class system), so if the type
defaults to `Integer` then ghci gives an error when running a printf.

See also `actions-at-prompt`ref"} for how the
monad of a computational expression defaults to `IO` if possible.



#### Interactive classes

The interactive classes (only relevant when `ExtendedDefaultRules`extension"} is in effect) are: any numeric class, `Show`, `Eq`, `Ord`, `Foldable` or `Traversable`.

As long as a type variable is constrained by one of these classes, defaulting will occur, as outlined above.

#### Extended rules around `default` declarations

Since the rules for defaulting are relaxed under
`ExtendedDefaultRules`extension"}, the rules
for `default` declarations are also relaxed. According to Section 4.3.4
of the Haskell 2010 Report, a `default` declaration looks like
`default (t1, ..., tn)` where, for each `ti`, `Num ti` must hold. This
is relaxed to say that for each `ti`, there must exist an interactive
class `C` such that `C ti` holds. This means that type *constructors*
can be allowed in these lists. For example, the following works if you
wish your `Foldable` constraints to default to `Maybe` but your `Num`
constraints to still default to `Integer` or `Double`: :

    default (Maybe, Integer, Double)

### Using a custom interactive printing function {#ghci-interactive-print}

::: {.index}
single: Custom printing function; in GHCi
:::

Since GHC 7.6.1, GHCi prints the result of expressions typed at the
prompt using the function `System.IO.print`. Its type signature is
`Show a => a -> IO ()`, and it works by converting the value to `String`
using `show`.

This is not ideal in certain cases, like when the output is long, or
contains strings with non-ascii characters.

The `-interactive-print ⟨name⟩`  flag
allows to specify any function of type `C a => a -> IO ()`, for some
constraint `C`, as the function for printing evaluated expressions. The
function can reside in any loaded module or any registered package, but
only when it resides in a registered package will it survive a
`:cd` , `:add` 
 , `:load` ,
`:reload`  or,
`:set` .

::: {.ghc-flag shortdesc=":ref:`Select the function to use for printing evaluated
expressions in GHCi <ghci-interactive-print>`" type="dynamic" category=""}
-interactive-print ⟨name⟩

Set the function used by GHCi to print evaluation results. Given name
must be of type `C a => a -> IO ()`.
:::

As an example, suppose we have following special printing module: :

    module SpecPrinter where
    import System.IO

    sprint a = putStrLn $ show a ++ "!"

The `sprint` function adds an exclamation mark at the end of any printed
value. Running GHCi with the command:

```bash
ghci -interactive-print=SpecPrinter.sprint SpecPrinter
```

will start an interactive session where values with be printed using
`sprint`:

```bash
*SpecPrinter> [1,2,3]
[1,2,3]!
*SpecPrinter> 42
42!
```

A custom pretty printing function can be used, for example, to format
tree-like and nested structures in a more readable way.

The `-interactive-print ⟨name⟩`  flag
can also be used when running GHC in `-e mode`:

```bash
% ghc -e "[1,2,3]" -interactive-print=SpecPrinter.sprint SpecPrinter
[1,2,3]!
```

### Stack Traces in GHCi {#ghci-stack-traces}

::: {.index}
simple: stack trace; in GHCi
:::

\[ This is an experimental feature enabled by the new
`-fexternal-interpreter` flag that was introduced in GHC 8.0.1. It is
currently not supported on Windows.\]

GHCi can use the profiling system to collect stack trace information
when running interpreted code. To gain access to stack traces, start
GHCi like this:

```bash
ghci -fexternal-interpreter -prof
```

This runs the interpreted code in a separate process (see
`external-interpreter`ref"}) and runs it in
profiling mode to collect call stack information. Note that because
we're running the interpreted code in profiling mode, all packages that
you use must be compiled for profiling. The `-prof` flag to GHCi only
works in conjunction with `-fexternal-interpreter`.

There are three ways to get access to the current call stack.

-   `error` and `undefined` automatically attach the current stack to
    the error message. This often complements the `HasCallStack` stack
    (see `hascallstack`ref"}), so both call
    stacks are shown.
-   `Debug.Trace.traceStack` is a version of `Debug.Trace.trace` that
    also prints the current call stack.
-   Functions in the module `GHC.Stack` can be used to get the current
    stack and render it.

You don't need to use `-fprof-auto` for interpreted modules,
annotations are automatically added at a granularity fine enough to
distinguish individual call sites. However, you won't see any call
stack information for compiled code unless it was compiled with
`-fprof-auto` or has explicit `SCC` annotations (see
`scc-pragma`ref"}).

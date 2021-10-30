# 3.5 GHCi Debugger

305-ghci-debugger.md


The GHCi Debugger

GHCi contains a simple imperative-style debugger in which you can stop a
running computation in order to examine the values of variables. The
debugger is integrated into GHCi, and is turned on by default: no flags
are required to enable the debugging facilities. There is one major
restriction: breakpoints and single-stepping are only available in
interpreted modules; compiled code is invisible to the debugger[^4].

The debugger provides the following:

-   The ability to set a breakpoint on a function definition or
    expression in the program. When the function is called, or the
    expression evaluated, GHCi suspends execution and returns to the
    prompt, where you can inspect the values of local variables before
    continuing with the execution.
-   Execution can be single-stepped: the evaluator will suspend
    execution approximately after every reduction, allowing local
    variables to be inspected. This is equivalent to setting a
    breakpoint at every point in the program.
-   Execution can take place in tracing mode, in which the evaluator
    remembers each evaluation step as it happens, but doesn't suspend
    execution until an actual breakpoint is reached. When this happens,
    the history of evaluation steps can be inspected.
-   Exceptions (e.g. pattern matching failure and `error`) can be
    treated as breakpoints, to help locate the source of an exception in
    the program.

There is currently no support for obtaining a "stack trace", but the
tracing and history features provide a useful second-best, which will
often be enough to establish the context of an error. For instance, it
is possible to break automatically when an exception is thrown, even if
it is thrown from within compiled code (see
`ghci-debugger-exceptions`ref"}).

### Breakpoints and inspecting variables {#breakpoints}

Let's use quicksort as a running example. Here's the code: :

    qsort [] = []
    qsort (a:as) = qsort left ++ [a] ++ qsort right
      where (left,right) = (filter (<=a) as, filter (>a) as)

    main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])

First, load the module into GHCi:

```bash
ghci> :l qsort.hs
[1 of 1] Compiling Main             ( qsort.hs, interpreted )
Ok, modules loaded: Main.
*ghci>
```

Now, let's set a breakpoint on the right-hand-side of the second
equation of qsort:

```bash
*ghci> :break 2
Breakpoint 0 activated at qsort.hs:2:15-46
*ghci>
```

The command `:break 2` sets a breakpoint on line 2 of the most
recently-loaded module, in this case `qsort.hs`. Specifically, it picks
the leftmost complete subexpression on that line on which to set the
breakpoint, which in this case is the expression
`(qsort left ++ [a] ++ qsort right)`.

Now, we run the program:

```bash
*ghci> main
Stopped at qsort.hs:2:15-46
_result :: [a]
a :: a
left :: [a]
right :: [a]
[qsort.hs:2:15-46] *ghci>
```

Execution has stopped at the breakpoint. The prompt has changed to
indicate that we are currently stopped at a breakpoint, and the
location: `[qsort.hs:2:15-46]`. To further clarify the location, we can
use the `:list`  command:

```bash
[qsort.hs:2:15-46] *ghci> :list
1  qsort [] = []
2  qsort (a:as) = qsort left ++ [a] ++ qsort right
3    where (left,right) = (filter (<=a) as, filter (>a) as)
```

The `:list`  command lists the source
code around the current breakpoint. If your output device supports it,
then GHCi will highlight the active subexpression in bold.

GHCi has provided bindings for the free variables[^5] of the expression
on which the breakpoint was placed (`a`, `left`, `right`), and
additionally a binding for the result of the expression (`_result`).
These variables are just like other variables that you might define in
GHCi; you can use them in expressions that you type at the prompt, you
can ask for their types with `:type` ,
and so on. There is one important difference though: these variables may
only have partial types. For example, if we try to display the value of
`left`:

```bash
[qsort.hs:2:15-46] *ghci> left

<interactive>:1:0:
    Ambiguous type variable `a' in the constraint:
      `Show a' arising from a use of `print' at <interactive>:1:0-3
    Cannot resolve unknown runtime types: a
    Use :print or :force to determine these types
```

This is because `qsort` is a polymorphic function, and because GHCi does
not carry type information at runtime, it cannot determine the runtime
types of free variables that involve type variables. Hence, when you ask
to display `left` at the prompt, GHCi can't figure out which instance
of `Show` to use, so it emits the type error above.

Fortunately, the debugger includes a generic printing command,
`:print` , which can inspect the
actual runtime value of a variable and attempt to reconstruct its type.
If we try it on `left`:

```bash
[qsort.hs:2:15-46] *ghci> :set -fprint-evld-with-show
[qsort.hs:2:15-46] *ghci> :print left
left = (_t1::[a])
```

This isn't particularly enlightening. What happened is that `left` is
bound to an unevaluated computation (a suspension, or thunk), and
`:print`  does not force any
evaluation. The idea is that `:print` 
can be used to inspect values at a breakpoint without any unfortunate
side effects. It won't force any evaluation, which could cause the
program to give a different answer than it would normally, and hence it
won't cause any exceptions to be raised, infinite loops, or further
breakpoints to be triggered (see `nested-breakpoints` 
role="ref"}). Rather than forcing thunks, `:print` 
  binds each thunk to a fresh variable beginning with an
underscore, in this case `_t1`.

::: {.ghc-flag shortdesc="Instruct :ghci-cmd:`:print` to use `Show` instances where possible." category="interactive" type="dynamic"}
-fprint-evld-with-show

The flag `-fprint-evld-with-show` 
instructs `:print`  to reuse available
`Show` instances when possible. This happens only when the contents of
the variable being inspected are completely evaluated.
:::

If we aren't concerned about preserving the evaluatedness of a
variable, we can use `:force`  instead
of `:print` . The
`:force`  command behaves exactly like
`:print` , except that it forces the
evaluation of any thunks it encounters:

```bash
[qsort.hs:2:15-46] *ghci> :force left
left = [4,0,3,1]
```

Now, since `:force`  has inspected the
runtime value of `left`, it has reconstructed its type. We can see the
results of this type reconstruction:

```bash
[qsort.hs:2:15-46] *ghci> :show bindings
_result :: [Integer]
a :: Integer
left :: [Integer]
right :: [Integer]
_t1 :: [Integer]
```

Not only do we now know the type of `left`, but all the other partial
types have also been resolved. So we can ask for the value of `a`, for
example:

```bash
[qsort.hs:2:15-46] *ghci> a
8
```

You might find it useful to use Haskell's `seq` function to evaluate
individual thunks rather than evaluating the whole expression with
`:force` . For example:

```bash
[qsort.hs:2:15-46] *ghci> :print right
right = (_t1::[Integer])
[qsort.hs:2:15-46] *ghci> seq _t1 ()
()
[qsort.hs:2:15-46] *ghci> :print right
right = 23 : (_t2::[Integer])
```

We evaluated only the `_t1` thunk, revealing the head of the list, and
the tail is another thunk now bound to `_t2`. The `seq` function is a
little inconvenient to use here, so you might want to use
`:def`  to make a nicer interface
(left as an exercise for the reader!).

Finally, we can continue the current execution:

```bash
[qsort.hs:2:15-46] *ghci> :continue
Stopped at qsort.hs:2:15-46
_result :: [a]
a :: a
left :: [a]
right :: [a]
[qsort.hs:2:15-46] *ghci>
```

The execution continued at the point it previously stopped, and has now
stopped at the breakpoint for a second time.

#### Setting breakpoints

Breakpoints can be set in various ways. Perhaps the easiest way to set a
breakpoint is to name a top-level function:

```bash
:break identifier
```

Where ⟨identifier⟩ names any top-level function in an interpreted module
currently loaded into GHCi (qualified names may be used). The breakpoint
will be set on the body of the function, when it is fully applied. If
the function has several patterns, then a breakpoint will be set on each
of them.

By using qualified names, one can set breakpoints on all functions
(top-level and nested) in every loaded and interpreted module:

```bash
:break [ModQual.]topLevelIdent[.nestedIdent]...[.nestedIdent]
```

⟨ModQual⟩ is optional and is either the effective name of a module or
the local alias of a qualified import statement.

⟨topLevelIdent⟩ is the name of a top level function in the module
referenced by ⟨ModQual⟩.

⟨nestedIdent⟩ is optional and the name of a function nested in a let or
where clause inside the previously mentioned function ⟨nestedIdent⟩ or
⟨topLevelIdent⟩.

If ⟨ModQual⟩ is a module name, then ⟨topLevelIdent⟩ can be any top level
identifier in this module. If ⟨ModQual⟩ is missing or a local alias of a
qualified import, then ⟨topLevelIdent⟩ must be in scope.

Breakpoints can be set on arbitrarily deeply nested functions, but the
whole chain of nested function names must be specified.

Consider the function `foo` in a module `Main`:

```bash
foo s = 'a' : add s
    where add = (++"z")
```

The breakpoint on the function `add` can be set with one of the
following commands:

```bash
:break Main.foo.add
:break foo.add
```

Breakpoints can also be set by line (and optionally column) number:

```bash
:break line
:break line column
:break module line
:break module line column
```

When a breakpoint is set on a particular line, GHCi sets the breakpoint
on the leftmost subexpression that begins and ends on that line. If two
complete subexpressions start at the same column, the longest one is
picked. If there is no complete subexpression on the line, then the
leftmost expression starting on the line is picked, and failing that the
rightmost expression that partially or completely covers the line.

When a breakpoint is set on a particular line and column, GHCi picks the
smallest subexpression that encloses that location on which to set the
breakpoint. Note: GHC considers the TAB character to have a width of 1,
wherever it occurs; in other words it counts characters, rather than
columns. This matches what some editors do, and doesn't match others.
The best advice is to avoid tab characters in your source code
altogether (see `-Wtabs`  in
`options-sanity`ref"}).

If the module is omitted, then the most recently-loaded module is used.

Not all subexpressions are potential breakpoint locations. Single
variables are typically not considered to be breakpoint locations
(unless the variable is the right-hand-side of a function definition,
lambda, or case alternative). The rule of thumb is that all redexes are
breakpoint locations, together with the bodies of functions, lambdas,
case alternatives and binding statements. There is normally no
breakpoint on a let expression, but there will always be a breakpoint on
its body, because we are usually interested in inspecting the values of
the variables bound by the let.

#### Managing breakpoints

The list of breakpoints currently defined can be displayed using
`:show breaks` :

```bash
*ghci> :show breaks
[0] Main qsort.hs:1:11-12 enabled
[1] Main qsort.hs:2:15-46 enabled
```

To disable one or several defined breakpoint, use the
`:disable`  command with one or
several blank separated numbers given in the output from
`:show breaks` :. To disable all
breakpoints at once, use `:disable *`.

```bash
*ghci> :disable 0
*ghci> :show breaks
[0] Main qsort.hs:1:11-12 disabled
[1] Main qsort.hs:2:15-46 enabled
```

Disabled breakpoints can be (re-)enabled with the
`:enable`  command. The parameters of
the `:disable`  and
`:enable`  commands are identical.

To delete a breakpoint, use the `:delete` 
  command with the number given in the output from
`:show breaks` :

```bash
*ghci> :delete 0
*ghci> :show breaks
[1] Main qsort.hs:2:15-46 disabled
```

To delete all breakpoints at once, use `:delete *`.

### Single-stepping

Single-stepping is a great way to visualise the execution of your
program, and it is also a useful tool for identifying the source of a
bug. GHCi offers two variants of stepping. Use `:step` 
  to enable all the breakpoints in the program, and
execute until the next breakpoint is reached. Use
`:steplocal`  to limit the set of
enabled breakpoints to those in the current top level function.
Similarly, use `:stepmodule`  to
single step only on breakpoints contained in the current module. For
example:

```bash
*ghci> :step main
Stopped at qsort.hs:5:7-47
_result :: IO ()
```

The command `:step expr <:step>` 
begins the evaluation of ⟨expr⟩ in single-stepping mode. If ⟨expr⟩ is
omitted, then it single-steps from the current breakpoint.
`:steplocal`  and
`:stepmodule`  commands work
similarly.

The `:list`  command is particularly
useful when single-stepping, to see where you currently are:

```bash
[qsort.hs:5:7-47] *ghci> :list
4
5  main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
6
[qsort.hs:5:7-47] *ghci>
```

In fact, GHCi provides a way to run a command when a breakpoint is hit,
so we can make it automatically do `:list` 
 :

```bash
[qsort.hs:5:7-47] *ghci> :set stop :list
[qsort.hs:5:7-47] *ghci> :step
Stopped at qsort.hs:5:14-46
_result :: [Integer]
4
5  main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])
6
[qsort.hs:5:14-46] *ghci>
```

### Nested breakpoints

When GHCi is stopped at a breakpoint, and an expression entered at the
prompt triggers a second breakpoint, the new breakpoint becomes the
"current" one, and the old one is saved on a stack. An arbitrary
number of breakpoint contexts can be built up in this way. For example:

```bash
[qsort.hs:2:15-46] *ghci> :st qsort [1,3]
Stopped at qsort.hs:(1,0)-(3,55)
_result :: [a]
... [qsort.hs:(1,0)-(3,55)] *ghci>
```

While stopped at the breakpoint on line 2 that we set earlier, we
started a new evaluation with `:step qsort [1,3]`. This new evaluation
stopped after one step (at the definition of `qsort`). The prompt has
changed, now prefixed with `...`, to indicate that there are saved
breakpoints beyond the current one. To see the stack of contexts, use
`:show context` :

```bash
... [qsort.hs:(1,0)-(3,55)] *ghci> :show context
--> main
  Stopped at qsort.hs:2:15-46
--> qsort [1,3]
  Stopped at qsort.hs:(1,0)-(3,55)
... [qsort.hs:(1,0)-(3,55)] *ghci>
```

To abandon the current evaluation, use `:abandon` 
 :

```bash
... [qsort.hs:(1,0)-(3,55)] *ghci> :abandon
[qsort.hs:2:15-46] *ghci> :abandon
*ghci>
```

### The `_result` variable {#ghci-debugger-result}

When stopped at a breakpoint or single-step, GHCi binds the variable
`_result` to the value of the currently active expression. The value of
`_result` is presumably not available yet, because we stopped its
evaluation, but it can be forced: if the type is known and showable,
then just entering `_result` at the prompt will show it. However,
there's one caveat to doing this: evaluating `_result` will be likely
to trigger further breakpoints, starting with the breakpoint we are
currently stopped at (if we stopped at a real breakpoint, rather than
due to `:step` ). So it will probably
be necessary to issue a `:continue` 
immediately when evaluating `_result`. Alternatively, you can use
`:force`  which ignores breakpoints.

### Tracing and history {#tracing}

A question that we often want to ask when debugging a program is "how
did I get here?". Traditional imperative debuggers usually provide some
kind of stack-tracing feature that lets you see the stack of active
function calls (sometimes called the "lexical call stack"), describing a
path through the code to the current location. Unfortunately this is
hard to provide in Haskell, because execution proceeds on a
demand-driven basis, rather than a depth-first basis as in strict
languages. The "stack" in GHC's execution engine bears little
resemblance to the lexical call stack. Ideally GHCi would maintain a
separate lexical call stack in addition to the dynamic call stack, and
in fact this is exactly what our profiling system does
(`profiling`ref"}), and what some other Haskell
debuggers do. For the time being, however, GHCi doesn't maintain a
lexical call stack (there are some technical challenges to be overcome).
Instead, we provide a way to backtrack from a breakpoint to previous
evaluation steps: essentially this is like single-stepping backwards,
and should in many cases provide enough information to answer the "how
did I get here?" question.

To use tracing, evaluate an expression with the
`:trace`  command. For example, if we
set a breakpoint on the base case of `qsort`:

```bash
*ghci> :list qsort
1  qsort [] = []
2  qsort (a:as) = qsort left ++ [a] ++ qsort right
3    where (left,right) = (filter (<=a) as, filter (>a) as)
4
*ghci> :b 1
Breakpoint 1 activated at qsort.hs:1:11-12
*ghci>
```

and then run a small `qsort` with tracing:

```bash
*ghci> :trace qsort [3,2,1]
Stopped at qsort.hs:1:11-12
_result :: [a]
[qsort.hs:1:11-12] *ghci>
```

We can now inspect the history of evaluation steps:

```bash
[qsort.hs:1:11-12] *ghci> :hist
-1  : qsort.hs:3:24-38
-2  : qsort.hs:3:23-55
-3  : qsort.hs:(1,0)-(3,55)
-4  : qsort.hs:2:15-24
-5  : qsort.hs:2:15-46
-6  : qsort.hs:3:24-38
-7  : qsort.hs:3:23-55
-8  : qsort.hs:(1,0)-(3,55)
-9  : qsort.hs:2:15-24
-10 : qsort.hs:2:15-46
-11 : qsort.hs:3:24-38
-12 : qsort.hs:3:23-55
-13 : qsort.hs:(1,0)-(3,55)
-14 : qsort.hs:2:15-24
-15 : qsort.hs:2:15-46
-16 : qsort.hs:(1,0)-(3,55)
<end of history>
```

To examine one of the steps in the history, use
`:back` :

```bash
[qsort.hs:1:11-12] *ghci> :back
Logged breakpoint at qsort.hs:3:24-38
_result :: [a]
as :: [a]
a :: a
[-1: qsort.hs:3:24-38] *ghci>
```

Note that the local variables at each step in the history have been
preserved, and can be examined as usual. Also note that the prompt has
changed to indicate that we're currently examining the first step in
the history: `-1`. The command `:forward` 
  can be used to traverse forward in the history.

The `:trace`  command can be used with
or without an expression. When used without an expression, tracing
begins from the current breakpoint, just like `:step` 
 .

The history is only available when using `:trace` 
 ; the reason for this is we found that logging each
breakpoint in the history cuts performance by a factor of 2 or more.

::: {.ghc-flag shortdesc="Set the number of entries GHCi keeps for `:history`.
See :ref:`ghci-debugger`." type="dynamic" category=""}
-fghci-hist-size=⟨n⟩

default

:   50

Modify the depth of the evaluation history tracked by GHCi.
:::

### Debugging exceptions {#ghci-debugger-exceptions}

Another common question that comes up when debugging is "where did this
exception come from?". Exceptions such as those raised by `error` or
`head []` have no context information attached to them. Finding which
particular call to `head` in your program resulted in the error can be a
painstaking process, usually involving `Debug.Trace.trace`, or compiling
with profiling and using `Debug.Trace.traceStack` or `+RTS -xc` (see
`-xc`rts-flag"}).

The GHCi debugger offers a way to hopefully shed some light on these
errors quickly and without modifying or recompiling the source code. One
way would be to set a breakpoint on the location in the source code that
throws the exception, and then use `:trace` 
  and `:history`  to
establish the context. However, `head` is in a library and we can't set
a breakpoint on it directly. For this reason, GHCi provides the flags
`-fbreak-on-exception`  which causes
the evaluator to stop when an exception is thrown, and
`-fbreak-on-error` , which works
similarly but stops only on uncaught exceptions. When stopping at an
exception, GHCi will act just as it does when a breakpoint is hit, with
the deviation that it will not show you any source code location. Due to
this, these commands are only really useful in conjunction with
`:trace` , in order to log the steps
leading up to the exception. For example:

```bash
*ghci> :set -fbreak-on-exception
*ghci> :trace qsort ("abc" ++ undefined)
“Stopped at <exception thrown>
_exception :: e
[<exception thrown>] *ghci> :hist
-1  : qsort.hs:3:24-38
-2  : qsort.hs:3:23-55
-3  : qsort.hs:(1,0)-(3,55)
-4  : qsort.hs:2:15-24
-5  : qsort.hs:2:15-46
-6  : qsort.hs:(1,0)-(3,55)
<end of history>
[<exception thrown>] *ghci> :back
Logged breakpoint at qsort.hs:3:24-38
_result :: [a]
as :: [a]
a :: a
[-1: qsort.hs:3:24-38] *ghci> :force as
*** Exception: Prelude.undefined
[-1: qsort.hs:3:24-38] *ghci> :print as
as = 'b' : 'c' : (_t1::[Char])
```

The exception itself is bound to a new variable, `_exception`.

Breaking on exceptions is particularly useful for finding out what your
program was doing when it was in an infinite loop. Just hit Control-C,
and examine the history to find out what was going on.

::: {.ghc-flag shortdesc=":ref:`Break on any exception thrown <ghci-debugger-exceptions>`" type="dynamic" reverse="-fno-break-on-exception" category=""}
-fbreak-on-exception

Causes GHCi to halt evaluation and return to the interactive prompt in
the event of an exception. `-fbreak-on-exception` 
role="  breaks on all exceptions.
:::

::: {.ghc-flag shortdesc=":ref:`Break on uncaught exceptions and errors
<ghci-debugger-exceptions>`" type="dynamic" reverse="-fno-break-on-error" category=""}
-fbreak-on-error

Causes GHCi to halt evaluation and return to the interactive prompt in
the event of an exception. `-fbreak-on-error` 
role="  breaks on only those exceptions which would otherwise
be uncaught.
:::

### Example: inspecting functions

It is possible to use the debugger to examine function values. When we
are at a breakpoint and a function is in scope, the debugger cannot show
you the source code for it; however, it is possible to get some
information by applying it to some arguments and observing the result.

The process is slightly complicated when the binding is polymorphic. We
show the process by means of an example. To keep things simple, we will
use the well known `map` function:

    import Prelude hiding (map)

    map :: (a->b) -> [a] -> [b]
    map f [] = []
    map f (x:xs) = f x : map f xs

We set a breakpoint on `map`, and call it.

```bash
*ghci> :break 5
Breakpoint 0 activated at  map.hs:5:15-28
*ghci> map Just [1..5]
Stopped at map.hs:(4,0)-(5,12)
_result :: [b]
x :: a
f :: a -> b
xs :: [a]
```

GHCi tells us that, among other bindings, `f` is in scope. However, its
type is not fully known yet, and thus it is not possible to apply it to
any arguments. Nevertheless, observe that the type of its first argument
is the same as the type of `x`, and its result type is shared with
`_result`.

As we demonstrated earlier (`breakpoints` 
role="ref"}), the debugger has some intelligence built-in to update the
type of `f` whenever the types of `x` or `_result` are discovered. So
what we do in this scenario is force `x` a bit, in order to recover both
its type and the argument part of `f`.

```bash
*ghci> seq x ()
*ghci> :print x
x = 1
```

We can check now that as expected, the type of `x` has been
reconstructed, and with it the type of `f` has been too:

```bash
*ghci> :t x
x :: Integer
*ghci> :t f
f :: Integer -> b
```

From here, we can apply f to any argument of type Integer and observe
the results.

```bash
*ghci> let b = f 10
*ghci> :t b
b :: b
*ghci> b
<interactive>:1:0:
    Ambiguous type variable `b' in the constraint:
      `Show b' arising from a use of `print' at <interactive>:1:0
*ghci> :p b
b = (_t2::a)
*ghci> seq b ()
()
*ghci> :t b
b :: a
*ghci> :p b
b = Just 10
*ghci> :t b
b :: Maybe Integer
*ghci> :t f
f :: Integer -> Maybe Integer
*ghci> f 20
Just 20
*ghci> map f [1..5]
[Just 1, Just 2, Just 3, Just 4, Just 5]
```

In the first application of `f`, we had to do some more type
reconstruction in order to recover the result type of `f`. But after
that, we are free to use `f` normally.

### Limitations

-   When stopped at a breakpoint, if you try to evaluate a variable that
    is already under evaluation, the second evaluation will hang. The
    reason is that GHC knows the variable is under evaluation, so the
    new evaluation just waits for the result before continuing, but of
    course this isn't going to happen because the first evaluation is
    stopped at a breakpoint. Control-C can interrupt the hung evaluation
    and return to the prompt.

    The most common way this can happen is when you're evaluating a CAF
    (e.g. main), stop at a breakpoint, and ask for the value of the CAF
    at the prompt again.

-   Implicit parameters (see `implicit-parameters` 
    role="ref"}) are only available at the scope of a breakpoint if
    there is an explicit type signature.

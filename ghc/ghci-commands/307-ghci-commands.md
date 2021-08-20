# 3.7 GHCi Commands

307-ghci-commands.md

GHCi commands all begin with `:` and consist of a single command name followed by zero or more parameters. The command name may be abbreviated, with ambiguities being resolved in favour of the more commonly used commands.

## abandon

`:abandon`    
the current evaluation; only available when stopped at a breakpoint.

## add

`:add [*] ⟨module⟩`    
Add one or more ⟨module⟩ to the current target set, and perform a reload. Normally pre-compiled code for the module will be loaded if available, otherwise the module will be compiled to byte-code. Using the `*` prefix forces the module to be loaded as byte-code. `⟨module⟩` may be a file path. A `~` symbol at the beginning of ⟨module⟩ will be replaced by the contents of `HOME`.

## all-types

`:all-types`    
List all types collected for expressions and (local) bindings currently loaded (while `:set +c` was active) with their respective source-code span, e.g.

```hs
GhciTypes> :all-types
GhciTypes.hs:(38,13)-(38,24): Maybe Id
GhciTypes.hs:(45,10)-(45,29): Outputable SpanInfo
GhciTypes.hs:(45,10)-(45,29): (Rational -> SpanInfo -> SDoc) -> Outputable SpanInfo
```

## back

`:back ⟨n⟩`   
Travel back ⟨n⟩ steps in the history. ⟨n⟩ is 1 if omitted. See tracing for more about GHCi's debugging facilities. See also: `:trace` ghci-cmd, `:history`, `:forward`.

## break

`:break [⟨identifier⟩ | [⟨module⟩] ⟨line⟩ [⟨column⟩]]`    
Set a breakpoint on the specified function or line and column. See `setting-breakpoints`

## break

`:browse [!] [[*] ⟨module⟩]`   
Displays the identifiers exported by the module `⟨module⟩`, which must be either loaded into GHCi or be a member of a package. If `⟨module⟩` is omitted, the most recently-loaded module is used. Like all other GHCi commands, the output is always displayed in the current GHCi scope.

There are two variants of the browse command:
- if the `*` symbol is placed before the module name, then *all* the identifiers in scope in `⟨module⟩` (rather that just its exports) are shown.

The `*` form is only available for modules which are interpreted; for compiled modules (including modules from packages) only the non-`*` form of `:browse`  is available.

Data constructors and class methods are usually displayed in the context of their data type or class declaration. However, if the `!` symbol is appended to the command, thus `:browse!`, they are listed individually. The `!`-form also annotates the listing with comments giving possible imports for each group of entries. Here is an example:

```hs
ghci> :browse! Data.Maybe

-- not currently imported
Data.Maybe.catMaybes :: [Maybe a] -> [a]
Data.Maybe.fromJust :: Maybe a -> a
Data.Maybe.fromMaybe :: a -> Maybe a -> a
Data.Maybe.isJust :: Maybe a -> Bool
Data.Maybe.isNothing :: Maybe a -> Bool
Data.Maybe.listToMaybe :: [a] -> Maybe a
Data.Maybe.mapMaybe :: (a -> Maybe b) -> [a] -> [b]
Data.Maybe.maybeToList :: Maybe a -> [a]

-- imported via Prelude
Just :: a -> Maybe a
data Maybe a = Nothing | Just a
Nothing :: Maybe a
maybe :: b -> (a -> b) -> Maybe a -> b
```

This output shows that, in the context of the current session (ie in the scope of `Prelude`), the first group of items from `Data.Maybe` are not in scope (although they are available in fully qualified form in the GHCi session), whereas the second group of items are in scope (via `Prelude`) and are therefore available either unqualified, or with a `Prelude.` qualifier.


## cd

`:cd ⟨dir⟩`    
Changes the current working directory to ⟨dir⟩. A "~" symbol at the beginning of ⟨dir⟩ will be replaced by the contents of the environment `HOME`. See also the `:show paths` command for showing the current working directory.

Note: changing directories causes all currently loaded modules to be unloaded. This is because the search path is usually expressed using relative dirs, and changing the search path in the middle of a session is not supported.

## cmd

`:cmd ⟨expr⟩`    
Executes ⟨expr⟩ as a computation of type `IO String`, and then executes the resulting string as a list of GHCi commands. Multiple commands are separated by newlines. The `:cmd` command is useful with `:def`  and `:set stop`.

## complete

`:complete ⟨type⟩ ⟨n-m⟩ ⟨string-literal⟩`    
This command allows to request command completions from GHCi even when interacting over a pipe instead of a proper terminal and is designed for integrating GHCi's completion with text editors and IDEs.

When called, `:complete` prints the nth to mth completion candidates for the partial input ⟨string-literal⟩ for the *completion domain* denoted by ⟨type⟩. Currently, only the `repl` domain is supported which denotes the kind of completion that would be provided interactively by GHCi at the input prompt.

If omitted, ⟨n⟩ and ⟨m⟩ default to the first or last available completion candidate respectively. If there are less candidates than requested via the range argument, ⟨n⟩ and ⟨m⟩ are implicitly capped to the number of available completion candidates.

The output of `:complete`  begins with a header line containing 3 fields:
- an integer denoting the number of printed completions, `l`
- an integer denoting the total number of completions available
- a string literal denoting a common prefix to be added to the returned completion candidates

The header line is followed by `⟨l⟩` lines each containing one completion candidate encoded as (quoted) string literal.

Here are some example invocations showing the various cases:

```bash
ghci> :complete repl 0 ""
0 470 ""

ghci> :complete repl 5 "import For"
5 21 "import "
"Foreign"
"Foreign.C"
"Foreign.C.Error"
"Foreign.C.String"
"Foreign.C.Types"

ghci> :complete repl 5-10 "import For"
6 21 "import "
"Foreign.C.Types"
"Foreign.Concurrent"
"Foreign.ForeignPtr"
"Foreign.ForeignPtr.Safe"
"Foreign.ForeignPtr.Unsafe"
"Foreign.Marshal"

ghci> :complete repl 20- "import For"
2 21 "import "
"Foreign.StablePtr"
"Foreign.Storable"

ghci> :complete repl "map"
3 3 ""
"map"
"mapM"
"mapM_"

ghci> :complete repl 5-10 "map"
0 3 ""
```


## continue

`:continue`   
Continue the current evaluation, when stopped at a breakpoint.

## ctags

`:ctags [⟨filename⟩]`    
Generates a "tags" file for Vi-style editors (`:ctags` ) or Emacs-style editors (`:etags`). If no filename is specified, the default `tags` or `TAGS` is used, respectively. Tags for all the functions, constructors and types in the currently loaded modules are created. All modules must be interpreted for these commands to work.

## def

`:def [!] ⟨name⟩ ⟨expr⟩`    
:def is used to define new commands, or macros, in GHCi.
The command `:def ⟨name⟩ ⟨expr⟩` defines a new GHCi command `:name`, implemented by the Haskell expression ⟨expr⟩, which must have type `String -> IO String`. When `:name args` is typed at the prompt, GHCi will run the expression `(name args)`, take the resulting `String`, and feed it back into GHCi as a new sequence of commands. Separate commands in the result must be separated by '\n'.

Here are a few examples:

* command which doesn't take any arguments or produce any results, it just *outputs the current date and time*:

```bash
ghci> let date _ = Data.Time.getZonedTime >>= print >> return ""
ghci> :def date date
ghci> :date
2017-04-10 12:34:56.93213581 UTC
```

* command that takes an argument. It's a re-implementation of `:cd`

```bash
ghci> let mycd d = System.Directory.setCurrentDirectory d >> return ""
ghci> :def mycd mycd
ghci> :mycd ..
```

* command to invoke "`ghc --make Main`" in the cwd:

```bash
ghci> :def make (\_ -> return ":! ghc --make Main")
```

* command that reads GHCi input from a file. This might be useful for creating a set of bindings that we want to repeatedly load into the GHCi session:

```bash
ghci> :def . readFile
ghci> :. cmds.ghci
```


Typing `:def` on its own lists the currently-defined macros. Attempting to redefine an existing command name results in an error unless the `:def!` form is used, in which case the old command with that name is silently overwritten.

However for builtin commands the old command can still be used by preceding the command name with a double colon (eg `::load`). It's not possible to redefine the commands `:{`, `:}` and `:!`.


## delete

`:delete * | ⟨num⟩ ...`     
Delete one or more breakpoints by number (use `:show breaks` to see the number of each breakpoint). The `*` form deletes all the breakpoints.

## disable

`:disable * | ⟨num⟩ ...`    
Disable one or more breakpoints by number (use `:show breaks` to see the number and state of each breakpoint). The `*` form disables all the breakpoints.

## doc

`:doc ⟨name⟩`    
Displays the documentation for the given name. Currently the command is restricted to displaying the documentation directly on the declaration in question, ignoring documentation for arguments, constructors etc.

## edit

`:edit ⟨file⟩`    
Opens an editor to edit the file ⟨file⟩, or the most recently loaded module if ⟨file⟩ is omitted. If there were errors during the last loading, the cursor will be positioned at the line of the first error. The editor to invoke is taken from the `EDITOR` environment variable, or a default editor. You can change the editor using `:set editor`.

## enable

`:enable * | ⟨num⟩ ...`    
Enable one or more disabled breakpoints by number. The `*` form enables all the disabled breakpoints.








:force; ⟨identifier⟩ \...

Prints the value of ⟨identifier⟩ in the same way as
`:print` . Unlike
`:print` , `:force` 
  evaluates each thunk that it encounters while
traversing the value. This may cause exceptions or infinite loops, or
further breakpoints (which are ignored, but displayed).
:::

::: {.ghci-cmd}
:forward; ⟨n⟩

Move forward ⟨n⟩ steps in the history. ⟨n⟩ is one if omitted. See
`tracing`ref"} for more about GHCi's debugging
facilities. See also: `:trace` ,
`:history` , `:back` 
 .
:::

::: {.ghci-cmd}
:help :?

Displays a list of the available commands.
:::

::: {.ghci-cmd}
:

::: {.index}
pair: Repeating last command; in GHCi
:::

Repeat the previous command.
:::

::: {.ghci-cmd}
:history; \[num\]

Display the history of evaluation steps. With a number, displays that
many steps (default: 20). For use with `:trace` 
 ; see `tracing`ref"}. To set
the number of history entries stored by GHCi, use the
`-fghci-hist-size=⟨n⟩`  flag.
:::

::: {.ghci-cmd}
:info;\[!\] ⟨name⟩

Displays information about the given name(s). For example, if ⟨name⟩ is
a class, then the class methods and their types will be printed; if
⟨name⟩ is a type constructor, then its definition will be printed; if
⟨name⟩ is a function, then its type will be printed. If ⟨name⟩ has been
loaded from a source file, then GHCi will also display the location of
its definition in the source.

For types and classes, GHCi also summarises instances that mention them.
To avoid showing irrelevant information, an instance is shown only if
(a) its head mentions ⟨name⟩, and (b) all the other things mentioned in
the instance are in scope (either qualified or otherwise) as a result of
a `:load`  or
`:module`  commands.

The command `:info!` works in a similar fashion but it removes
restriction (b), showing all instances that are in scope and mention
⟨name⟩ in their head.
:::

::: {.ghci-cmd}
:instances; ⟨type⟩

Displays all the class instances available to the argument ⟨type⟩. The
command will match ⟨type⟩ with the first parameter of every instance and
then check that all constraints are satisfiable.

When combined with `PartialTypeSignatures` 
role="extension"}, a user can insert wildcards into a query and learn
the constraints required of each wildcard for ⟨type⟩ match with an
instance.

The output is a listing of all matching instances, simplified and
instantiated as much as possible.

For example:

```bash
> :instances Maybe (Maybe Int)
instance Eq (Maybe (Maybe Int)) -- Defined in ‘GHC.Maybe’
instance Ord (Maybe (Maybe Int)) -- Defined in ‘GHC.Maybe’
instance Show (Maybe (Maybe Int)) -- Defined in ‘GHC.Show’
instance Read (Maybe (Maybe Int)) -- Defined in ‘GHC.Read’

> :set -XPartialTypeSignatures -fno-warn-partial-type-signatures

> :instances Maybe _
instance Eq _ => Eq (Maybe _) -- Defined in ‘GHC.Maybe’
instance Semigroup _ => Monoid (Maybe _) -- Defined in ‘GHC.Base’
instance Ord _ => Ord (Maybe _) -- Defined in ‘GHC.Maybe’
instance Semigroup _ => Semigroup (Maybe _) -- Defined in ‘GHC.Base’
instance Show _ => Show (Maybe _) -- Defined in ‘GHC.Show’
instance Read _ => Read (Maybe _) -- Defined in ‘GHC.Read’
```

Only instances which could potentially be used will be displayed in the
results. Instances which require unsatisfiable constraints such as
`TypeError` will not be included. In the following example, the instance
for `A` is not shown because it cannot be used.

```bash
ghci>:set -XDataKinds -XUndecidableInstances
ghci>import GHC.TypeLits
ghci>class A a
ghci>instance (TypeError (Text "Not possible")) => A Bool
ghci>:instances Bool
instance Eq Bool -- Defined in ‘GHC.Classes’
instance Ord Bool -- Defined in ‘GHC.Classes’
instance Enum Bool -- Defined in ‘GHC.Enum’
instance Show Bool -- Defined in ‘GHC.Show’
instance Read Bool -- Defined in ‘GHC.Read’
instance Bounded Bool -- Defined in ‘GHC.Enum’
```
:::

::: {.ghci-cmd}
:issafe; \[⟨module⟩\]

Displays Safe Haskell information about the given module (or the current
module if omitted). This includes the trust type of the module and its
containing package.
:::

::: {.ghci-cmd}
:kind;\[!\] ⟨type⟩

Infers and prints the kind of ⟨type⟩. The latter can be an arbitrary
type expression, including a partial application of a type constructor,
such as `Either Int`. In fact, `:kind` 
  even allows you to write a partial application of a
type synonym (usually disallowed), so that this works:

```bash
ghci> type T a b = (a,b,a)
ghci> :k T Int Bool
T Int Bool :: *
ghci> :k T
T :: * -> * -> *
ghci> :k T Int
T Int :: * -> *
```

If you specify the optional "`!`", GHC will in addition normalise the
type by expanding out type synonyms and evaluating type-function
applications, and display the normalised result.
:::

::: {.ghci-cmd}
:list; ⟨identifier⟩

Lists the source code around the definition of ⟨identifier⟩ or the
current breakpoint if not given. This requires that the identifier be
defined in an interpreted module. If your output device supports it,
then GHCi will highlight the active subexpression in bold.
:::

::: {.ghci-cmd}
:list \[⟨module⟩\]; ⟨line⟩

Lists the source code around the given line number of ⟨module⟩. This
requires that the module be interpreted. If your output device supports
it, then GHCi will highlight the active subexpression in bold.
:::

::: {.ghci-cmd}
:load;\[!\] \[\*\]⟨module⟩

Recursively loads the specified ⟨module⟩s, and all the modules they
depend on. Here, each ⟨module⟩ must be a module name or filename, but
may not be the name of a module in a package.

All previously loaded modules, except package modules, are forgotten.
The new set of modules is known as the target set. Note that
`:load`  can be used without any
arguments to unload all the currently loaded modules and bindings.

Normally pre-compiled code for a module will be loaded if available, or
otherwise the module will be compiled to byte-code. Using the `*` prefix
forces a module to be loaded as byte-code.

Adding the optional "`!`" turns type errors into warnings while
loading. This allows to use the portions of the module that are correct,
even if there are type errors in some definitions. Effectively, the
"-fdefer-type-errors" flag is set before loading and unset after
loading if the flag has not already been set before. See
`defer-type-errors`ref"} for further motivation
and details.

After a `:load`  command, the current
context is set to:

-   ⟨module⟩, if it was loaded successfully, or
-   the most recently successfully loaded module, if any other modules
    were loaded as a result of the current `:load` 
     , or
-   `Prelude` otherwise.
:::

::: {.ghci-cmd}
:loc-at; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ \[⟨name⟩\]

Tries to find the definition site of the name at the given source-code
span, e.g.:

```bash
X> :loc-at X.hs 6 14 6 16 mu
X.hs:(8,7)-(8,9)
```

This command is useful when integrating GHCi with text editors and IDEs
for providing a goto-definition facility.

The `:loc-at` command requires `:set +c` 
  to be set.
:::

::: {.ghci-cmd}
:main; ⟨arg1⟩ \... ⟨argn⟩

When a program is compiled and executed, it can use the `getArgs`
function to access the command-line arguments. However, we cannot simply
pass the arguments to the `main` function while we are testing in ghci,
as the `main` function doesn't take its arguments directly.

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
:::

::: {.ghci-cmd}
:module; +\|- \[\*\]⟨mod1⟩ \...
:::

::: {.ghci-cmd}
import; ⟨mod⟩

Sets or modifies the current context for statements typed at the prompt.
The form `import mod` is equivalent to `:module +mod`. See
`ghci-scope`ref"} for more details.
:::

::: {.ghci-cmd}
:print; ⟨names⟩

Prints a value without forcing its evaluation.
`:print`  may be used on values whose
types are unknown or partially known, which might be the case for local
variables with polymorphic types at a breakpoint. While inspecting the
runtime value, `:print`  attempts to
reconstruct the type of the value, and will elaborate the type in
GHCi's environment if possible. If any unevaluated components (thunks)
are encountered, then `:print`  binds
a fresh variable with a name beginning with `_t` to each thunk. See
`breakpoints`ref"} for more information. See
also the `:sprint`  command, which
works like `:print`  but does not bind
new variables.
:::

::: {.ghci-cmd}
:quit

Quits GHCi. You can also quit by typing `Control-D` 
role="kbd"} at the prompt.
:::

::: {.ghci-cmd}
:reload;\[!\]

Attempts to reload the current target set (see `:load` 
 ) if any of the modules in the set, or any dependent
module, has changed. Note that this may entail loading new modules, or
dropping modules which are no longer indirectly required by the target.

Adding the optional "`!`" turns type errors into warnings while
loading. This allows to use the portions of the module that are correct,
even if there are type errors in some definitions. Effectively, the
"-fdefer-type-errors" flag is set before loading and unset after
loading if the flag has not already been set before. See
`defer-type-errors`ref"} for further motivation
and details.
:::

::: {.ghci-cmd}
:run

See `:main` .
:::

::: {.ghci-cmd}
:script; \[⟨n⟩\] ⟨filename⟩

Executes the lines of a file as a series of GHCi commands. The syntax
for file-name arguments respects shell quoting rules, i.e., file names
containing spaces can be enclosed in double quotes or with spaces
escaped with a backslash. This command is compatible with multiline
statements as set by `:set +m` 
:::

::: {.ghci-cmd}
:set; \[⟨option⟩ \...\]

Sets various options. See `ghci-set`ref"} for a
list of available options and
`interactive-mode-options`ref"} for a list of
GHCi-specific flags. The `:set` 
command by itself shows which options are currently set. It also lists
the current dynamic flag settings, with GHCi-specific flags listed
separately.
:::

::: {.ghci-cmd}
:set args; ⟨arg⟩

::: {.index}
single: getArgs, behavior in GHCi
:::

Sets the list of arguments which are returned when the program calls
`System.getArgs`.
:::

::: {.ghci-cmd}
:set editor; ⟨cmd⟩

Sets the command used by `:edit`  to
⟨cmd⟩.
:::

::: {.ghci-cmd}
:set local-config; ⟨source\|ignore⟩

If `ignore`, `./.ghci`file"} files will be
ignored (sourcing untrusted local scripts is a security risk). The
default is `source`. Set this directive in your user
`.ghci`file"} script, i.e. before the local
script would be sourced.

Even when set to `ignore`, a local script will still be processed if
given by `-ghci-script`  on the
command line, or sourced via `:script` 
 .
:::

::: {.ghci-cmd}
:set prog; ⟨prog⟩

::: {.index}
single: getProgName, behavior in GHCi
:::

Sets the string to be returned when the program calls
`System.getProgName`.
:::

::: {.ghci-cmd}
:set prompt; ⟨prompt⟩

::: {.index}
single: GHCi prompt; setting
:::

Sets the string to be used as the prompt in GHCi. Inside ⟨prompt⟩, the
next sequences are replaced:

-   `%s` by the names of the modules currently in scope.
-   `%l` by the line number (as referenced in compiler messages) of the
    current prompt.
-   `%d` by the date in "Weekday Month Date" format (e.g., "Tue May
    26") .
-   `%t` by the current time in 24-hour HH:MM:SS format.
-   `%T` by the current time in 12-hour HH:MM:SS format.
-   `%@` by the current time in 12-hour am/pm format.
-   `%A` by the current time in 24-hour HH:MM format.
-   `%u` by the username of the current user.
-   `%w` by the current working directory.
-   `%o` by the operating system.
-   `%a` by the machine architecture.
-   `%N` by the compiler name.
-   `%V` by the compiler version.
-   `%call(cmd [args])` by the result of calling `cmd args`.
-   `%%` by `%`.

If ⟨prompt⟩ starts with `"` then it is parsed as a Haskell String;
otherwise it is treated as a literal string.
:::

::: {.ghci-cmd}
:set prompt-cont; ⟨prompt⟩

Sets the string to be used as the continuation prompt (used when using
the `:{`  command) in GHCi.
:::

::: {.ghci-cmd}
:set prompt-function; ⟨prompt-function⟩

::: {.index}
single: GHCi prompt function; setting
:::

Sets the function to be used for the prompt displaying in GHCi. The
function should be of the type `[String] -> Int -> IO String`. This
function is called each time the prompt is being made. The first
argument stands for the names of the modules currently in scope(the name
of the "topmost" module will begin with a `*`; see
`ghci-scope`ref"} for more information). The
second arguments is the line number (as referenced in compiler messages)
of the current prompt.
:::

::: {.ghci-cmd}
:set prompt-cont-function; ⟨prompt-function⟩

Sets the function to be used for the continuation prompt (used when
using the `:{`  command) displaying in
GHCi.
:::

::: {.ghci-cmd}
:set stop; ⟨num⟩ ⟨cmd⟩

Set a command to be executed when a breakpoint is hit, or a new item in
the history is selected. The most common use of
`:set stop`  is to display the source
code at the current location, e.g. `:set stop :list`.

If a number is given before the command, then the commands are run when
the specified breakpoint (only) is hit. This can be quite useful: for
example, `:set stop 1 :continue` effectively disables breakpoint 1, by
running `:continue`  whenever it is
hit In this case GHCi will still emit a message to say the breakpoint
was hit. If you don't want such a message, you can use the
`:disable`  command. What's more,
with cunning use of `:def`  and
`:cmd`  you can use
`:set stop`  to implement conditional
breakpoints:

```bash
*ghci> :def cond \expr -> return (":cmd if (" ++ expr ++ ") then return "" else return ":continue"")
*ghci> :set stop 0 :cond (x < 3)
```

Ignoring breakpoints for a specified number of iterations is also
possible using similar techniques.
:::

::: {.ghci-cmd}
:seti; \[⟨option⟩ \...\]

Like `:set` , but options set with
`:seti`  affect only expressions and
commands typed at the prompt, and not modules loaded with
`:load`  (in contrast, options set
with `:set`  apply everywhere). See
`ghci-interactive-options`ref"}.

Without any arguments, displays the current set of options that are
applied to expressions and commands typed at the prompt.
:::

::: {.ghci-cmd}
:show bindings

Show the bindings made at the prompt and their types.
:::

::: {.ghci-cmd}
:show breaks

List the active breakpoints.
:::

::: {.ghci-cmd}
:show context

List the active evaluations that are stopped at breakpoints.
:::

::: {.ghci-cmd}
:show imports

Show the imports that are currently in force, as created by `import` and
`:module`  commands.
:::

::: {.ghci-cmd}
:show modules

Show the list of modules currently loaded.
:::

::: {.ghci-cmd}
:show packages

Show the currently active package flags, as well as the list of packages
currently loaded.
:::

::: {.ghci-cmd}
:show paths

Show the current working directory (as set via `:cd` 
  command), as well as the list of directories searched
for source files (as set by the `-i` option).
:::

::: {.ghci-cmd}
:show language

Show the currently active language flags for source files.
:::

::: {.ghci-cmd}
:showi language

Show the currently active language flags for expressions typed at the
prompt (see also `:seti` ).
:::

::: {.ghci-cmd}
:show; \[argspromptstop\]

Displays the specified setting (see `:set` 
 ).
:::

::: {.ghci-cmd}
:sprint; ⟨expr⟩

Prints a value without forcing its evaluation.
`:sprint`  is similar to
`:print` , with the difference that
unevaluated subterms are not bound to new variables, they are simply
denoted by `_`.
:::

::: {.ghci-cmd}
:step; \[⟨expr⟩\]

Enable all breakpoints and begin evaluating an expression in
single-stepping mode. In this mode evaluation will be stopped after
every reduction, allowing local variables to be inspected. If ⟨expr⟩ is
not given, evaluation will resume at the last breakpoint. See
`single-stepping`ref"}.
:::

::: {.ghci-cmd}
:steplocal

Enable only breakpoints in the current top-level binding and resume
evaluation at the last breakpoint. Continuation with
`:steplocal`  is not possible if this
last breakpoint was hit by an error
(`-fbreak-on-error` ) or an exception
(`-fbreak-on-exception` ).
:::

::: {.ghci-cmd}
:stepmodule

Enable only breakpoints in the current module and resume evaluation at
the last breakpoint.
:::

::: {.ghci-cmd}
:trace; ⟨expr⟩

Evaluates the given expression (or from the last breakpoint if no
expression is given), and additionally logs the evaluation steps for
later inspection using `:history` .
See `tracing`ref"}.
:::

::: {.ghci-cmd}
:type; ⟨expression⟩

Infers and prints the type of ⟨expression⟩, including explicit forall
quantifiers for polymorphic types. The type reported is the type that
would be inferred for a variable assigned to the expression, but without
the monomorphism restriction applied.

```bash
```

\*X\> :type length length :: Foldable t =\> t a -\> Int
:::

::: {.ghci-cmd}
:type +v; ⟨expression⟩

Infers and prints the type of ⟨expression⟩, but without fiddling with
type variables or class constraints. This is useful when you are using
`TypeApplications`extension"} and care about
the distinction between specified type variables (available for type
application) and inferred type variables (not available). This mode
sometimes prints constraints (such as `Show Int`) that could readily be
solved, but solving these constraints may affect the type variables, so
GHC refrains.

```bash
```

*X\> :set -fprint-explicit-foralls*X\> :type +v length length :: forall
(t :: \* -\> \*). Foldable t =\> forall a. t a -\> Int
:::

::: {.ghci-cmd}
:type +d; ⟨expression⟩

Infers and prints the type of ⟨expression⟩, defaulting type variables if
possible. In this mode, if the inferred type is constrained by any
interactive class (`Num`, `Show`, `Eq`, `Ord`, `Foldable`, or
`Traversable`), the constrained type variable(s) are defaulted according
to the rules described under `ExtendedDefaultRules` 
role="extension"}. This mode is quite useful when the inferred type is
quite general (such as for `foldr`) and it may be helpful to see a more
concrete instantiation.

```bash
```

\*X\> :type +d length length :: \[a\] -\> Int
:::

::: {.ghci-cmd}
:type-at; ⟨path⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ \[⟨name⟩\]

Reports the inferred type at the given span/position in the module,
e.g.:

```bash
*X> :type-at X.hs 6 6 6 7 f
Int -> Int
```

This command is useful when integrating GHCi with text editors and IDEs
for providing a show-type-under-point facility.

The first parameter (path) must be a file path and not a module name.
The type of this path is dependent on how the module was loaded into
GHCi: If the module was loaded by name, then the path name calculated by
GHCi as described in `ghci-modules-filenames` 
role="ref"} must be used. If the module was loaded with an absolute or a
relative path, then the same path must be specified.

The last string parameter is useful for when the span is out of date,
i.e. the file changed and the code has moved. In which case
`:type-at`  falls back to a general
`:type`  like lookup.

The `:type-at`  command requires
`:set +c`  to be set.
:::

::: {.ghci-cmd}
:undef; ⟨name⟩

Undefines the user-defined command ⟨name⟩ (see `:def` 
  above).
:::

::: {.ghci-cmd}
:unset; ⟨option⟩

Unsets certain options. See `ghci-set`ref"} for
a list of available options.
:::

::: {.ghci-cmd}
:uses; ⟨module⟩ ⟨line⟩ ⟨col⟩ ⟨end-line⟩ ⟨end-col⟩ \[⟨name⟩\]

Reports all module-local uses of the thing at the given position in the
module, e.g.:

```bash
:uses GhciFind.hs 53 66 53 70 name
GhciFind.hs:(46,25)-(46,29)
GhciFind.hs:(47,37)-(47,41)
GhciFind.hs:(53,66)-(53,70)
GhciFind.hs:(57,62)-(57,66)
```

This command is useful for highlighting and navigating all uses of an
identifier in editors and IDEs.

The `:uses`  command requires
`:set +c`  to be set.
:::

::: {.ghci-cmd}
:: ⟨builtin-command⟩

Executes the GHCi built-in command (e.g. `::type 3`). That is, look up
on the list of builtin commands, excluding defined macros. See also:
`:def` .
:::

::: {.ghci-cmd}
:! ⟨command⟩

::: {.index}
single: shell commands; in GHCi
:::

Executes the shell command ⟨command⟩.
:::

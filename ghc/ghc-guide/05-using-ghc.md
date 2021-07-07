# Using GHC

## Getting started: compiling programs

In this chapter you\'ll find a complete reference to the GHC
command-line syntax, including all 400+ flags. It\'s a large and complex
system, and there are lots of details, so it can be quite hard to figure
out how to get started. With that in mind, this introductory section
provides a quick introduction to the basic usage of GHC for compiling a
Haskell program, before the following sections dive into the full
syntax.

Let\'s create a Hello World program, and compile and run it. First,
create a file `hello.hs`{.interpreted-text role="file"} containing the
Haskell code: :

    main = putStrLn "Hello, World!"

To compile the program, use GHC like this:

```sh
$ ghc hello.hs
```

(where `$` represents the prompt: don\'t type it). GHC will compile the
source file `hello.hs`{.interpreted-text role="file"}, producing an
object file `hello.o`{.interpreted-text role="file"} and an interface
file `hello.hi`{.interpreted-text role="file"}, and then it will link
the object file to the libraries that come with GHC to produce an
executable called `hello`{.interpreted-text role="file"} on
Unix/Linux/Mac, or `hello.exe`{.interpreted-text role="file"} on
Windows.

By default GHC will be very quiet about what it is doing, only printing
error messages. If you want to see in more detail what\'s going on
behind the scenes, add `-v`{.interpreted-text role="ghc-flag"} to the
command line.

Then we can run the program like this:

```sh
$ ./hello
Hello World!
```

If your program contains multiple modules, then you only need to tell
GHC the name of the source file containing the `Main` module, and GHC
will examine the `import` declarations to find the other modules that
make up the program and find their source files. This means that, with
the exception of the `Main` module, every source file should be named
after the module name that it contains (with dots replaced by directory
separators). For example, the module `Data.Person` would be in the file
`Data/Person.hs` on Unix/Linux/Mac, or `Data\Person.hs` on Windows.

Options overview
----------------

GHC\'s behaviour is controlled by options, which for historical reasons
are also sometimes referred to as command-line flags or arguments.
Options can be specified in three ways:

### Command-line arguments

An invocation of GHC takes the following form:


Command-line arguments are either options or file names.

Command-line options begin with `-`. They may *not* be grouped: `-vO` is
different from `-v -O`. Options need not precede filenames: e.g.,
`ghc *.o -o foo`. All options are processed and then applied to all
files; you cannot, for example, invoke `ghc -c -O1 Foo.hs -O2 Bar.hs` to
apply different optimisation levels to the files `Foo.hs` and `Bar.hs`.


Note that command-line options are *order-dependent*, with arguments
being evaluated from left-to-right. This can have seemingly strange
effects in the presence of flag implication. For instance, consider
`-fno-specialise <-fspecialise>`{.interpreted-text role="ghc-flag"} and
`-O1`{.interpreted-text role="ghc-flag"} (which implies
`-fspecialise`{.interpreted-text role="ghc-flag"}). These two command
lines mean very different things:

`-fno-specialise -O1`

> `-fspecialise` will be enabled as the `-fno-specialise` is overridden
> by the `-O1`.

`-O1 -fno-specialise`

> `-fspecialise` will not be enabled, since the `-fno-specialise`
> overrides the `-fspecialise` implied by `-O1`.
:::

### Command line options in source files {#source-file-options}

::: {.index}
single: source-file options
:::

Sometimes it is useful to make the connection between a source file and
the command-line options it requires quite tight. For instance, if a
Haskell source file deliberately uses name shadowing, it should be
compiled with the `-Wno-name-shadowing` option. Rather than maintaining
the list of per-file options in a `Makefile`, it is possible to do this
directly in the source file using the `OPTIONS_GHC`
`pragma <options-pragma>`{.interpreted-text role="ref"} :

    {-# OPTIONS_GHC -Wno-name-shadowing #-}
    module X where
    ...

`OPTIONS_GHC` is a *file-header pragma* (see
`options-pragma`{.interpreted-text role="ref"}).

Only *dynamic* flags can be used in an `OPTIONS_GHC` pragma (see
`mode-dynamic-flags`{.interpreted-text role="ref"}).

Note that your command shell does not get to the source file options,
they are just included literally in the array of command-line arguments
the compiler maintains internally, so you\'ll be desperately
disappointed if you try to glob etc. inside `OPTIONS_GHC`.

::: {.note}
::: {.admonition-title}
Note
:::

The contents of `OPTIONS_GHC` are appended to the command-line options,
so options given in the source file override those given on the
command-line.
:::

It is not recommended to move all the contents of your Makefiles into
your source files, but in some circumstances, the `OPTIONS_GHC` pragma
is the Right Thing. (If you use `-keep-hc-file`{.interpreted-text
role="ghc-flag"} and have `OPTION` flags in your module, the
`OPTIONS_GHC` will get put into the generated `.hc` file).

### Setting options in GHCi

Options may also be modified from within GHCi, using the
`:set`{.interpreted-text role="ghci-cmd"} command.

Dynamic and Mode options {#mode-dynamic-flags}
------------------------

::: {.index}
single: dynamic; options single: mode; options
:::

Each of GHC\'s command line options is classified as dynamic or mode:

> Mode: A mode may be used on the command line only. You can pass only
> one mode flag. For example, `--make`{.interpreted-text
> role="ghc-flag"} or `-E`{.interpreted-text role="ghc-flag"}. The
> available modes are listed in `modes`{.interpreted-text role="ref"}.
>
> Dynamic: A dynamic flag may be used on the command line, in a
> `OPTIONS_GHC` pragma in a source file, or set using
> `:set`{.interpreted-text role="ghci-cmd"} in GHCi.

The flag reference tables (`flag-reference`{.interpreted-text
role="ref"}) lists the status of each flag.

Meaningful file suffixes {#file-suffixes}
------------------------

::: {.index}
single: suffixes, file single: file suffixes for GHC
:::

File names with \"meaningful\" suffixes (e.g., `.lhs` or `.o`) cause the
\"right thing\" to happen to those files.

`.hs`

:   A Haskell module.

`.lhs`

:   ::: {.index}
    single: lhs file extension
    :::

    A "literate Haskell" module.

`.hspp`

:   A file created by the preprocessor.

`.hi`

:   A Haskell interface file, probably compiler-generated.

`.hie`

:   An extended Haskell interface file, produced by the Haskell
    compiler.

`.hc`

:   Intermediate C file produced by the Haskell compiler.

`.c`

:   A C file not produced by the Haskell compiler.

`.ll`

:   An llvm-intermediate-language source file, usually produced by the
    compiler.

`.bc`

:   An llvm-intermediate-language bitcode file, usually produced by the
    compiler.

`.s`

:   An assembly-language source file, usually produced by the compiler.

`.o`

:   An object file, produced by an assembler.

Files with other suffixes (or without suffixes) are passed straight to
the linker.

Modes of operation {#modes}
------------------

::: {.index}
single: help options
:::

GHC\'s behaviour is firstly controlled by a mode flag. Only one of these
flags may be given, but it does not necessarily need to be the first
option on the command-line. For instance,

``` {.sourceCode .none}
$ ghc Main.hs --make -o my-application
```

If no mode flag is present, then GHC will enter
`--make`{.interpreted-text role="ghc-flag"} mode
(`make-mode`{.interpreted-text role="ref"}) if there are any Haskell
source files given on the command line, or else it will link the objects
named on the command line to produce an executable.

The available mode flags are:

::: {.ghc-flag shortdesc="Interactive mode - normally used by just running ``ghci``;
see :ref:`ghci` for details." type="mode" category="modes"}
\--interactive

::: {.index}
single: interactive mode single: GHCi
:::

Interactive mode, which is also available as `ghci`{.interpreted-text
role="program"}. Interactive mode is described in more detail in
`ghci`{.interpreted-text role="ref"}.
:::

::: {.ghc-flag shortdesc="Build a multi-module Haskell program, automatically figuring out
dependencies. Likely to be much easier, and faster, than using
``make``; see :ref:`make-mode` for details." type="mode" category="modes"}
\--make

::: {.index}
single: make mode; of GHC
:::

In this mode, GHC will build a multi-module Haskell program
automatically, figuring out dependencies for itself. If you have a
straightforward Haskell program, this is likely to be much easier, and
faster, than using `make`{.interpreted-text role="command"}. Make mode
is described in `make-mode`{.interpreted-text role="ref"}.

This mode is the default if there are any Haskell source files mentioned
on the command line, and in this case the `--make`{.interpreted-text
role="ghc-flag"} option can be omitted.
:::

::: {.ghc-flag shortdesc="Evaluate ``expr``; see :ref:`eval-mode` for details." type="mode" category="modes"}
-e ⟨expr⟩

::: {.index}
single: eval mode; of GHC
:::

Expression-evaluation mode. This is very similar to interactive mode,
except that there is a single expression to evaluate (⟨expr⟩) which is
given on the command line. See `eval-mode`{.interpreted-text role="ref"}
for more details.
:::

::: {.ghc-flag shortdesc="Stop after preprocessing (``.hspp`` file)" type="mode" category="phases"}
-E

Stop after preprocessing (`.hspp` file)
:::

::: {.ghc-flag shortdesc="Stop after generating C (``.hc`` file)" type="mode" category="phases"}
-C

Stop after generating C (`.hc` file)
:::

::: {.ghc-flag shortdesc="Stop after generating assembly (``.s`` file)" type="mode" category="phases"}
-S

Stop after generating assembly (`.s` file)
:::

::: {.ghc-flag shortdesc="Stop after generating object (``.o``) file" type="mode" category="phases"}
-c

Stop after generating object (`.o`) file

This is the traditional batch-compiler mode, in which GHC can compile
source files one at a time, or link objects together into an executable.
See `options-order`{.interpreted-text role="ref"}.
:::

::: {.ghc-flag shortdesc="generate dependency information suitable for use in a
``Makefile``; see :ref:`makefile-dependencies` for details." type="mode" category="modes"}
-M

::: {.index}
single: dependency-generation mode; of GHC
:::

Dependency-generation mode. In this mode, GHC can be used to generate
dependency information suitable for use in a `Makefile`. See
`makefile-dependencies`{.interpreted-text role="ref"}.
:::

::: {.ghc-flag shortdesc="run GHC with the given frontend plugin; see
:ref:`frontend_plugins` for details." type="mode" category="modes"}
\--frontend ⟨module⟩

::: {.index}
single: frontend plugins; using
:::

Run GHC using the given frontend plugin. See
`frontend_plugins`{.interpreted-text role="ref"} for details.
:::

::: {.ghc-flag shortdesc="DLL-creation mode (Windows only)" type="mode" category="modes"}
\--mk-dll

::: {.index}
single: DLL-creation mode
:::

DLL-creation mode (Windows only). See
`win32-dlls-create`{.interpreted-text role="ref"}.
:::

::: {.ghc-flag shortdesc="Display help" type="mode" category="modes"}
\--help -?

Cause GHC to spew a long usage message to standard output and then exit.
:::

::: {.ghc-flag shortdesc="display the contents of an interface file." type="mode" category="modes"}
\--show-iface ⟨file⟩

Read the interface in ⟨file⟩ and dump it as text to `stdout`. For
example `ghc --show-iface M.hi`.
:::

::: {.ghc-flag shortdesc="display the supported language extensions" type="mode" category="modes"}
\--supported-extensions \--supported-languages

Print the supported language extensions.
:::

::: {.ghc-flag shortdesc="display the supported command line options" type="mode" category="modes"}
\--show-options

Print the supported command line options. This flag can be used for
autocompletion in a shell.
:::

::: {.ghc-flag shortdesc="display information about the compiler" type="mode" category="modes"}
\--info

Print information about the compiler.
:::

::: {.ghc-flag shortdesc="display GHC version" type="mode" category="modes"}
\--version -V

Print a one-line string including GHC\'s version number.
:::

::: {.ghc-flag shortdesc="display GHC version (numeric only)" type="mode" category="modes"}
\--numeric-version

Print GHC\'s numeric version number only.
:::

::: {.ghc-flag shortdesc="display GHC library directory" type="mode" category="modes"}
\--print-libdir

::: {.index}
single: libdir
:::

Print the path to GHC\'s library directory. This is the top of the
directory tree containing GHC\'s libraries, interfaces, and include
files (usually something like `/usr/local/lib/ghc-5.04` on Unix). This
is the value of `$libdir` in the package configuration file (see
`packages`{.interpreted-text role="ref"}).
:::

### Using `ghc` `--make` {#make-mode}

::: {.index}
single: \--make; mode of GHC single: separate compilation
:::

In this mode, GHC will build a multi-module Haskell program by following
dependencies from one or more root modules (usually just `Main`). For
example, if your `Main` module is in a file called
`Main.hs`{.interpreted-text role="file"}, you could compile and link the
program like this:

``` {.sourceCode .none}
ghc --make Main.hs
```

In fact, GHC enters make mode automatically if there are any Haskell
source files on the command line and no other mode is specified, so in
this case we could just type

``` {.sourceCode .none}
ghc Main.hs
```

Any number of source file names or module names may be specified; GHC
will figure out all the modules in the program by following the imports
from these initial modules. It will then attempt to compile each module
which is out of date, and finally, if there is a `Main` module, the
program will also be linked into an executable.

The main advantages to using `ghc --make` over traditional `Makefile`s
are:

-   GHC doesn\'t have to be restarted for each compilation, which means
    it can cache information between compilations. Compiling a
    multi-module program with `ghc --make` can be up to twice as fast as
    running `ghc` individually on each source file.
-   You don\'t have to write a `Makefile`.

    ::: {.index}
    single: Makefiles; avoiding
    :::

-   GHC re-calculates the dependencies each time it is invoked, so the
    dependencies never get out of sync with the source.
-   Using the `-j[⟨n⟩]`{.interpreted-text role="ghc-flag"} flag, you can
    compile modules in parallel. Specify `-j ⟨n⟩` to compile ⟨n⟩ jobs in
    parallel. If ⟨n⟩ is omitted, then it defaults to the number of
    processors.

Any of the command-line options described in the rest of this chapter
can be used with `--make`, but note that any options you give on the
command line will apply to all the source files compiled, so if you want
any options to apply to a single source file only, you\'ll need to use
an `OPTIONS_GHC` pragma (see `source-file-options`{.interpreted-text
role="ref"}).

If the program needs to be linked with additional objects (say, some
auxiliary C code), then the object files can be given on the command
line and GHC will include them when linking the executable.

For backward compatibility with existing make scripts, when used in
combination with `-c`{.interpreted-text role="ghc-flag"}, the linking
phase is omitted (same as `--make -no-link`).

Note that GHC can only follow dependencies if it has the source file
available, so if your program includes a module for which there is no
source file, even if you have an object and an interface file for the
module, then GHC will complain. The exception to this rule is for
package modules, which may or may not have source files.

The source files for the program don\'t all need to be in the same
directory; the `-i`{.interpreted-text role="ghc-flag"} option can be
used to add directories to the search path (see
`search-path`{.interpreted-text role="ref"}).

::: {.ghc-flag shortdesc="When compiling with :ghc-flag:`--make`, compile ⟨n⟩ modules
in parallel." type="dynamic" category="misc"}
-j\[⟨n⟩\]

Perform compilation in parallel when possible. GHC will use up to ⟨N⟩
threads during compilation. If N is omitted, then it defaults to the
number of processors. Note that compilation of a module may not begin
until its dependencies have been built.
:::

### Expression evaluation mode {#eval-mode}

This mode is very similar to interactive mode, except that there is a
single expression to evaluate which is specified on the command line as
an argument to the `-e` option:

``` {.sourceCode .none}
ghc -e expr
```

Haskell source files may be named on the command line, and they will be
loaded exactly as in interactive mode. The expression is evaluated in
the context of the loaded modules.

For example, to load and run a Haskell program containing a module
`Main`, we might say:

``` {.sourceCode .none}
ghc -e Main.main Main.hs
```

or we can just use this mode to evaluate expressions in the context of
the `Prelude`:

``` {.sourceCode .none}
$ ghc -e "interact (unlines.map reverse.lines)"
hello
olleh
```

### Batch compiler mode {#options-order}

In *batch mode*, GHC will compile one or more source files given on the
command line.

The first phase to run is determined by each input-file suffix, and the
last phase is determined by a flag. If no relevant flag is present, then
go all the way through to linking. This table summarises:

  ------------------------------------------------------------------------
  Phase of the         Suffix saying     Flag saying      (suffix of)
  compilation system   "start here"      "stop after"     output file
  -------------------- ----------------- ---------------- ----------------
  literate             `.lhs`                             `.hs`
  pre-processor                                           

  C pre-processor      `.hs` (with       `-E`             `.hspp`
  (opt.)               `-cpp`)                            

  Haskell compiler     `.hs`             `-C`, `-S`       `.hc`, `.s`

  C compiler (opt.)    `.hc` or `.c`     `-S`             `.s`

  assembler            `.s`              `-c`             `.o`

  linker               ⟨other⟩                            `a.out`
  ------------------------------------------------------------------------

::: {.index}
single: -C single: -E single: -S single: -c
:::

Thus, a common invocation would be:

``` {.sourceCode .none}
ghc -c Foo.hs
```

to compile the Haskell source file `Foo.hs` to an object file `Foo.o`.

::: {.note}
::: {.admonition-title}
Note
:::

What the Haskell compiler proper produces depends on what backend code
generator is used. See `code-generators`{.interpreted-text role="ref"}
for more details.
:::

::: {.note}
::: {.admonition-title}
Note
:::

Pre-processing is optional, the `-cpp`{.interpreted-text
role="ghc-flag"} flag turns it on. See
`c-pre-processor`{.interpreted-text role="ref"} for more details.
:::

::: {.note}
::: {.admonition-title}
Note
:::

The option `-E`{.interpreted-text role="ghc-flag"} runs just the
pre-processing passes of the compiler, dumping the result in a file.
:::

::: {.note}
::: {.admonition-title}
Note
:::

The option `-C`{.interpreted-text role="ghc-flag"} is only available
when GHC is built in unregisterised mode. See `unreg`{.interpreted-text
role="ref"} for more details.
:::

#### Overriding the default behaviour for a file {#overriding-suffixes}

As described above, the way in which a file is processed by GHC depends
on its suffix. This behaviour can be overridden using the
`-x ⟨suffix⟩`{.interpreted-text role="ghc-flag"} option:

::: {.ghc-flag shortdesc="Override default behaviour for source files" type="dynamic" category="phases"}
-x ⟨suffix⟩

Causes all files following this option on the command line to be
processed as if they had the suffix ⟨suffix⟩. For example, to compile a
Haskell module in the file `M.my-hs`, use `ghc -c -x hs M.my-hs`.
:::

Verbosity options {#options-help}
-----------------

::: {.index}
single: verbosity options
:::

See also the `--help`, `--version`, `--numeric-version`, and
`--print-libdir` modes in `modes`{.interpreted-text role="ref"}.

::: {.ghc-flag shortdesc="verbose mode (equivalent to ``-v3``)" type="dynamic" category="verbosity"}
-v

The `-v`{.interpreted-text role="ghc-flag"} option makes GHC *verbose*:
it reports its version number and shows (on stderr) exactly how it
invokes each phase of the compilation system. Moreover, it passes the
`-v` flag to most phases; each reports its version number (and possibly
some other information).

Please, oh please, use the `-v` option when reporting bugs! Knowing that
you ran the right bits in the right order is always the first thing we
want to verify.
:::

::: {.ghc-flag shortdesc="set verbosity level" type="dynamic" category="verbosity"}
-v⟨n⟩

To provide more control over the compiler\'s verbosity, the `-v` flag
takes an optional numeric argument. Specifying `-v` on its own is
equivalent to `-v3`, and the other levels have the following meanings:

`-v0`

:   Disable all non-essential messages (this is the default).

`-v1`

:   Minimal verbosity: print one line per compilation (this is the
    default when `--make`{.interpreted-text role="ghc-flag"} or
    `--interactive`{.interpreted-text role="ghc-flag"} is on).

`-v2`

:   Print the name of each compilation phase as it is executed.
    (equivalent to `-dshow-passes`{.interpreted-text role="ghc-flag"}).

`-v3`

:   The same as `-v2`, except that in addition the full command line (if
    appropriate) for each compilation phase is also printed.

`-v4`

:   The same as `-v3` except that the intermediate program
    representation after each compilation phase is also printed
    (excluding preprocessed and C/assembly files).
:::

::: {.ghc-flag shortdesc="display all available instances in type error messages" type="dynamic" reverse="-fno-print-potential-instances" category="verbosity"}
-fprint-potential-instances

When GHC can\'t find an instance for a class, it displays a short list
of some in the instances it knows about. With this flag it prints *all*
the instances it knows about.
:::

::: {.ghc-flag shortdesc="hide module source and object paths" type="dynamic" category="verbosity"}
-fhide-source-paths

Starting with minimal verbosity (`-v1`, see `-v`{.interpreted-text
role="ghc-flag"}), GHC displays the name, the source path and the target
path of each compiled module. This flag can be used to reduce GHC\'s
output by hiding source paths and target paths.
:::

The following flags control the way in which GHC displays types in error
messages and in GHCi:

::: {.ghc-flag shortdesc="Use unicode syntax when printing expressions, types and kinds.
See also :extension:`UnicodeSyntax`" type="dynamic" reverse="-fno-print-unicode-syntax" category="verbosity"}
-fprint-unicode-syntax

When enabled GHC prints type signatures using the unicode symbols from
the `UnicodeSyntax`{.interpreted-text role="extension"} extension. For
instance,

``` {.sourceCode .none}
ghci> :set -fprint-unicode-syntax
ghci> :t +v (>>)
(>>) ∷ Monad m ⇒ ∀ a b. m a → m b → m b
```
:::

::: {#pretty-printing-types}
::: {.ghc-flag shortdesc="Print explicit ``forall`` quantification in types.
See also :extension:`ExplicitForAll`" type="dynamic" reverse="-fno-print-explicit-foralls" category="verbosity"}
-fprint-explicit-foralls

Using `-fprint-explicit-foralls`{.interpreted-text role="ghc-flag"}
makes GHC print explicit `forall` quantification at the top level of a
type; normally this is suppressed. For example, in GHCi:

``` {.sourceCode .none}
ghci> let f x = x
ghci> :t f
f :: a -> a
ghci> :set -fprint-explicit-foralls
ghci> :t f
f :: forall a. a -> a
```

However, regardless of the flag setting, the quantifiers are printed
under these circumstances:

-   For nested `foralls`, e.g.

    ``` {.sourceCode .none}
    ghci> :t GHC.ST.runST
    GHC.ST.runST :: (forall s. GHC.ST.ST s a) -> a
    ```

-   If any of the quantified type variables has a kind that mentions a
    kind variable, e.g.

    ``` {.sourceCode .none}
    ghci> :i Data.Type.Equality.sym
    Data.Type.Equality.sym ::
      forall k (a :: k) (b :: k).
      (a Data.Type.Equality.:~: b) -> b Data.Type.Equality.:~: a
            -- Defined in Data.Type.Equality
    ```
:::
:::

::: {.ghc-flag shortdesc="Print explicit kind foralls and kind arguments in types.
See also :extension:`KindSignatures`" type="dynamic" reverse="-fno-print-explicit-kinds" category="verbosity"}
-fprint-explicit-kinds

Using `-fprint-explicit-kinds`{.interpreted-text role="ghc-flag"} makes
GHC print kind arguments in types, which are normally suppressed. This
can be important when you are using kind polymorphism. For example:

``` {.sourceCode .none}
ghci> :set -XPolyKinds
ghci> data T a (b :: l) = MkT
ghci> :t MkT
MkT :: forall k l (a :: k) (b :: l). T a b
ghci> :set -fprint-explicit-kinds
ghci> :t MkT
MkT :: forall k l (a :: k) (b :: l). T @{k} @l a b
ghci> :set -XNoPolyKinds
ghci> :t MkT
MkT :: T @{*} @* a b
```

In the output above, observe that `T` has two kind variables (`k` and
`l`) and two type variables (`a` and `b`). Note that `k` is an
*inferred* variable and `l` is a *specified* variable (see
`inferred-vs-specified`{.interpreted-text role="ref"}), so as a result,
they are displayed using slightly different syntax in the type
`T @{k} @l a b`. The application of `l` (with `@l`) is the standard
syntax for visible type application (see
`visible-type-application`{.interpreted-text role="ref"}). The
application of `k` (with `@{k}`), however, uses a hypothetical syntax
for visible type application of inferred type variables. This syntax is
not currently exposed to the programmer, but it is nevertheless
displayed when `-fprint-explicit-kinds`{.interpreted-text
role="ghc-flag"} is enabled.
:::

::: {.ghc-flag shortdesc="Print coercions in types" type="dynamic" reverse="-fno-print-explicit-coercions" category="verbosity"}
-fprint-explicit-coercions

Using `-fprint-explicit-coercions`{.interpreted-text role="ghc-flag"}
makes GHC print coercions in types. When trying to prove the equality
between types of different kinds, GHC uses type-level coercions. Users
will rarely need to see these, as they are meant to be internal.
:::

::: {.ghc-flag shortdesc="Display equation incompatibilities in closed type families" type="dynamic" reverse="-fno-print-axiom-incomps" category="verbosity"}
-fprint-axiom-incomps

Using `-fprint-axiom-incomps`{.interpreted-text role="ghc-flag"} tells
GHC to display incompatibilities between closed type families\'
equations, whenever they are printed by `:info`{.interpreted-text
role="ghci-cmd"} or `--show-iface ⟨file⟩`{.interpreted-text
role="ghc-flag"}.

``` {.sourceCode .none}
ghci> :i Data.Type.Equality.==
type family (==) (a :: k) (b :: k) :: Bool
  where
      (==) (f a) (g b) = (f == g) && (a == b)
      (==) a a = 'True
      (==) _1 _2 = 'False
ghci> :set -fprint-axiom-incomps
ghci> :i Data.Type.Equality.==
type family (==) (a :: k) (b :: k) :: Bool
  where
      {- #0 -} (==) (f a) (g b) = (f == g) && (a == b)
      {- #1 -} (==) a a = 'True
          -- incompatible with: #0
      {- #2 -} (==) _1 _2 = 'False
          -- incompatible with: #1, #0
```

The equations are numbered starting from 0, and the comment after each
equation refers to all preceding equations it is incompatible with.
:::

::: {.ghc-flag shortdesc="Distinguish between equality relations when printing" type="dynamic" reverse="-fno-print-equality-relations" category="verbosity"}
-fprint-equality-relations

Using `-fprint-equality-relations`{.interpreted-text role="ghc-flag"}
tells GHC to distinguish between its equality relations when printing.
For example, `~` is homogeneous lifted equality (the kinds of its
arguments are the same) while `~~` is heterogeneous lifted equality (the
kinds of its arguments might be different) and `~#` is heterogeneous
unlifted equality, the internal equality relation used in GHC\'s solver.
Generally, users should not need to worry about the subtleties here; `~`
is probably what you want. Without
`-fprint-equality-relations`{.interpreted-text role="ghc-flag"}, GHC
prints all of these as `~`. See also
`equality-constraints`{.interpreted-text role="ref"}.
:::

::: {.ghc-flag shortdesc="In type errors, also print type-synonym-expanded types." type="dynamic" reverse="-fno-print-expanded-synonyms" category="verbosity"}
-fprint-expanded-synonyms

When enabled, GHC also prints type-synonym-expanded types in type
errors. For example, with this type synonyms: :

    type Foo = Int
    type Bar = Bool
    type MyBarST s = ST s Bar

This error message:

``` {.sourceCode .none}
Couldn't match type 'Int' with 'Bool'
Expected type: ST s Foo
  Actual type: MyBarST s
```

Becomes this:

``` {.sourceCode .none}
Couldn't match type 'Int' with 'Bool'
Expected type: ST s Foo
  Actual type: MyBarST s
Type synonyms expanded:
Expected type: ST s Int
  Actual type: ST s Bool
```
:::

::: {.ghc-flag shortdesc="Print extra information from typechecker." type="dynamic" reverse="-fno-print-typechecker-elaboration" category="verbosity"}
-fprint-typechecker-elaboration

When enabled, GHC also prints extra information from the typechecker in
warnings. For example: :

    main :: IO ()
    main = do
      return $ let a = "hello" in a
      return ()

This warning message:

``` {.sourceCode .none}
A do-notation statement discarded a result of type ‘[Char]’
Suppress this warning by saying
  ‘_ <- ($) return let a = "hello" in a’
or by using the flag -fno-warn-unused-do-bind
```

Becomes this:

``` {.sourceCode .none}
A do-notation statement discarded a result of type ‘[Char]’
Suppress this warning by saying
  ‘_ <- ($)
          return
          let
            AbsBinds [] []
              {Exports: [a <= a
                           <>]
               Exported types: a :: [Char]
                               [LclId, Str=DmdType]
               Binds: a = "hello"}
          in a’
or by using the flag -fno-warn-unused-do-bind
```
:::

::: {.ghc-flag shortdesc="Defer and group diagnostic messages by severity" type="dynamic" category="verbosity"}
-fdefer-diagnostics

Causes GHC to group diagnostic messages by severity and output them
after other messages when building a multi-module Haskell program. This
flag can make diagnostic messages more visible when used in conjunction
with `--make`{.interpreted-text role="ghc-flag"} and
`-j[⟨n⟩]`{.interpreted-text role="ghc-flag"}. Otherwise, it can be hard
to find the relevant errors or likely to ignore the warnings when they
are mixed with many other messages.
:::

::: {.ghc-flag shortdesc="Use colors in error messages" type="dynamic" category="verbosity"}
-fdiagnostics-color=⟨alwaysnever⟩

Causes GHC to display error messages with colors. To do this, the
terminal must have support for ANSI color codes, or else garbled text
will appear. The default value is `auto`, which means GHC will make an
attempt to detect whether terminal supports colors and choose
accordingly.

The precise color scheme is controlled by the environment variable
`GHC_COLORS` (or `GHC_COLOURS`). This can be set to colon-separated list
of `key=value` pairs. These are the default settings:

``` {.sourceCode .none}
header=:message=1:warning=1;35:error=1;31:fatal=1;31:margin=1;34
```

Each value is expected to be a [Select Graphic Rendition (SGR)
substring](https://en.wikipedia.org/wiki/ANSI_escape_code#graphics). The
formatting of each element can inherit from parent elements. For
example, if `header` is left empty, it will inherit the formatting of
`message`. Alternatively if `header` is set to `1` (bold), it will be
bolded but still inherits the color of `message`.

Currently, in the primary message, the following inheritance tree is in
place:

-   `message`
    -   `header`
        -   `warning`
        -   `error`
        -   `fatal`

In the caret diagnostics, there is currently no inheritance at all
between `margin`, `warning`, `error`, and `fatal`.

The environment variable can also be set to the magical values `never`
or `always`, which is equivalent to setting the corresponding
`-fdiagnostics-color` flag but with lower precedence.
:::

::: {.ghc-flag shortdesc="Whether to show snippets of original source code" type="dynamic" reverse="-fno-diagnostics-show-caret" category="verbosity"}
-fdiagnostics-show-caret

Controls whether GHC displays a line of the original source code where
the error was detected. This also affects the associated caret symbol
that points at the region of code at fault. The flag is on by default.
:::

::: {.ghc-flag shortdesc="Output full span in error messages" type="dynamic" category="verbosity"}
-ferror-spans

Causes GHC to emit the full source span of the syntactic entity relating
to an error message. Normally, GHC emits the source location of the
start of the syntactic entity only.

For example:

``` {.sourceCode .none}
test.hs:3:6: parse error on input `where'
```

becomes:

``` {.sourceCode .none}
test296.hs:3:6-10: parse error on input `where'
```

And multi-line spans are possible too:

``` {.sourceCode .none}
test.hs:(5,4)-(6,7):
    Conflicting definitions for `a'
    Bound at: test.hs:5:4
              test.hs:6:7
    In the binding group for: a, b, a
```

Note that line numbers start counting at one, but column numbers start
at zero. This choice was made to follow existing convention (i.e. this
is how Emacs does it).
:::

::: {.ghc-flag shortdesc="Continue compilation as far as possible on errors" type="dynamic" category="verbosity"}
-fkeep-going

since

:   8.10.1

Causes GHC to continue the compilation if a module has an error. Any
reverse dependencies are pruned immediately and the whole compilation is
still flagged as an error. This option has no effect if parallel
compilation (`-j[⟨n⟩]`{.interpreted-text role="ghc-flag"}) is in use.
:::

::: {.ghc-flag shortdesc="Output errors in reverse order" type="dynamic" reverse="-fno-reverse-errors" category="verbosity"}
-freverse-errors

Causes GHC to output errors in reverse line-number order, so that the
errors and warnings that originate later in the file are displayed
first.
:::

::: {.ghc-flag shortdesc="Set the minimum size of the heap to ⟨size⟩" type="dynamic" category="misc"}
-H ⟨size⟩

Set the minimum size of the heap to ⟨size⟩. This option is equivalent to
`+RTS -Hsize`, see `rts-options-gc`{.interpreted-text role="ref"}.
:::

::: {.ghc-flag shortdesc="Summarise timing stats for GHC (same as ``+RTS -tstderr``)." type="dynamic" category="verbosity"}
-Rghc-timing

Prints a one-line summary of timing statistics for the GHC run. This
option is equivalent to `+RTS -tstderr`, see
`rts-options-gc`{.interpreted-text role="ref"}.
:::

Platform-specific Flags {#options-platform}
-----------------------

::: {.index}
single: -m\* options single: platform-specific options single:
machine-specific options
:::

Some flags only make sense for particular target platforms.

::: {.ghc-flag shortdesc="(x86 only) Use SSE2 for floating-point operations" type="dynamic" category="platform-options"}
-msse2

(x86 only, added in GHC 7.0.1) Use the SSE2 registers and instruction
set to implement floating point operations when using the
`native code generator <native-code-gen>`{.interpreted-text role="ref"}.
This gives a substantial performance improvement for floating point, but
the resulting compiled code will only run on processors that support
SSE2 (Intel Pentium 4 and later, or AMD Athlon 64 and later). The
`LLVM backend <llvm-code-gen>`{.interpreted-text role="ref"} will also
use SSE2 if your processor supports it but detects this automatically so
no flag is required.

SSE2 is unconditionally used on x86-64 platforms.
:::

::: {.ghc-flag shortdesc="(x86 only) Use SSE4.2 for floating-point operations" type="dynamic" category="platform-options"}
-msse4.2

(x86 only, added in GHC 7.4.1) Use the SSE4.2 instruction set to
implement some floating point and bit operations when using the
`native code generator <native-code-gen>`{.interpreted-text role="ref"}.
The resulting compiled code will only run on processors that support
SSE4.2 (Intel Core i7 and later). The
`LLVM backend <llvm-code-gen>`{.interpreted-text role="ref"} will also
use SSE4.2 if your processor supports it but detects this automatically
so no flag is required.
:::

::: {.ghc-flag shortdesc="(x86 only) Use BMI2 for bit manipulation operations" type="dynamic" category="platform-options"}
-mbmi2

(x86 only, added in GHC 7.4.1) Use the BMI2 instruction set to implement
some bit operations when using the
`native code generator <native-code-gen>`{.interpreted-text role="ref"}.
The resulting compiled code will only run on processors that support
BMI2 (Intel Haswell and newer, AMD Excavator, Zen and newer).
:::

Haddock
-------

::: {.index}
single: haddock
:::

::: {.ghc-flag shortdesc="With this flag GHC will parse Haddock comments and include them
in the interface file it produces." type="dynamic" reverse="-no-haddock" category="haddock"}
-haddock

By default, GHC ignores Haddock comments (`-- | ...` and `-- ^ ...`) and
does not check that they\'re associated with a valid term, such as a
top-level type-signature. With this flag GHC will parse Haddock comments
and include them in the interface file it produces.

Note that this flag makes GHC\'s parser more strict so programs which
are accepted without Haddock may be rejected with
`-haddock`{.interpreted-text role="ghc-flag"}.
:::

Miscellaneous flags
-------------------

::: {.index}
single: miscellaneous flags
:::

Some flags only make sense for a particular use case.

::: {.ghc-flag shortdesc="(GHC as a C compiler only) Use this ``ghcversion.h`` file" type="dynamic" category="misc"}
-ghcversion-file ⟨path to ghcversion.h⟩

When GHC is used to compile C files, GHC adds package include paths and
includes `ghcversion.h` directly. The compiler will lookup the path for
the `ghcversion.h` file from the `rts` package in the package database.
In some cases, the compiler\'s package database does not contain the
`rts` package, or one wants to specify a specific `ghcversions.h` to be
included. This option can be used to specify the path to the
`ghcversions.h` file to be included. This is primarily intended to be
used by GHC\'s build system.
:::

### Other environment variables

::: {.index}
single: environment variables
:::

GHC can also be configured using environment variables. Currently the
only variable it supports is `GHC_NO_UNICODE`, which, when set, disables
Unicode output regardless of locale settings. `GHC_NO_UNICODE` can be
set to anything +(event an empty string) to trigger this behaviour.

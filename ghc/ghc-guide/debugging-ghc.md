Debugging the compiler {#options-debugging}
======================

::: {.index}
single: debugging options (for GHC)
:::

HACKER TERRITORY. HACKER TERRITORY. (You were warned.)

::: {.contents}
Dump flags
:::

Dumping out compiler intermediate structures {#dumping-output}
--------------------------------------------

::: {.index}
single: dumping GHC intermediates single: intermediate passes, output
:::

::: {.ghc-flag shortdesc="Dump to files instead of stdout" type="dynamic"}
-ddump-to-file

Causes the output from all of the flags listed below to be dumped to a
file. The file name depends upon the output produced; for instance,
output from `-ddump-simpl`{.interpreted-text role="ghc-flag"} will end
up in `{module}.dump-simpl`{.interpreted-text role="file"}.
:::

::: {.ghc-flag shortdesc="Set the prefix of the filenames used for debugging output." type="dynamic"}
-ddump-file-prefix=⟨str⟩

Set the prefix of the filenames used for debugging output. For example,
`-ddump-file-prefix=Foo` will cause the output from
`-ddump-simpl`{.interpreted-text role="ghc-flag"} to be dumped to
`Foo.dump-simpl`{.interpreted-text role="file"}.
:::

::: {.ghc-flag shortdesc="Dump error messages as JSON documents" type="dynamic"}
-ddump-json

Dump error messages as JSON documents. This is intended to be consumed
by external tooling. A good way to use it is in conjunction with
`-ddump-to-file`{.interpreted-text role="ghc-flag"}.
:::

::: {.ghc-flag shortdesc="Print out each pass name as it happens" type="dynamic"}
-dshow-passes

Print out each pass name, its runtime and heap allocations as it
happens. Note that this may come at a slight performance cost as the
compiler will be a bit more eager in forcing pass results to more
accurately account for their costs.

Two types of messages are produced: Those beginning with `***` do denote
the beginning of a compilation phase whereas those starting with `!!!`
mark the end of a pass and are accompanied by allocation and runtime
statistics.
:::

::: {.ghc-flag shortdesc="Show statistics for fast string usage when finished" type="dynamic"}
-dfaststring-stats

Show statistics on the usage of fast strings by the compiler.
:::

::: {.ghc-flag shortdesc="Turn on debug printing (more verbose)" type="dynamic"}
-dppr-debug

Debugging output is in one of several \"styles.\" Take the printing of
types, for example. In the \"user\" style (the default), the compiler\'s
internal ideas about types are presented in Haskell source-level syntax,
insofar as possible. In the \"debug\" style (which is the default for
debugging output), the types are printed in with explicit foralls, and
variables have their unique-id attached (so you can check for things
that look the same but aren\'t). This flag makes debugging output appear
in the more verbose debug style.
:::

::: {.ghc-flag shortdesc="Dump per-pass timing and allocation statistics" type="dynamic"}
-ddump-timings

Show allocation and runtime statistics for various stages of
compilation. Allocations are measured in bytes. Timings are measured in
milliseconds.
:::

GHC is a large program consisting of a number of stages. You can tell
GHC to dump information from various stages of compilation using the
`-ddump-⟨pass⟩` flags listed below. Note that some of these tend to
produce a lot of output. You can prevent them from clogging up your
standard output by passing `-ddump-to-file`{.interpreted-text
role="ghc-flag"}.

### Front-end

These flags dump various information from GHC\'s frontend. This includes
the parser and interface file reader.

::: {.ghc-flag shortdesc="Dump parse tree" type="dynamic"}
-ddump-parsed

Dump parser output
:::

::: {.ghc-flag shortdesc="Dump parser output as a syntax tree" type="dynamic"}
-ddump-parsed-ast

Dump parser output as a syntax tree
:::

::: {.ghc-flag shortdesc="Trace interface files" type="dynamic"}
-ddump-if-trace

Make the interface loader be *real* chatty about what it is up to.
:::

### Type-checking and renaming

These flags dump various information from GHC\'s typechecker and
renamer.

::: {.ghc-flag shortdesc="Trace typechecker" type="dynamic"}
-ddump-tc-trace

Make the type checker be *real* chatty about what it is up to.
:::

::: {.ghc-flag shortdesc="Trace renamer" type="dynamic"}
-ddump-rn-trace

Make the renamer be *real* chatty about what it is up to.
:::

::: {.ghc-flag shortdesc="Trace exhaustiveness checker" type="dynamic"}
-ddump-ec-trace

Make the pattern match exhaustiveness checker be *real* chatty about
what it is up to.
:::

::: {.ghc-flag shortdesc="Renamer stats" type="dynamic"}
-ddump-rn-stats

Print out summary of what kind of information the renamer had to bring
in.
:::

::: {.ghc-flag shortdesc="Dump renamer output" type="dynamic"}
-ddump-rn

Dump renamer output
:::

::: {.ghc-flag shortdesc="Dump renamer output as a syntax tree" type="dynamic"}
-ddump-rn-ast

Dump renamer output as a syntax tree
:::

::: {.ghc-flag shortdesc="Dump typechecker output" type="dynamic"}
-ddump-tc

Dump typechecker output. Note that this hides a great deal of detail by
default; you might consider using this with
`-fprint-typechecker-elaboration`{.interpreted-text role="ghc-flag"}.
:::

::: {.ghc-flag shortdesc="Dump typechecker output as a syntax tree" type="dynamic"}
-ddump-tc-ast

Dump typechecker output as a syntax tree
:::

::: {.ghc-flag shortdesc="Dump the hie file syntax tree" type="dynamic"}
-ddump-hie

Dump the hie file syntax tree if we are generating extended interface
files
:::

::: {.ghc-flag shortdesc="Dump TH spliced expressions, and what they evaluate to" type="dynamic"}
-ddump-splices

Dump Template Haskell expressions that we splice in, and what Haskell
code the expression evaluates to.
:::

::: {.ghc-flag shortdesc="Dump evaluated TH declarations into `*.th.hs` files" type="dynamic"}
-dth-dec-file

Dump expansions of all top-level Template Haskell splices into
`{module}.th.hs`{.interpreted-text role="file"} for each file
`{module}.hs`{.interpreted-text role="file"}.
:::

::: {.ghc-flag shortdesc="Dump type signatures" type="dynamic"}
-ddump-types

Dump a type signature for each value defined at the top level of the
module. The list is sorted alphabetically. Using
`-dppr-debug`{.interpreted-text role="ghc-flag"} dumps a type signature
for all the imported and system-defined things as well; useful for
debugging the compiler.
:::

::: {.ghc-flag shortdesc="Dump deriving output" type="dynamic"}
-ddump-deriv

Dump derived instances
:::

### Core representation and simplification

These flags dump various phases of GHC\'s Core-to-Core pipeline. This
begins with the desugarer and includes the simplifier, worker-wrapper
transformation, the rule engine, the specialiser, the
strictness/occurrence analyser, and a common subexpression elimination
pass.

::: {.ghc-flag shortdesc="Print a one-line summary of the size of the Core program at the
end of the optimisation pipeline" type="dynamic"}
-ddump-core-stats

Print a one-line summary of the size of the Core program at the end of
the optimisation pipeline.
:::

::: {.ghc-flag shortdesc="Dump desugarer output." type="dynamic"}
-ddump-ds -ddump-ds-preopt

Dump desugarer output. `-ddump-ds`{.interpreted-text role="ghc-flag"}
dumps the output after the very simple optimiser has run (which discards
a lot of clutter and hence is a sensible default.
`-ddump-ds-preopt`{.interpreted-text role="ghc-flag"} shows the output
after desugaring but before the very simple optimiser.
:::

::: {.ghc-flag shortdesc="Dump output from each simplifier iteration" type="dynamic"}
-ddump-simpl-iterations

Show the output of each *iteration* of the simplifier (each run of the
simplifier has a maximum number of iterations, normally 4).
:::

::: {.ghc-flag shortdesc="Dump simplifier stats" type="dynamic"}
-ddump-simpl-stats

Dump statistics about how many of each kind of transformation took
place. If you add `-dppr-debug`{.interpreted-text role="ghc-flag"} you
get more detailed information.
:::

::: {.ghc-flag shortdesc="Show output from each core-to-core pass" type="dynamic"}
-dverbose-core2core

Show the output of the intermediate Core-to-Core pass. (*lots* of
output!) So: when we\'re really desperate:

``` {.sourceCode .sh}
% ghc -noC -O -ddump-simpl -dverbose-core2core -dcore-lint Foo.hs
```
:::

::: {.ghc-flag shortdesc="Dump specialiser output" type="dynamic"}
-ddump-spec

Dump output of specialisation pass
:::

::: {.ghc-flag shortdesc="Dump rewrite rules" type="dynamic"}
-ddump-rules

Dumps all rewrite rules specified in this module; see
`controlling-rules`{.interpreted-text role="ref"}.
:::

::: {.ghc-flag shortdesc="Dump rule firing info" type="dynamic"}
-ddump-rule-firings

Dumps the names of all rules that fired in this module
:::

::: {.ghc-flag shortdesc="Dump detailed rule firing info" type="dynamic"}
-ddump-rule-rewrites

Dumps detailed information about all rules that fired in this module
:::

::: {.ghc-flag shortdesc="Dump information about potential rule application" type="dynamic"}
-drule-check=⟨str⟩

This flag is useful for debugging why a rule you expect to be firing
isn\'t.

Rules are filtered by the user provided string, a rule is kept if a
prefix of its name matches the string. The pass then checks whether any
of these rules could apply to the program but which didn\'t fire for
some reason. For example, specifying `-drule-check=SPEC` will check
whether there are any applications which might be subject to a rule
created by specialisation.
:::

::: {.ghc-flag shortdesc="Dump information about inlining decisions" type="dynamic"}
-dinline-check=⟨str⟩

This flag is useful for debugging why a definition is not inlined.

When a string is passed to this flag we report information about all
functions whose name shares a prefix with the string.

For example, if you are inspecting the core of your program and you
observe that `foo` is not being inlined. You can pass
`-dinline-check foo` and you will see a report about why `foo` is not
inlined.
:::

::: {.ghc-flag shortdesc="Dump final simplifier output" type="dynamic"}
-ddump-simpl

Dump simplifier output (Core-to-Core passes)
:::

::: {.ghc-flag shortdesc="Dump inlining info" type="dynamic"}
-ddump-inlinings

Dumps inlining info from the simplifier. Note that if used in
conjunction with `-dverbose-core2core`{.interpreted-text
role="ghc-flag"} the compiler will also dump the inlinings that it
considers but passes up, along with its rationale.
:::

::: {.ghc-flag shortdesc="Dump strictness analyser output" type="dynamic"}
-ddump-stranal

Dump strictness analyser output
:::

::: {.ghc-flag shortdesc="Dump strictness signatures" type="dynamic"}
-ddump-str-signatures

Dump strictness signatures
:::

::: {.ghc-flag shortdesc="Dump CPR analysis output" type="dynamic"}
-ddump-cpranal

Dump Constructed Product Result analysis output
:::

::: {.ghc-flag shortdesc="Dump CPR signatures" type="dynamic"}
-ddump-cpr-signatures

Dump Constructed Product Result signatures
:::

::: {.ghc-flag shortdesc="Dump CSE output" type="dynamic"}
-ddump-cse

Dump common subexpression elimination (CSE) pass output
:::

::: {.ghc-flag shortdesc="Dump worker-wrapper output" type="dynamic"}
-ddump-worker-wrapper

Dump worker/wrapper split output
:::

::: {.ghc-flag shortdesc="Dump occurrence analysis output" type="dynamic"}
-ddump-occur-anal

Dump \"occurrence analysis\" output
:::

::: {.ghc-flag shortdesc="Dump prepared core" type="dynamic"}
-ddump-prep

Dump output of Core preparation pass
:::

### STG representation

These flags dump various phases of GHC\'s STG pipeline.

::: {.ghc-flag shortdesc="Show CoreToStg output" type="dynamic"}
-ddump-stg

Show the output of CoreToStg pass.
:::

::: {.ghc-flag shortdesc="Show output from each STG-to-STG pass" type="dynamic"}
-dverbose-stg2stg

Show the output of the intermediate STG-to-STG pass. (*lots* of output!)
:::

::: {.ghc-flag shortdesc="Show unarised STG" type="dynamic"}
-ddump-stg-unarised

Show the output of the unarise pass.
:::

::: {.ghc-flag shortdesc="Show output of last STG pass." type="dynamic"}
-ddump-stg-final

Show the output of the last STG pass before we generate Cmm.
:::

### C-\\- representation

These flags dump various phases of GHC\'s C-\\- pipeline.

::: {.ghc-flag shortdesc="Show output from main C-\\- pipeline passes (grouped by proc)" type="dynamic"}
-ddump-cmm-verbose-by-proc

Dump output from main C-\\- pipeline stages. In case of `.cmm`
compilation this also dumps the result of file parsing. Not included are
passes run by the chosen backend. Currently only the NCG backends runs
additional passes ( `-ddump-opt-cmm`{.interpreted-text role="ghc-flag"}
).

Cmm dumps don\'t include unreachable blocks since we print blocks in
reverse post-order.
:::

::: {.ghc-flag shortdesc="Write output from main C-\\- pipeline passes to files" type="dynamic"}
-ddump-cmm-verbose

If used in conjunction with `-ddump-to-file`{.interpreted-text
role="ghc-flag"}, writes dump output from main C-\\- pipeline stages to
files (each stage per file).
:::

::: {.ghc-flag shortdesc="Dump STG-to-C-\\- output" type="dynamic"}
-ddump-cmm-from-stg

Dump the result of STG-to-C-\\- conversion
:::

::: {.ghc-flag shortdesc="Dump raw C-\\-" type="dynamic"}
-ddump-cmm-raw

Dump the "raw" C-\\-.
:::

::: {.ghc-flag shortdesc="Dump the results of the C-\\- control flow optimisation pass." type="dynamic"}
-ddump-cmm-cfg

Dump the results of the C-\\- control flow optimisation pass.
:::

::: {.ghc-flag shortdesc="Dump the results of common block elimination" type="dynamic"}
-ddump-cmm-cbe

Dump the results of the C-\\- Common Block Elimination (CBE) pass.
:::

::: {.ghc-flag shortdesc="Dump the results of switch lowering passes" type="dynamic"}
-ddump-cmm-switch

Dump the results of the C-\\- switch lowering pass.
:::

::: {.ghc-flag shortdesc="Dump the results of proc-point analysis" type="dynamic"}
-ddump-cmm-proc

Dump the results of the C-\\- proc-point analysis pass.
:::

::: {.ghc-flag shortdesc="Dump the results of the C-\\- stack layout pass." type="dynamic"}
-ddump-cmm-sp

Dump the results of the C-\\- stack layout pass.
:::

::: {.ghc-flag shortdesc="Dump the results of the C-\\- sinking pass." type="dynamic"}
-ddump-cmm-sink

Dump the results of the C-\\- sinking pass.
:::

::: {.ghc-flag shortdesc="Dump the results of the C-\\- CAF analysis pass." type="dynamic"}
-ddump-cmm-caf

Dump the results of the C-\\- CAF analysis pass.
:::

::: {.ghc-flag shortdesc="Dump the results of the C-\\- proc-point map pass." type="dynamic"}
-ddump-cmm-procmap

Dump the results of the C-\\- proc-point map pass.
:::

::: {.ghc-flag shortdesc="Dump the results of the C-\\- proc-point splitting pass." type="dynamic"}
-ddump-cmm-split

Dump the results of the C-\\- proc-point splitting pass.
:::

::: {.ghc-flag shortdesc="Dump the results of the C-\\- info table augmentation pass." type="dynamic"}
-ddump-cmm-info

Dump the results of the C-\\- info table augmentation pass.
:::

::: {.ghc-flag shortdesc="Dump the results of the CPS pass" type="dynamic"}
-ddump-cmm-cps

Dump the results of the CPS pass.
:::

::: {.ghc-flag shortdesc="Dump the final C-\\- output" type="dynamic"}
-ddump-cmm

Dump the result of the C-\\- pipeline processing
:::

::: {.ghc-flag shortdesc="Dump the assumed weights of the CFG." type="dynamic"}
-ddump-cfg-weights

Dumps the CFG with weights used by the new block layout code. Each CFG
is dumped in dot format graph making it easy to visualize them.
:::

### LLVM code generator

::: {.ghc-flag shortdesc="Dump LLVM intermediate code." type="dynamic"}
-ddump-llvm

implies

:   `-fllvm`{.interpreted-text role="ghc-flag"}

LLVM code from the
`LLVM code generator <llvm-code-gen>`{.interpreted-text role="ref"}
:::

### Native code generator

These flags dump various stages of the `native code generator's
<native-code-gen>`{.interpreted-text role="ref"} pipeline, which starts
with C-\\- and produces native assembler.

::: {.ghc-flag shortdesc="Dump the results of C-\\- to C-\\- optimising passes" type="dynamic"}
-ddump-cmm-opt

Dump the results of C-\\- to C-\\- optimising passes performed by the
NCG.
:::

::: {.ghc-flag shortdesc="Dump the results of C-\\- to C-\\- optimising passes" type="dynamic"}
-ddump-opt-cmm

Alias for `-ddump-cmm-opt`{.interpreted-text role="ghc-flag"}
:::

::: {.ghc-flag shortdesc="Dump initial assembly" type="dynamic"}
-ddump-asm-native

Dump the initial assembler output produced from C-\\-.
:::

::: {.ghc-flag shortdesc="Dump assembly augmented with register liveness" type="dynamic"}
-ddump-asm-liveness

Dump the result of the register liveness pass.
:::

::: {.ghc-flag shortdesc="Dump the result of register allocation" type="dynamic"}
-ddump-asm-regalloc

Dump the result of the register allocation pass.
:::

::: {.ghc-flag shortdesc="Dump the build/spill stages of the :ghc-flag:`-fregs-graph`
register allocator." type="dynamic"}
-ddump-asm-regalloc-stages

Dump the build/spill stages of the `-fregs-graph`{.interpreted-text
role="ghc-flag"} register allocator.
:::

::: {.ghc-flag shortdesc="Dump statistics from the register allocator." type="dynamic"}
-ddump-asm-stats

Dump statistics from the register allocator.
:::

::: {.ghc-flag shortdesc="Dump the result of the synthetic instruction expansion pass." type="dynamic"}
-ddump-asm-expanded

Dump the result of the synthetic instruction expansion pass.
:::

::: {.ghc-flag shortdesc="Dump final assembly" type="dynamic"}
-ddump-asm

Dump the final assembly produced by the native code generator.
:::

### Miscellaneous backend dumps

These flags dump various bits of information from other backends.

::: {.ghc-flag shortdesc="Dump interpreter byte code" type="dynamic"}
-ddump-bcos

Dump byte-code objects (BCOs) produced for the GHC\'s byte-code
interpreter.
:::

::: {.ghc-flag shortdesc="Trace runtime type inference" type="dynamic"}
-ddump-rtti

Trace runtime type inference done by various interpreter commands.
:::

::: {.ghc-flag shortdesc="Dump ``foreign export`` stubs" type="dynamic"}
-ddump-foreign

Dump foreign export stubs.
:::

::: {.ghc-flag shortdesc="Dump the code instrumented by HPC (:ref:`hpc`)." type="dynamic"}
-ddump-ticked

Dump the code instrumented by HPC (`hpc`{.interpreted-text role="ref"}).
:::

::: {.ghc-flag shortdesc="An alias for :ghc-flag:`-ddump-ticked`." type="dynamic"}
-ddump-hpc

An alias for `-ddump-ticked`{.interpreted-text role="ghc-flag"}.
:::

::: {.ghc-flag shortdesc="Dump the state of the module mapping database." type="dynamic"}
-ddump-mod-map

Dump a mapping of modules to where they come from, and how:

-   `(hidden module)`: Module is hidden, and thus will never be
    available for import.
-   `(unusable module)`: Module is unavailable because the package is
    unusable.
-   `(hidden package)`: This module is in someone\'s exported-modules
    list, but that package is hidden.
-   `(exposed package)`: Module is available for import.
-   `(reexport by <PACKAGES>)`: This module is available from a reexport
    of some set of exposed packages.
-   `(hidden reexport by <PACKAGES>)`: This module is available from a
    reexport of some set of hidden packages.
-   `(package flag)`: This module export comes from a package flag.
:::

Formatting dumps {#formatting dumps}
----------------

::: {.index}
single: formatting dumps
:::

::: {.ghc-flag shortdesc="Set the depth for printing expressions in error msgs" type="dynamic"}
-dppr-user-length

In error messages, expressions are printed to a certain \"depth\", with
subexpressions beyond the depth replaced by ellipses. This flag sets the
depth. Its default value is 5.
:::

::: {.ghc-flag shortdesc="Set the width of debugging output. For example ``-dppr-cols200``" type="dynamic"}
-dppr-cols=⟨n⟩

Set the width of debugging output. Use this if your code is wrapping too
much. For example: `-dppr-cols=200`.
:::

::: {.ghc-flag shortdesc="Print single alternative case expressions as strict lets." type="dynamic"}
-dppr-case-as-let

Print single alternative case expressions as though they were strict let
expressions. This is helpful when your code does a lot of unboxing.
:::

::: {.ghc-flag shortdesc="Print values of type `Word#` in hexadecimal." type="dynamic"}
-dhex-word-literals

Print values of type [Word\#]{.title-ref} and [Word64\#]{.title-ref}
(but not values of type [Int\#]{.title-ref} and [Int64\#]{.title-ref})
in hexadecimal instead of decimal. The hexadecimal is zero-padded to
make the length of the representation a power of two. For example:
[0x0A0A\#\#]{.title-ref}, [0x000FFFFF\#\#]{.title-ref},
[0xC\#\#]{.title-ref}. This flag may be helpful when you are producing a
bit pattern that to expect to work correctly on a 32-bit or a 64-bit
architecture. Dumping hexadecimal literals after optimizations and
constant folding makes it easier to confirm that the generated bit
pattern is correct.
:::

::: {.ghc-flag shortdesc="Suppress unsolicited debugging output" type="dynamic" reverse="-ddebug-output"}
-dno-debug-output

Suppress any unsolicited debugging output. When GHC has been built with
the `DEBUG` option it occasionally emits debug output of interest to
developers. The extra output can confuse the testing framework and cause
bogus test failures, so this flag is provided to turn it off.
:::

Suppressing unwanted information {#suppression}
--------------------------------

::: {.index}
single: suppression; of unwanted dump output
:::

Core dumps contain a large amount of information. Depending on what you
are doing, not all of it will be useful. Use these flags to suppress the
parts that you are not interested in.

::: {.ghc-flag shortdesc="In dumps, suppress everything (except for uniques) that is
suppressible." type="dynamic"}
-dsuppress-all

Suppress everything that can be suppressed, except for unique ids as
this often makes the printout ambiguous. If you just want to see the
overall structure of the code, then start here.
:::

::: {.ghc-flag shortdesc="Suppress "ticks" in the pretty-printer output." type="dynamic"}
-dsuppress-ticks

Suppress \"ticks\" in the pretty-printer output.
:::

::: {.ghc-flag shortdesc="Suppress the printing of uniques in debug output (easier to use
``diff``)" type="dynamic"}
-dsuppress-uniques

Suppress the printing of uniques. This may make the printout ambiguous
(e.g. unclear where an occurrence of \'x\' is bound), but it makes the
output of two compiler runs have many fewer gratuitous differences, so
you can realistically apply `diff`. Once `diff` has shown you where to
look, you can try again without `-dsuppress-uniques`{.interpreted-text
role="ghc-flag"}
:::

::: {.ghc-flag shortdesc="Suppress extended information about identifiers where they
are bound" type="dynamic"}
-dsuppress-idinfo

Suppress extended information about identifiers where they are bound.
This includes strictness information and inliner templates. Using this
flag can cut the size of the core dump in half, due to the lack of
inliner templates
:::

::: {.ghc-flag shortdesc="Suppress the printing of the stable unfolding of a variable at
its binding site" type="dynamic"}
-dsuppress-unfoldings

Suppress the printing of the stable unfolding of a variable at its
binding site.
:::

::: {.ghc-flag shortdesc="Suppress the printing of module qualification prefixes" type="dynamic"}
-dsuppress-module-prefixes

Suppress the printing of module qualification prefixes. This is the
`Data.List` in `Data.List.length`.
:::

::: {.ghc-flag shortdesc="Suppress timestamps in dumps" type="dynamic"}
-dsuppress-timestamps

Suppress the printing of timestamps. This makes it easier to diff dumps.
:::

::: {.ghc-flag shortdesc="Suppress type signatures" type="dynamic"}
-dsuppress-type-signatures

Suppress the printing of type signatures.
:::

::: {.ghc-flag shortdesc="Suppress type applications" type="dynamic"}
-dsuppress-type-applications

Suppress the printing of type applications.
:::

::: {.ghc-flag shortdesc="Suppress the printing of coercions in Core dumps to make them
shorter" type="dynamic"}
-dsuppress-coercions

Suppress the printing of type coercions.
:::

::: {.ghc-flag shortdesc="Suppress the printing of variable kinds" type="dynamic"}
-dsuppress-var-kinds

Suppress the printing of variable kinds
:::

::: {.ghc-flag shortdesc="Suppress the printing of closure free variable lists in STG output" type="dynamic"}
-dsuppress-stg-free-vars

Suppress the printing of closure free variable lists in STG output
:::

Checking for consistency {#checking-consistency}
------------------------

::: {.index}
single: consistency checks single: lint
:::

::: {.ghc-flag shortdesc="Turn on internal sanity checking" type="dynamic"}
-dcore-lint

Turn on heavyweight intra-pass sanity-checking within GHC, at Core
level. (It checks GHC\'s sanity, not yours.)
:::

::: {.ghc-flag shortdesc="Turn on internal sanity checking" type="dynamic"}
-dlinear-core-lint

Turn on linearity checking in GHC. Currently, some optimizations in GHC
might not preserve linearity and they valid programs might fail Linear
Core Lint. In the near future, this option will be removed and folded
into normal Core Lint.
:::

::: {.ghc-flag shortdesc="STG pass sanity checking" type="dynamic"}
-dstg-lint

Ditto for STG level.
:::

::: {.ghc-flag shortdesc="C-\\- pass sanity checking" type="dynamic"}
-dcmm-lint

Ditto for C-\\- level.
:::

::: {.ghc-flag shortdesc="Intruct LLVM to fill dead STG registers with garbage" type="dynamic"}
-fllvm-fill-undef-with-garbage

Instructs the LLVM code generator to fill dead STG registers with
garbage instead of `undef` in calls. This makes it easier to catch
subtle code generator and runtime system bugs (e.g. see
`11487`{.interpreted-text role="ghc-ticket"}).
:::

::: {.ghc-flag shortdesc="Compile with alignment checks for all info table dereferences." type="dynamic"}
-falignment-sanitisation

Compile with alignment checks for all info table dereferences. This can
be useful when finding pointer tagging issues.
:::

::: {.ghc-flag shortdesc="Align functions at given boundary." type="dynamic"}
-fproc-alignment

Align functions to multiples of the given value. Only valid values are
powers of two.

`-fproc-alignment=64` can be used to limit alignment impact on
performance as each function will start at a cache line. However forcing
larger alignments in general reduces performance.
:::

::: {.ghc-flag shortdesc="Add a default ``error`` alternative to case expressions without
a default alternative." type="dynamic"}
-fcatch-bottoms

GHC generates case expressions without a default alternative in some
cases:

-   When the demand analysis thinks that the scrutinee does not return
    (i.e. a bottoming expression)
-   When the scrutinee is a GADT and its type rules out some
    constructors, and others constructors are already handled by the
    case expression.

With this flag GHC generates a default alternative with `error` in these
cases. This is helpful when debugging demand analysis or type checker
bugs which can sometimes manifest as segmentation faults.
:::

Checking for determinism {#checking-determinism}
------------------------

::: {.index}
single: deterministic builds
:::

::: {.ghc-flag shortdesc="Start ``UniqSupply`` allocation from ⟨s⟩." type="dynamic"}
-dinitial-unique=⟨s⟩

Start `UniqSupply` allocation from ⟨s⟩.
:::

::: {.ghc-flag shortdesc="Set the increment for the generated ``Unique``'s to ⟨i⟩." type="dynamic"}
-dunique-increment=⟨i⟩

Set the increment for the generated `Unique`\'s to ⟨i⟩.

This is useful in combination with
`-dinitial-unique=⟨s⟩`{.interpreted-text role="ghc-flag"} to test if the
generated files depend on the order of `Unique`\'s.

Some interesting values:

-   `-dinitial-unique=0 -dunique-increment=1` - current sequential
    `UniqSupply`
-   `-dinitial-unique=16777215 -dunique-increment=-1` - `UniqSupply`
    that generates in decreasing order
-   `-dinitial-unique=1 -dunique-increment=PRIME` - where PRIME big
    enough to overflow often - nonsequential order
:::

Other
-----

::: {.ghc-flag shortdesc="Don't generate bindings for Typeable methods" type="dynamic"}
-dno-typeable-binds

This avoid generating Typeable-related bindings for modules and types.
This is useful when debugging because it gives smaller modules and
dumps, but the compiler will panic if you try to use Typeable instances
of things that you built with this flag.
:::

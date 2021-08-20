# The :set and :seti commands

308-set-and-seti-commands.md


The `:set`  command sets two types of options: GHCi options, which begin with "`+`", and "command-line" options, which begin with "`-`".

At the moment, the `:set`  command doesn't support any kind of quoting in its arguments: quotes will not be removed and cannot be used to group words together. For example, `:set -DFOO='BAR BAZ'` will not do what you expect.


### GHCi options

GHCi options may be set using `:set` 
and unset using `:unset` .

The available GHCi options are:

::: {.ghci-cmd}
:set +c

Collect type and location information after loading modules. The
commands `:all-types` ,
`:loc-at` ,
`:type-at` , and
`:uses`  require `+c` to be active.
:::

::: {.ghci-cmd}
:set +m

::: {.index}
single: multiline input; in GHCi
:::

Enable parsing of multiline commands. A multiline command is prompted
for when the current input line contains open layout contexts (see
`ghci-multiline`ref"}).
:::

::: {.ghci-cmd}
:set +r

::: {.index}
single: CAFs; in GHCi single: Constant Applicative Form
:::

Normally, any evaluation of top-level expressions (otherwise known as
CAFs or Constant Applicative Forms) in loaded modules is retained
between evaluations. Turning on `+r` causes all evaluation of top-level
expressions to be discarded after each evaluation (they are still
retained *during* a single evaluation).

This option may help if the evaluated top-level expressions are
consuming large amounts of space, or if you need repeatable performance
measurements.
:::

::: {.ghci-cmd}
:set +s

Display some stats after evaluating each expression, including the
elapsed time and number of bytes allocated. NOTE: the allocation figure
is only accurate to the size of the storage manager's allocation area,
because it is calculated at every GC. Hence, you might see values of
zero if no GC has occurred.
:::

::: {.ghci-cmd}
:set +t

::: {.index}
single: displaying type; in GHCi
:::

Display the type of each variable bound after a statement is entered at
the prompt. If the statement is a single expression, then the only
variable binding will be for the variable `it`.
:::

### Setting GHC command-line options in GHCi {#ghci-cmd-line-options}

Normal GHC command-line options may also be set using
`:set` . For example, to turn on
`-Wmissing-signatures` , you would
say:

```bash
ghci> :set -Wmissing-signatures
```

Any GHC command-line option that is designated as dynamic (see the table
in `flag-reference`ref"}), may be set using
`:set` . To unset an option, you can
set the reverse option:

::: {.index}
single: dynamic; options
:::

```bash
ghci> :set -Wno-incomplete-patterns -XNoMultiParamTypeClasses
```

`flag-reference`ref"} lists the reverse for
each option where applicable.

Certain static options (`-package ⟨pkg⟩` 
role=" , `-I⟨dir⟩` ,
`-i⟨dir⟩[:⟨dir⟩]*` , and
`-l ⟨lib⟩`  in particular) will also
work, but some may not take effect until the next reload.

::: {.index}
single: static; options
:::

### Setting options for interactive evaluation only {#ghci-interactive-options}

GHCi actually maintains *two* sets of options:

-   The *loading options* apply when loading modules
-   The *interactive options* apply when evaluating expressions and
    commands typed at the GHCi prompt.

The `:set`  command modifies both, but
there is also a `:seti`  command (for
"set interactive") that affects only the interactive options set.

It is often useful to change the interactive options, without having
that option apply to loaded modules too. For example

```bash
:seti -XMonoLocalBinds
```

It would be undesirable if `MonoLocalBinds` 
role="extension"} were to apply to loaded modules too: that might cause
a compilation error, but more commonly it will cause extra
recompilation, because GHC will think that it needs to recompile the
module because the flags have changed.

If you are setting language options in your `.ghci` file, it is good
practice to use `:seti`  rather than
`:set` , unless you really do want
them to apply to all modules you load in GHCi.

The two sets of options can be inspected using the
`:set`  and `:seti` 
  commands respectively, with no arguments. For example,
in a clean GHCi session we might see something like this:

```bash
ghci> :seti
base language is: Haskell2010
with the following modifiers:
  -XNoMonomorphismRestriction
  -XNoDatatypeContexts
  -XNondecreasingIndentation
  -XExtendedDefaultRules
GHCi-specific dynamic flag settings:
other dynamic, non-language, flag settings:
  -fimplicit-import-qualified
warning settings:
```

The two sets of options are initialised as follows. First, both sets of
options are initialised as described in
`ghci-dot-files`ref"}. Then the interactive
options are modified as follows:

-   The option `-XExtendedDefaultRules` is enabled, in order to apply
    special defaulting rules to expressions typed at the prompt (see
    `extended-default-rules`ref"}).
-   The Monomorphism Restriction is disabled (see
    `monomorphism`ref"}).

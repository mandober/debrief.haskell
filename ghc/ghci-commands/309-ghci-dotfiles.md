# 3.9. GHCi dotfiles

309-ghci-dotfiles.md



# The `.ghci` and `.haskeline` files


### The `.ghci` files {#dot-ghci-files}

::: {.index}
single: .ghci; file single: startup; files, GHCi
:::

When it starts, unless the `-ignore-dot-ghci` 
role="  flag is given, GHCi reads and executes commands from
the following files, in this order, if they exist:

1.  `{ghcappdata}/ghci.conf`file"}, where
    ⟨ghcappdata⟩ depends on your system, but is usually something like
    `$HOME/.ghc`file"} on Unix or
    `C:/Documents and Settings/user/Application
    Data/ghc`file"} on Windows.
2.  `$HOME/.ghci`file"}
3.  `./.ghci`file"}

The `ghci.conf`file"} file is most useful for
turning on favourite options (e.g. `:set +s`), and defining useful
macros.


When setting language options in this file it is usually desirable to
use `:seti`  rather than
`:set`  (see
`ghci-interactive-options`ref"}).
:::

Placing a `.ghci`file"} file in a directory
with a Haskell project is a useful way to set certain project-wide
options so you don't have to type them every time you start GHCi: eg.
if your project uses multi-parameter type classes, scoped type
variables, and CPP, and has source files in three subdirectories A, B
and C, you might put the following lines in `.ghci` 
role="file"}:

```bash
:set -XMultiParamTypeClasses -XScopedTypeVariables -cpp
:set -iA:B:C
```

(Note that strictly speaking the `-i` 
flag is a static one, but in fact it works to set it using
`:set`  like this. The changes won't
take effect until the next `:load` ,
though.)


Sourcing untrusted `./.ghci`file"} files is a
security risk. They can contain arbitrary commands that will be executed
as the user. Use `:set local-config` 
to inhibit the processing of `./.ghci`file"}
files.
:::

Once you have a library of GHCi macros, you may want to source them from
separate files, or you may want to source your `.ghci` file into your
running GHCi session while debugging it

```bash
:def source readFile
```

With this macro defined in your `.ghci` file, you can use `:source file`
to read GHCi commands from `file`. You can find (and contribute!-) other
suggestions for `.ghci` files on this Haskell wiki page:
[GHC/GHCi](http://haskell.org/haskellwiki/GHC/GHCi)

Additionally, any files specified with `-ghci-script` 
role="  flags will be read after the standard files, allowing
the use of custom .ghci files.

Two command-line options control whether the startup files files are
read:

::: {.ghc-flag shortdesc="Disable reading of `.ghci` files" type="dynamic" category=""}
-ignore-dot-ghci

Don't read either `./.ghci`file"} or the other
startup files when starting up.
:::

::: {.ghc-flag shortdesc="Read additional `.ghci` files" type="dynamic" category=""}
-ghci-script

Read a specific file after the usual startup files. May be specified
repeatedly for multiple inputs. `-ignore-dot-ghci` 
role="  does not apply to these files.
:::

When defining GHCi macros, there is some important behavior you should
be aware of when names may conflict with built-in commands, especially
regarding tab completion.

For example, consider if you had a macro named `:time` and in the shell,
typed `:t 3` --- what should happen? The current algorithm we use for
completing commands is:

1.  First, look up an exact match on the name from the defined macros.
2.  Look for the exact match on the name in the built-in command list.
3.  Do a prefix lookup on the list of built-in commands - if a built-in
    command matches, but a macro is defined with the same name as the
    built-in defined, pick the macro.
4.  Do a prefix lookup on the list of built-in commands.
5.  Do a prefix lookup on the list of defined macros.

Here are some examples:

1.  You have a macro `:time` and enter `:t 3`

    You get `:type 3`

2.  You have a macro `:type` and enter `:t 3`

    You get `:type 3` with your defined macro, not the builtin.

3.  You have a macro `:time` and a macro `:type`, and enter `:t 3`

    You get `:type 3` with your defined macro.

When giving priority to built-in commands, you can use
`:: ⟨builtin-command⟩` , like
`::type 3`.

### The `.haskeline` file {#dot-haskeline-file}

::: {.index}
single: .haskeline; file single: startup; files, GHCi
:::

GHCi uses [Haskeline](https://hackage.haskell.org/package/haskeline)
under the hood. You can configure it to, among other things, prune
duplicates from GHCi history. See: [Haskeline user
preferences](https://github.com/judah/haskeline/wiki/UserPreferences).

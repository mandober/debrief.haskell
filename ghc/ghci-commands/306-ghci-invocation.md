# 3.6. GHCi invocation

306-ghci-invocation.md

GHCi is invoked with the command `ghci` or `ghc --interactive`. One or
more modules or filenames can also be specified on the command line;
this instructs GHCi to load the specified modules or filenames (and all
the modules they depend on), just as if you had said `:load modules` at
the GHCi prompt (see `ghci-commands`ref"}). For
example, to start GHCi and load the program whose topmost module is in
the file `Main.hs`, we could say:

```bash
$ ghci Main.hs
```

Most of the command-line options accepted by GHC (see
`using-ghc`ref"}) also make sense in
interactive mode. The ones that don't make sense are mostly obvious.

::: {.ghc-flag shortdesc="Use current directory for the GHCi command history
file `.ghci-history`." type="dynamic" reverse="-fno-local-ghci-history" category=""}
-flocal-ghci-history

By default, GHCi keeps global history in `~/.ghc/ghci_history` or
`%APPDATA%/<app>/ghci_history`, but you can use current directory, e.g.:

```bash
$ ghci -flocal-ghci-history
```

It will create `.ghci-history` in current folder where GHCi is launched.
:::

::: {.ghc-flag shortdesc="(Debugging only) check for space leaks when loading
new modules in GHCi." type="dynamic" reverse="-fno-ghci-leak-check" category=""}
-fghci-leak-check

(Debugging only) When loading new modules with `:load`, check that any
previously loaded modules have been correctly garbage collected. Emits
messages if a leak is detected.
:::

### Packages

::: {.index}
single: packages; with GHCi
:::

Most packages (see `using-packages`ref"}) are
available without needing to specify any extra flags at all: they will
be automatically loaded the first time they are needed.

For hidden packages, however, you need to request the package be loaded
by using the `-package ⟨pkg⟩`  flag:

```bash
$ ghci -package readline
GHCi, version 8.y.z: https://www.haskell.org/ghc/  :? for help
Loading package base ... linking ... done.
Loading package readline-1.0 ... linking ... done.
ghci>
```

The following command works to load new packages into a running GHCi:

```bash
ghci> :set -package name
```

But note that doing this will cause all currently loaded modules to be
unloaded, and you'll be dumped back into the `Prelude`.

## Extra libraries

Extra libraries may be specified on the command line using the normal `-llib` option. (The term *library* here refers to libraries of foreign object code; for using libraries of Haskell source code, see `ghci-modules-filenames`ref"}.) For example, to load the "m" library:

```bash
$ ghci -lm
```

On systems with `.so`-style shared libraries, the actual library loaded will the `liblib.so`. GHCi searches the following places for libraries, in this order:
- Paths specified using the `-L ⟨dir⟩` command-line option, The standard library search path for your system loader, which on some systems may be overridden by setting the `LD_LIBRARY_PATH` environment variable.
- The linker standard library search can also be overridden on some systems using the `LIBRARY_PATH` environment variable. Because of some implementation detail on Windows, setting `LIBRARY_PATH` will also extend the system loader path for any library it finds. So often setting `LIBRARY_PATH` is enough.


On systems with dll-style shared libraries, the actual library loaded will be `lib.dll`, `liblib.dll`. GHCi also has full support for import libraries, either Microsoft style `.lib`, or GNU GCC style `.a` and `.dll.a` libraries. If you have an import library it is advisable to always specify the import library instead of the `.dll`. e.g. use `-lgcc` instead of `-llibgcc\_s\_seh-1`. Again, GHCi will signal an error if it can't find the library.

GHCi can also load plain object files (`.o`or`.obj`depending on your platform) or static archives (`.a`) from the command-line. Just add the name the object file or library to the command line. On Windows GHCi also supports the`big-obj`format.  Ordering of`-l` options matters: a library should be mentioned *before* the libraries it depends on (see :ref:`options-linker`).

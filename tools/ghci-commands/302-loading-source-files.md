# 3.2. Loading source files

302-loading-source-files.md

Suppose we have the following Haskell source code, which we place in a file `Main.hs`


    main = print (fac 20)

    fac 0 = 1
    fac n = n * fac (n-1)

You can save `Main.hs` anywhere you like, but if you save it somewhere other than the current directory[^2] then we will need to change to the right directory in GHCi:

```bash
ghci> :cd dir
```

where ⟨dir⟩ is the directory (or folder) in which you saved `Main.hs`.

To load a Haskell source file into GHCi, use the `:load` command:

```bash
ghci> :load Main
Compiling Main             ( Main.hs, interpreted )
Ok, modules loaded: Main.
*ghci>
```

GHCi has loaded the `Main` module, and the prompt has changed to `*ghci>` to indicate that the current context for expressions typed at the prompt is the `Main` module we just loaded. So we can now type expressions involving the functions from `Main.hs`:

```bash
*ghci> fac 17
355687428096000
```

Loading a multi-module program is just as straightforward; just give the name of the "topmost" module to the `:load` command can be abbreviated to `:l`).

The topmost module will normally be `Main`, but it doesn't have to be. GHCi will discover which modules are required, directly or indirectly, by the topmost module, and load them all in dependency order.

`-fshow-loaded-modules` ghc-flag to show the names of modules that GHCi loaded after a :load command (dynamic). default: off, since: 8.2.2.

Typically GHCi will show only the number of modules that it loaded after a `:load` command. With this flag, GHC will also list the loaded modules' names. This was the default behavior prior to GHC 8.2.1 and can be useful for some tooling users.


## Modules vs. filenames

How does GHC find the filename which contains module ⟨M⟩?

It looks for the file `M.hs`, or `M.lhs`. This means that for most modules, the module name must match the filename. If it doesn't, GHCi won't find it.

There is one exception to this general rule: when you load a program with `:load` or specify it when you invoke `ghci`, you can give a filename rather than a module name. This filename is loaded if it exists, and it may contain any module you like. This is particularly convenient if you have several `Main` modules in the same directory and you can't call them all `Main.hs`.

The **search path** for finding source files is specified with the `-i` option on the GHCi command line, like so:

```bash
ghci -idir₁:…:dirₙ
```

or it can be set using the `:set` command from within GHCi.

One consequence of the way that GHCi follows dependencies to find modules to load is that every module must have a source file. The only exception to the rule is modules that come from a package, including the `Prelude` and standard libraries such as `IO` and `Complex`. If you attempt to load a module for which GHCi can't find a source file, even if there are object and interface files for the module, you'll get an error message.


## Making changes and recompilation

If you make some changes to the source code and want GHCi to recompile the program, give the `:reload` command. The program will be recompiled as necessary, with GHCi doing its best to avoid actually recompiling modules if their external dependencies haven't changed. This is the same mechanism we use to avoid re-compiling modules in the batch compilation setting.

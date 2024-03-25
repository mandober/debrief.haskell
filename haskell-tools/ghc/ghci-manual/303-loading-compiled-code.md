# 3.3. Loading compiled code

303-loading-compiled-code.md

Loading compiled code

When you load a Haskell source module into GHCi, it is normally converted to *byte-code* and run using the interpreter. However, *interpreted code* can also run alongside compiled code in GHCi; indeed, normally when GHCi starts, it loads up a compiled copy of the `base` package, which contains the `Prelude`.

Why should we want to run compiled code? Well, compiled code is roughly 10x faster than interpreted code, but takes about 2x longer to produce (perhaps longer if optimisation is on). So it pays to compile the parts of a program that aren't changing very often, and use the interpreter for the code being actively developed.

When loading up source modules with `:load`, GHCi normally looks for any corresponding compiled object files, and will use one in preference to interpreting the source if possible.

For example, suppose we have a 4-module program with diamond shape dependencies, consisting of modules `A`, `B`, `C`, and `D`. Modules `B` and `C` both import `D` only, and `A` imports both `B` and `C`.

```
  A
 / \
B   C
 \ /
  D
```

We can compile `D`, then load the whole program, like this:

```bash
ghci> :! ghc -c -dynamic D.hs
ghci> :load A
Compiling B                ( B.hs, interpreted )
Compiling C                ( C.hs, interpreted )
Compiling A                ( A.hs, interpreted )
Ok, modules loaded: A, B, C, D (D.o).
*ghci>
```

In the messages from the compiler, we see that there is no line for `D`. This is because it isn't necessary to compile `D`, because the source and everything it depends on is unchanged since the last compilation.

Note the `-dynamic` flag to GHC: GHCi uses dynamically-linked object code (if you are on a platform that supports it), and so in order to use compiled code with GHCi it must be compiled for dynamic linking.

At any time you can use the command `:show modules` to get a list of the modules currently loaded into GHCi:

```bash
*ghci> :show modules
D                ( D.hs, D.o )
C                ( C.hs, interpreted )
B                ( B.hs, interpreted )
A                ( A.hs, interpreted )
*ghci>
```


If we now modify the source of `D` (or pretend to: using `touch`), the compiler will no longer be able to use the object file, because it might be out of date:

```bash
*ghci> :! touch D.hs
*ghci> :reload
Compiling D                ( D.hs, interpreted )
Ok, modules loaded: A, B, C, D.
*ghci>
```

Note that module `D` was compiled, but in this instance because its
source hadn't really changed, its interface remained the same, and the
recompilation checker determined that `A`, `B` and `C` didn't need to
be recompiled.

So let's try compiling one of the other modules:

```bash
*ghci> :! ghc -c C.hs
*ghci> :load A
Compiling D                ( D.hs, interpreted )
Compiling B                ( B.hs, interpreted )
Compiling C                ( C.hs, interpreted )
Compiling A                ( A.hs, interpreted )
Ok, modules loaded: A, B, C, D.
```

We didn't get the compiled version of `C`! What happened? Well, in GHCi
a compiled module may only depend on other compiled modules, and in this
case `C` depends on `D`, which doesn't have an object file, so GHCi
also rejected `C`'s object file. Ok, so let's also compile `D`:

```bash
*ghci> :! ghc -c D.hs
*ghci> :reload
Ok, modules loaded: A, B, C, D.
```

Nothing happened! Here's another lesson: newly compiled modules aren't
picked up by `:reload` , only
`:load` :

```bash
*ghci> :load A
Compiling B                ( B.hs, interpreted )
Compiling A                ( A.hs, interpreted )
Ok, modules loaded: A, B, C (C.o), D (D.o).
```

The automatic loading of object files can sometimes lead to confusion,
because non-exported top-level definitions of a module are only
available for use in expressions at the prompt when the module is
interpreted (see `ghci-scope`ref"}). For this
reason, you might sometimes want to force GHCi to load a module using
the interpreter. This can be done by prefixing a `*` to the module name
or filename when using `:load` , for
example

```bash
ghci> :load *A
Compiling A                ( A.hs, interpreted )
*ghci>
```

When the `*` is used, GHCi ignores any pre-compiled object code and
interprets the module. If you have already loaded a number of modules as
object code and decide that you wanted to interpret one of them, instead
of re-loading the whole set you can use `:add *M` to specify that you
want `M` to be interpreted (note that this might cause other modules to
be interpreted too, because compiled modules cannot depend on
interpreted ones).

To always compile everything to object code and never use the
interpreter, use the `-fobject-code` 
option (see `ghci-obj`ref"}).


Since GHCi will only use a compiled object file if it can be sure that
the compiled version is up-to-date, a good technique when working on a
large program is to occasionally run `ghc --make` to compile the whole
project (say before you go for lunch :-), then continue working in the
interpreter. As you modify code, the changed modules will be
interpreted, but the rest of the project will remain compiled.

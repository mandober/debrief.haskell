# 3.13. FAQ and Gotchas

313-faq-and-gotchas.md

FAQ and Things To Watch Out For {#ghci-faq}
-------------------------------

The interpreter can't load modules with foreign export declarations!

:   Unfortunately not. We haven't implemented it yet. Please compile
    any offending modules by hand before loading them into GHCi.

`-O`  doesn't work with GHCi!

> ::: {.index}
> single: optimization; and GHCi
> :::
>
> For technical reasons, the bytecode compiler doesn't interact well
> with one of the optimisation passes, so we have disabled optimisation
> when using the interpreter. This isn't a great loss: you'll get a
> much bigger win by compiling the bits of your code that need to go
> fast, rather than interpreting them with optimisation turned on.

Modules using unboxed tuples or sums will automatically enable
`-fobject-code` 

> ::: {.index}
> single: unboxed tuples, sums; and GHCi
> :::
>
> The bytecode interpreter doesn't support most uses of unboxed tuples
> or sums, so GHCi will automatically compile these modules, and all
> modules they depend on, to object code instead of bytecode.
>
> GHCi checks for the presence of unboxed tuples and sums in a somewhat
> conservative fashion: it simply checks to see if a module enables the
> `UnboxedTuples`extension"} or
> `UnboxedSums`extension"} language extensions.
> It is not always the case that code which enables
> `UnboxedTuples`extension"} or
> `UnboxedSums`extension"} requires
> `-fobject-code` , so if you *really*
> want to compile `UnboxedTuples` 
> role="extension"}/`UnboxedSums` 
> role="extension"}-using code to bytecode, you can do so explicitly by
> enabling the `-fbyte-code`  flag. If
> you do this, do note that bytecode interpreter will throw an error if
> it encounters unboxed tuple/sum--related code that it cannot handle.
>
> Incidentally, the previous point, that `-O` 
> role="  is incompatible with GHCi, is because the bytecode
> compiler can't deal with unboxed tuples or sums.

Concurrent threads don't carry on running when GHCi is waiting for input.

:   This should work, as long as your GHCi was built with the
    `-threaded`  switch, which is the
    default. Consult whoever supplied your GHCi installation.

After using `getContents`, I can't use `stdin`, until I do `:load` or `:reload`

:   This is the defined behaviour of `getContents`: it puts the stdin
    Handle in a state known as semi-closed, wherein any further I/O
    operations on it are forbidden. Because I/O state is retained
    between computations, the semi-closed state persists until the next
    `:load`  or
    `:reload`  command.

    You can make `stdin` reset itself after every evaluation by giving
    GHCi the command `:set +r`. This works because `stdin` is just a
    top-level expression that can be reverted to its unevaluated state
    in the same way as any other top-level expression (CAF).

I can't use `Control-C`kbd"} to interrupt computations in GHCi on Windows.

:   See `ghci-windows`ref"}.

The default buffering mode is different in GHCi to GHC.

:   In GHC, the stdout handle is line-buffered by default. However, in
    GHCi we turn off the buffering on stdout, because this is normally
    what you want in an interpreter: output appears as it is generated.

    If you want line-buffered behaviour, as in GHC, you can start your
    program thus: :

        main = do { hSetBuffering stdout LineBuffering; ... }

[^1]: The "i" stands for "Interactive"

[^2]: If you started up GHCi from the command line then GHCi's current
    directory is the same as the current directory of the shell from
    which it was started. If you started GHCi from the "Start" menu in
    Windows, then the current directory is probably something like
    `C:\Documents and Settings\user name`.

[^3]: Note that in GHCi, and `--make` 
    mode, the `-i`  option is used to
    specify the search path for *source* files, whereas in standard
    batch-compilation mode the `-i` 
    option is used to specify the search path for interface files, see
    `search-path`ref"}.

[^4]: Note that packages only contain compiled code, so debugging a
    package requires finding its source and loading that directly.

[^5]: We originally provided bindings for all variables in scope, rather
    than just the free variables of the expression, but found that this
    affected performance considerably, hence the current restriction to
    just the free variables.

# 3.11. Running the interpreter in a separate process 

311-repl-in-separate-process.md

Running the interpreter in a separate process {#external-interpreter}
---------------------------------------------

Normally GHCi runs the interpreted code in the same process as GHC
itself, on top of the same RTS and sharing the same heap. However, if
the flag `-fexternal-interpreter`  is
given, then GHC will spawn a separate process for running interpreted
code, and communicate with it using messages over a pipe.

::: {.ghc-flag shortdesc="Run interpreted code in a separate process" type="dynamic" category="misc"}
-fexternal-interpreter

since

:   8.0.1

Run interpreted code (for GHCi, Template Haskell, Quasi-quoting, or
Annotations) in a separate process. The interpreter will run in
profiling mode if `-prof`  is in
effect, and in dynamically-linked mode if `-dynamic` 
role="  is in effect.

There are a couple of caveats that will hopefully be removed in the
future: this option is currently not implemented on Windows (it is a
no-op), and the external interpreter does not support the GHCi debugger,
so breakpoints and single-stepping don't work with
`-fexternal-interpreter` .

See also the `-pgmi ⟨cmd⟩` 
(`replacing-phases`ref"}) and
`-opti ⟨option⟩` 
(`forcing-options-through`ref"}) flags.
:::

Why might we want to do this? The main reason is that the RTS running
the interpreted code can be a different flavour (profiling or
dynamically-linked) from GHC itself. So for example:

-   We can use the profiler to collect stack traces when using GHCi (see
    `ghci-stack-traces`ref"}).
-   When compiling Template Haskell code with `-prof` 
    role="  we don't need to compile the modules without
    `-prof`  first (see
    `th-profiling`ref"}) because we can run the
    profiled object code in the interpreter.

This feature is experimental in GHC 8.0.x, but it may become the default
in future releases.

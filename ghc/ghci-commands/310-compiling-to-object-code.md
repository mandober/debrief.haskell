# 3.10. Compiling to object code in GHCi

310-compiling-to-object-code.md


Compiling to object code inside GHCi {#ghci-obj}
------------------------------------

By default, GHCi compiles Haskell source code into byte-code that is
interpreted by the runtime system. GHCi can also compile Haskell code to
object code: to turn on this feature, use the
`-fobject-code`  flag either on the
command line or with `:set`  (the
option `-fbyte-code`  restores
byte-code compilation again). Compiling to object code takes longer, but
typically the code will execute 10-20 times faster than byte-code.

Compiling to object code inside GHCi is particularly useful if you are
developing a compiled application, because the
`:reload`  command typically runs much
faster than restarting GHC with `--make` 
role="  from the command-line, because all the interface files
are already cached in memory.

There are disadvantages to compiling to object-code: you can't set
breakpoints in object-code modules, for example. Only the exports of an
object-code module will be visible in GHCi, rather than all top-level
bindings as in interpreted modules.

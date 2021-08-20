# GHCi

User Guide
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/ghci.html


* :q or :quit           Quit the repl
* :t or :type <expr>    Print inferred type for <expr>
* :set +t               Enable print of inferred type after each expr
* :set +m               Enable multiline commands
* :?                    Show in-repl help



Start repl:
```
$ ghci
GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
Prelude> :quit
```
`Prelude>` is the prompt signyfying that the `Prelude` module has been loaded.

Prefix expr with `:t` to show inferred type, or use `:set +t` to always show it
```
Prelude> :t 5
5 :: Num t => t
Prelude> :set +t
4
4 :: Num t => t
```

Enable printing of timing/memory stats after each evaluation:
```
Prelude> :set +s
Prelude> reverse [1,2,3]
[3,2,1]
(0.03 secs, 68,264 bytes)
```

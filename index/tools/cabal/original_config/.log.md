# Cabal

Haskell's package manager.
YAML config file.

original paths replaced with:

install-dirs user:
original: C:\Users\ivan\AppData\Roaming\cabal
replaced with: `T:\dev\haskell\cabals\userivan`

install-dirs global
original: C:\Program Files\Haskell
replaced with: `T:\dev\haskell\cabals\globals`




`cabal --config-file=FILE`
Set an alternate location for the config file
```
cabal --config-file=T:/dev/haskell/cabals/cabal.config
```

cabal --sandbox-config-file=FILE
Set an alternate location for the sandbox config file (default: `./cabal.sandbox.config`)
cabal --sandbox-config-file=T:/dev/haskell/cabals/cabal.sandbox.config


cabal --default-user-config=FILE
Set a location for a `cabal.config` file for projects without their own `cabal.config` freeze file.
cabal --default-user-config=T:/dev/haskell/cabals/cabal.freeze.config

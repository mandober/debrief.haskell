# implicit-hie

https://github.com/Avi-D-coder/implicit-hie

Auto generate a stack or cabal multi component `hie.yaml` file.

```bash
cd your-stack-or-cabal-package

cabal install implicit-hie
# or
stack install implicit-hie

gen-hie > hie.yaml
```

gen-hie [ --cabal | --stack ] > hie.yaml

* gen-hie should be run the root of a cabal or stack project.

* The config type (cabal or stack) is auto-determined by the existence of `dist-newstyle`, `.stack-work`, `stack.yaml`, items, but if none are found the default is *cabal*.

* Use `--cabal` or `--stack` options to explicitly specify the config type

* All common Cabal and Stack configurations should just work. If you use more advanced features, the generated config may not be complete.
- multi component cabal/stack projects
- multiple executables under a single path
- multiple paths provided to `hs-source-dirs`
- lookup nested packages in _cabal.project_ or _stack.yaml_
- Handle *Exe* and *Bench* `other-modules`


## Refs

* implicit-hie @ hackage
https://hackage.haskell.org/package/implicit-hie

* Mystified by hie.yaml
https://www.reddit.com/r/haskell/comments/hcu5sf/mystified_by_hieyaml/

* haskell-uber-cradle-example
https://github.com/sordina/haskell-uber-cradle-example
Example of HLS `hie.yaml` config for multiple nested dependent project navigation.

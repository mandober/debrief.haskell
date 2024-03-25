# GHC 9.4.1

* download ghc-9.4.1
https://downloads.haskell.org/ghc/9.4.1/

* release notes ghc-9.4.1
https://downloads.haskell.org/ghc/latest/docs/users_guide/9.4.1-notes.html

* migration guide ghc 9.4
https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/9.4


## Changes

- GHC Proposal [#511][#511] **DeepSubsumption** extension
- GHC Proposal [#371][#371] **The built-in type equality**
  - use of equality constraints no longer requires GADTs or TypeFamilies
  - use of equality constraint syntax `a ~ b` requires TypeOperators
    otherwise warning `-Wtype-equality-requires-operators`
  - `~` is now a legal name for a user-defined type operator
  - The built-in type equality is now exported from `Data.Type.Equality` and re-exported from `Prelude`. When `~` is not in scope, its use results in a warning `-Wtype-equality-out-of-scope`
- GHC Proposal [#302][#302] **Multi-way lambda expressions**



[#302]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0302-cases.rst
[#371]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0371-non-magical-eq.md
[#511]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0511-deep-subsumption.rst

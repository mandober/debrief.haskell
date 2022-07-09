# cabal config file format

The format of `${PROJ}.cabal` files that are located in a project's root.

- The very first line *must be* `cabal-version`, e.g. "cabal-version: 3.6"
- Initial part of the .cabal file has the package metadata.
- towards the end of the file you will find the executable or library sections
- cabal files use Haskell-style comment syntax (double dash, `--`)
- Comments are only allowed on lines on their own.
- *Trailing comments are disallowed* lest be confused with program options

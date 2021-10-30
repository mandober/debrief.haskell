# Tools


# Tooling

* Cabal: [Home](https://www.haskell.org/cabal/)
* Cabal: [GitHub repo](https://github.com/haskell/cabal/)
* Hackage: [Package repository](http://hackage.haskell.org/packages/)
* [cabal-install](http://hackage.haskell.org/package/cabal-install): CLI for Cabal and Hackage
* Stack: [Home](http://haskellstack.org/)
* Stack: [Comprehensive video tutorial](https://www.youtube.com/watch?v=sRonIB8ZStw)


## Text Editors

- Emacs
  - [Install haskell-mode](https://github.com/bitemyapp/dotfiles/blob/master/.emacs#L31)
  - [Install flycheck](https://github.com/bitemyapp/dotfiles/blob/master/.emacs#L29)
  - Enable and configure Haskell, this can be just `(require 'haskell)` if you don't care about anything else.
  - [Enable flycheck](https://github.com/bitemyapp/dotfiles/blob/master/.emacs#L97)
  - Symlink Stack to `/usr/bin` or otherwise make sure flycheck can find it
  - [dunzo.](https://twitter.com/bitemyapp/status/693621160571985920)

- Vim
  - We recommend [Stephen Diehl's vim instructions](http://www.stephendiehl.com/posts/vim_2016.html) but we suggest replacing ghc-mod with the below for now.
  - For getting type errors in vim reliably (in lieu of ghc-mod)
```
autocmd FileType haskell setlocal makeprg=stack\ build
autocmd FileType haskell setlocal errorformat=%f:%l:%v:%m
```
from: https://github.com/ishiy1993/dotfiles/blob/master/.vimrc

- [Sublime Text](https://github.com/SublimeHaskell/SublimeHaskell)

- [Atom](https://atom.io/packages/ide-haskell)

- [IntelliJ](https://github.com/carymrobbins/intellij-haskforce)

- Notepad++ has basic Haskell support built in.

- gedit has basic Haskell support built in.


## Other

- Haskell For Mac

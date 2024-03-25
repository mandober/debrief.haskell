# Tools :: Index

ghcid
ghcide (former: hie-core)
vscode-ghc-simple
haskell-language-server
hie-bios
hlint
ormolu
haskell-lsp
ghcide-nix
haskell-ide-engine
haskero
vscode-haskutil
dramforever.vscode-ghc-simple
ndmitchell.haskell-ghcid
haskell.haskell-language-server
implicit-hie
shake
jcanero.hoogle-vscode


* hie-bios
https://github.com/mpickering/hie-bios
https://github.com/mpickering/hie-bios#readme
https://github.com/ndmitchell/shake/blob/master/hie.yaml

* haskell/haskell-language-server
Official haskell ide support via language server (LSP). Successor of `ghcide` & `haskell-ide-engine`.
https://github.com/haskell/haskell-language-server
https://haskell-language-server.readthedocs.io/en/latest/


https://github.com/ndmitchell/hlint
https://github.com/tweag/ormolu
https://github.com/alanz/haskell-lsp
https://4ta.uk/p/shaking-up-the-ide
https://github.com/hercules-ci/ghcide-nix
https://github.com/ndmitchell/shake

* Avi-D-coder/implicit-hie
Auto generate a stack or cabal multi component hie.yaml file
https://github.com/Avi-D-coder/implicit-hie

```bash
cd your-stack-or-cabal-package

cabal install implicit-hie
# or:
# stack install implicit-hie

gen-hie > hie.yaml
```

>latest (2022-10-01) `implicit-hie-0.1.2.7`

https://github.com/Avi-D-coder/implicit-hie/blob/master/ChangeLog.md


* sordina/haskell-uber-cradle-example
example of HLS `hie.yaml` conf for multiple nested dependent project navigation
https://github.com/sordina/haskell-uber-cradle-example


## ghcid

* ndmitchell/ghcid: Very low feature GHCi based IDE
https://github.com/ndmitchell/ghcid

* ghcid is amazing (why have I just heard of it?) : haskell
https://www.reddit.com/r/haskell/comments/96pxuk/ghcid_is_amazing_why_have_i_just_heard_of_it/

* ghcid for the win!
https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html


* haskell org @ github

* cabal
https://github.com/haskell/cabal
Official upstream development repository for Cabal and cabal-install

* vscode-haskell
https://github.com/haskell/vscode-haskell
VS Code extension for Haskell, powered by haskell-language-server

* pvp
Haskell Package Version Policy (PVP)

* hackage-server
Hackage-Server: A Haskell Package Repository

* haskell-language-server
Official haskell ide support via language server (LSP). Successor of ghcide & haskell-ide-engine.

* actions
Github actions for Haskell CI

* haskell-mode



## Visual Studio Marketplace

* jcanero.hoogle-vscode
https://marketplace.visualstudio.com/items?itemName=jcanero.hoogle-vscode

* dramforever.vscode-ghc-simple
dramforever/vscode-ghc-simple: Simple GHC (Haskell) integration for VSCode
https://github.com/dramforever/vscode-ghc-simple
Project Configuration · dramforever/vscode-ghc-simple Wiki
https://github.com/dramforever/vscode-ghc-simple/wiki/Project-Configuration


* DigitalAssetHoldingsLLC.ghcide
ghcide - Visual Studio Marketplace
https://marketplace.visualstudio.com/items?itemName=DigitalAssetHoldingsLLC.ghcide

* ndmitchell.haskell-ghcid
haskell-ghcid - Visual Studio Marketplace
https://marketplace.visualstudio.com/items?itemName=ndmitchell.haskell-ghcid

* alanz.vscode-hie-server
Haskell (legacy) - Visual Studio Marketplace
https://marketplace.visualstudio.com/items?itemName=alanz.vscode-hie-server

* ComplYue.vscode-ghci
GHCi Sessions to run Interactive CodeLens - Visual Studio Marketplace
https://marketplace.visualstudio.com/items?itemName=ComplYue.vscode-ghci

* phoityne.phoityne-vscode
Haskell GHCi Debug Adapter Phoityne - Visual Studio Marketplace
https://marketplace.visualstudio.com/items?itemName=phoityne.phoityne-vscode

* EduardSergeev.vscode-haskutil
EduardSergeev/vscode-haskutil: QuickFix actions for Haskell in VSCode
https://github.com/EduardSergeev/vscode-haskutil

* Vans.haskero
Haskero - Visual Studio Marketplace
https://marketplace.visualstudio.com/items?itemName=Vans.haskero


## Other

* Overcoming Software
https://www.parsonsmatt.org/

* Tutorials
https://www.parsonsmatt.org/tutorials/

* Production Haskell by Matt Parsons [Leanpub PDF/iPad/Kindle]
https://leanpub.com/production-haskell


* Haskell by Example - 3 - Configuration Files - YouTube
https://www.youtube.com/watch?v=AJXzc3oa098&list=PLp2qifo30hMuNgmUUhgl82DTK2JTUqK6M

* Haskell tidbits: 24 days of Hackage, 2015: day 1: Introduction and Stack · Franklin Chen
https://conscientiousprogrammer.com/blog/2015/11/30/haskell-tidbits-24-days-of-hackage-2015-day-1-introduction-and-stack/

* ocharles.org.uk
https://blog.ocharles.org.uk/blog/

* In Praise of EitherT
https://blog.ocharles.org.uk/blog/posts/2012-07-24-in-praise-of-EitherT.html

* Announcing Raskell
https://blog.ocharles.org.uk/blog/posts/2012-08-05-announcing-raskell.html

* 24 Days of Hackage: Cabal
https://blog.ocharles.org.uk/blog/posts/2012-12-01-24-days-of-hackage.html

* 24 days of Hackage, 2015: day 8: multiset; I wish this were in the standard containers package · Franklin Chen
https://conscientiousprogrammer.com/blog/2015/12/08/24-days-of-hackage-2015-day-8-multiset-i-wish-this-were-in-the-standard-containers-package/

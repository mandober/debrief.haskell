# Language Options in GHC 8.10.1 User's Guide

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html

Set LANGUAGE or OPTIONS_GHC pragmas:
- as flags on cmdline
- in module itself as a pragma on top
- in ghci.conf
- in project.cabal file
- probably in some enwar (?)

file://wsl%24/ubuntu1804/home/ivan/.ghc/ghci.conf
file://wsl%24/ubuntu1804/home/ivan/.haskeline



## ImportQualifiedPost
allows `qualified` to appear in postpositive position: `import M qualified`

{-# LANGUAGE ImportQualifiedPost        #-}

## UnicodeSyntax
enables Unicode chars for certain ASCII character sequences:
∷  ⇒  → ←  ⤚ ⤙  ⤜ ⤛  ★  ∀   ⦇ ⦈  ⟦ ⟧

{-# LANGUAGE UnicodeSyntax              #-}


{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ExplicitForAll #-}


{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE CPP #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}


-- -Wno-orphans is needed for things like:
-- Orphan rule: "x# -# x#" ALWAYS forall x# :: Int# -# x# x# = 0
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK hide #-}



{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE BinaryLiterals             #-}
{-# LANGUAGE NegativeLiterals           #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE NoDatatypeContexts         #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE NondecreasingIndentation   #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE PostfixOperators           #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE CPP                        #-}

# GHC Version 9.0.1

<!-- TOC -->

- [Changes: highlights](#changes-highlights)
  - [LinearTypes](#lineartypes)
  - [NCG](#ncg)
  - [Pattern-Match Coverage Checking](#pattern-match-coverage-checking)
  - [Windows: Use the large address-space allocator](#windows-use-the-large-address-space-allocator)
  - [Windows: New IO Manager](#windows-new-io-manager)
  - [Big-number support](#big-number-support)
  - [Template Haskell splices](#template-haskell-splices)
- [Changes: full details](#changes-full-details)
  - [Language](#language)
  - [Compiler](#compiler)
  - [GHCi](#ghci)
  - [Runtime system](#runtime-system)
  - [Template Haskell](#template-haskell)
  - [Arrow notation](#arrow-notation)
  - [Haddock](#haddock)
  - [`base` library](#base-library)
  - [`ghc-prim` library](#ghc-prim-library)
  - [`ghc` library](#ghc-library)
  - [`base` library](#base-library-1)
  - [Build system](#build-system)
    - [Bootstrapping requirements](#bootstrapping-requirements)
    - [Included libraries](#included-libraries)

<!-- /TOC -->

The significant changes to the various parts of the compiler are listed in the following sections.

The ghc-flag `LLVM backend <-fllvm>` of this release is to be used with LLVM 9.

## Changes: highlights

### LinearTypes

* The :extension:`LinearTypes` extension enables linear function syntax
  `a %1 -> b`, as described in the `Linear Types GHC proposal
  <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst>`__.

  The GADT syntax can be used to define data types with linear and nonlinear fields.

  This extension is considered experimental: it doesn't implement the full proposal yet and the details
  are subject to change.

### NCG

  - The linear register allocator saw improvements reducing the number
    of redundant move instructions. Rare edge cases can see double
    digit improvements in runtime for inner loops.

    In the mean this improved runtime by about 0.8%. For details
    see ticket :ghc-ticket:`17823`.

### Pattern-Match Coverage Checking

  - The revamp of the pattern-match coverage checker that started in 8.10 concludes with this release and implements the
    novel `*Lower Your Guards* <https://www.microsoft.com/en-us/research/uploads/prod/2020/03/lyg.pdf>`_ algorithm.
  - Compared to 8.10, end users might notice improvements to "long-distance information": ::

      f True = 1
      f x    = ... case x of { False -> 2; True -> 3 } ...

    GHC is now able to detect the case alternative returning `3` as redundant.
  - Some more performance improvements in edge cases.

### Windows: Use the large address-space allocator

  This improves runtime but causes increased memory usage on Windows versions
  older than Win 8.1/Server 2012.

### Windows: New IO Manager

  A new I/O manager (WinIO) is now available as a community technical preview which is designed to allow experimentation and
  bootstrapping of third-party packages such as Network.   The new I/O manager is off by default and can be enabled with the
  RTS flag `--io-manager=native`.  Currently the I/O manager is *unoptimized* and is focused more on correctness.  There is also
  no support for pipes and sockets.  These will be added in the next release.  `*see more* <https://www.youtube.com/watch?v=kgNh5mdZ1xw>`__.


### Big-number support

GHC now relies on a new `ghc-bignum` package to provide Integer/Natural impl. This package supports the following backends:
- gmp: adapted from integer-gmp package that was used before
- native: new Haskell implementation, faster than `integer-simple` which is not used anymore
- All backends now use the same representation for big numbers (the one that was previously used only by `integer-gmp`). It led to several compiler simplifications, performance improvements and bug fixes (e.g. ghc-ticket `15262`, ghc-ticket `15286`).
- All backends must provide exactly the same set of functions with deterministic results so that they can be tested one against the other (they can only differ in performance). As a consequence, some functions that were only provided by integer-gmp (prime test, secure powmod, etc.) are no longer provided by ghc-bignum. Note that other packages (e.g. `hgmp`) provide these functions.
- For now GHC still doesn't allow dynamic selection of the `ghc-bignum` backend to use.

### Template Haskell splices

Breaking change: Template Haskell splices now act as separation points between constraint solving passes. It is no longer possible to use an instance of a class before a splice and define that instance after a splice.

For example, this code now reports a missing instance for `C Bool`

      class C a where foo :: a
      bar :: Bool
      bar = foo
      $(return [])
      instance C Bool where foo = True

Support for 32-bit Windows has officially been dropped as Microsoft has formally discontinued new 32-bit Windows 10 releases in 2020. See ghc-ticket `18487` for details.



## Changes: full details

### Language

* Record field selectors are now given type signatures that preserve the
  user-written order of quantified type variables. Moreover, field selector
  type signatures no longer make inferred type variables available for explicit
  type application. See :ref:`field-selectors-and-type-applications` for more
  details.

  In certain situations, this will constitute a breaking change as this can
  affect :extension:`TypeApplications`. For instance, given the following
  definitions: ::

    {-# LANGUAGE PolyKinds #-}

    newtype P a = MkP { unP :: Proxy a }

    newtype N :: Type -> Type -> Type where
      MkN :: forall b a. { unN :: Either a b } -> N a b

  Previous versions of GHC would give the following types to `unP` and
  `unN`: ::

    unP :: forall k (a :: k). P a -> Proxy a
    unN :: forall a b. N a b -> Either a b

  GHC will now give them the following types instead: ::

    unP :: forall {k} (a :: k). P a -> Proxy a
    unN :: forall b a. N a b -> Either a b

* In obscure scenarios, GHC now rejects programs it previously accepted, but
  with unhelpful types. For example, if (with `-XPartialTypeSignatures`) you
  were to write `x :: forall (f :: forall a (b :: a -> Type). b _). f _`, GHC previously
  would have accepted `x`, but its type would have involved the mysterious `Any`
  internal type family. Now, GHC rejects, explaining the situation.

* GHC now more faithfully implements the instance-lookup scheme described with
  :extension:`QuantifiedConstraints`. Previous bugs meant that programs like this
  were accepted::

    data T (c :: Type -> Constraint)
    instance (forall h. c h => Functor h) => Functor (T c)
    instance (forall h. c h => Applicative h) => Applicative (T c)

  Note that in the instance declaration for `Applicative (T c)`, we cannot prove
  `Functor (T c)`, because the quantified constraint shadows the global instance.
  There is an easy workaround, though: just include `Functor (T c)` as an assumption. ::

    instance (forall h. c h => Applicative h, Functor (T c)) => Applicative (T c)

  There is a chance we will tweak the lookup scheme in the future, to make this
  workaround unnecessary.

* GHC now consistently does eager instantiation during type inference.
  As a consequence, visible type application (VTA) now only works when
  the head of the application is:

  * A variable
  * An expression with a type signature

  For example `(let x = blah in id) @Bool True` no longer typechecks.
  You should write `let x = blah in id @Bool True` instead.

  This change prepares the way for `Quick Look impredicativity
  <https://gitlab.haskell.org/ghc/ghc/issues/18126>`_.

* GHC now implements simplified subsumption, as described in `GHC Proposal #287 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst>`__.
  This change simplifies the type system, and prevents the possibility of GHC
  silently changing the semantics of user programs, but it does mean that some libraries
  may need eta-expansion to typecheck.  More info here: :ref:`simple-subsumption`.

  This change also prepares the way for Quick Look impredicativity.

* GHC now allows users to manually define the specificity of type variable
  binders. By marking a variable with braces `{tyvar}` or `{tyvar :: kind}`,
  it becomes inferred despite appearing in a type signature. This feature
  effectively allows users to choose which variables can or can't be
  instantiated through visible type application. More information can be found
  here: :ref:`Manually-defining-inferred-variables`.

* GADT constructor types now properly adhere to :ref:`forall-or-nothing`. As
  a result, GHC will now reject some GADT constructors that previous versions
  of GHC would accept, such as the following: ::

    data T where
      MkT1 :: (forall a. a -> b -> T)
      MkT2 :: (forall a. a -> T)

  `MkT1` and `MkT2` are rejected because the lack of an outermost
  `forall` triggers implicit quantification, making the explicit `forall`\ s
  nested. Furthermore, GADT constructors do not permit the use of nested
  `forall`\ s, as explained in :ref:`formal-gadt-syntax`.

  In addition to rejecting nested `forall`\ s, GHC is now more stringent about
  rejecting uses of nested *contexts* in GADT constructors. For example, the
  following example, which previous versions of GHC would accept, is now
  rejected: ::

    data U a where
      MkU :: (Show a => U a)

* GHC more strictly enforces the rule that the type in the top of an instance
  declaration is not permitted to contain nested `forall`\ s or contexts, as
  documented in :ref:`formal-instance-syntax`. For example, the following
  examples, which previous versions of GHC would accept, are now rejected:

    instance (forall a. C a) where ...
    instance (Show a => C a) where ...

  In addition, GHC now enforces the rule that the types in `deriving` clauses
  and `via` types (for instances derived with :extension:`DerivingVia`)
  cannot contain nested `forall`\ s or contexts. For example, the following
  examples, which previous versions of GHC would accept, are now rejected: ::

    data T = MkT deriving (C1, (forall x. C2 x))
    deriving via (forall x. V x) instance C (S x)

* A new language extension :extension:`QualifiedDo` is implemented, allowing
  to qualify a do block to control which operations to use for desugaring do
  syntax. ::

    {-# LANGUAGE QualifiedDo #-}
    import qualified SomeModule as M

    f x = M.do           -- desugars to:
      y <- M.return x    -- M.return x M.>>= \y ->
      M.return y         -- M.return y M.>>
      M.return y         -- M.return y

  See :ref:`qualified-do-notation` for more details.

* :extension:`LexicalNegation` is a new extension that detects whether the
  minus sign stands for negation during lexical analysis by checking for the
  surrounding whitespace: ::

    a = x - y  -- subtraction
    b = f -x   -- negation

    f = (- x)  -- operator section
    c = (-x)   -- negation

* The behavior of :extension:`NegativeLiterals` changed, and now we require
  that a negative literal must not be preceded by a closing token (see
  `GHC Proposal #229 <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst>`__
  for the definition of a closing token). In other words, we parse `f -123`
  as `f (-123)`, but `x-123` as `(-) x 123`. Before this amendment,
  :extension:`NegativeLiterals` caused `x-123` to be parsed as `x(-123)`.


### Compiler

A new flag *ghc-flag* `-flink-rts` to enable linking the RTS when linking shared libraries.

### GHCi

- GHCi prompt no longer lists loaded modules. The previous behavior can be restored with `:set prompt "%s> "` and `:set prompt-cont "%s| "`.

- The `:script` command now allows for file names that contain spaces to passed as arguments: either by enclosing the file names in double quotes or by escaping spaces in file names with a backslash (ghc-ticket `18027`).

- The GHCi debugger syntax `:break <qualified.name>` now allows to set breakpoints on all functions. The restrictions `top-Level` and `exported` have been removed. Hence it's now possible to use this syntax to set breakpoints on functions defined in nested `where` or `let` clauses.


### Runtime system

- :rts-flag:`-N` without a count now tries to respect the number of processors
  in the process's affinity mask, making GHC's behavior more predictable in
  containerized settings (:ghc-ticket:`14781`).

- Support for Windows Vista has been dropped. GHC-compiled programs now require
  Windows 7 or later.

- Windows now uses the large address space allocator by default.
  In extreme cases we saw improvements by up to 3% decreased runtime.

  The downside is that haskell apps run on older (Pre Win-8.1/Server 2012)
  systems will have higher memory footprints.

### Template Haskell


- Implement the `Overloaded Quotations proposal (#246) <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0246-overloaded-bracket.rst>`_.
  The type of all quotation forms have now been generalised in terms of a
  minimal interface necessary (the `Quote` type class) for the
  implementation rather than the overapproximation of the `Q` monad.

- Template Haskell quotes now handle fixity declarations in `let` and
  `where` bindings properly. Previously, such fixity declarations would
  be dropped when quoted due to a Template Haskell bug.

- The `-XTemplateHaskellQuotes` extension now allows nested splices as nested
  splices do not lead directly to compile-time evaluation. (Merge request
  `!2288 <https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2288>`_)

### Arrow notation

- When combined with :extension:`Arrows`, the :extension:`LambdaCase` extension
  now additionally allows `\case` syntax to be used as a command in `proc`
  notation.

- When combined with :extension:`Arrows`, the effects of the
  :extension:`BlockArguments` extension now also apply to applications of
  arrow control operators in `(|` banana brackets `|)`: ::

    (| untilA (increment -< x + y) do
         within 0.5 -< x
         ... |)

### Haddock

- Parsing is now more robust to insufficiently indented Haddock comments::

    class C a where
      f :: a -> a
    -- ^ This comment used to trigger a parse error
      g :: a -> a

- :ghc-flag:`-Winvalid-haddock` is a new warning that reports discarded Haddock
  comments that cannot be associated with AST elements::

    myValue =
      -- | Invalid (discarded) comment in an expression
      2 + 2

- When faced with several comments for a data constructor or a data constructor
  field, Haddock now picks the first one instead of the last one.  The
  extraneous comment is reported as invalid when :ghc-flag:`-Winvalid-haddock`
  is enabled::

    data T
      -- | First comment
      = MkT
      -- ^ Second comment (rejected)


- Haddock is now more relaxed about the placement of comments in types relative
  to the function arrow `->`, allowing more formatting styles::

    f :: Int ->   -- ^ comment on Int (no longer a parse error)
         Bool     -- ^ comment on Bool

- Haddock can now parse the documentation comment for the first declaration in
  a module without a module header (:ghc-ticket:`17561`)::

    -- | This comment used to trigger a parse error
    main = putStrLn "Hello"


### `base` library

- `Foreign.ForeignPtr.withForeignPtr` is now less aggressively optimised,
  avoiding the unsoundness issue reported in
  :ghc-ticket:`17760` in exchange for a small amount of additional allocation.

  If your application is impacted significantly by this change and the
  continuation given to `withForeignPtr` will not *provably* diverge (via
  throwing of an exception or looping) then the previous optimisation behavior
  can be recovered by instead using `GHC.ForeignPtr.unsafeWithForeignPtr`.


### `ghc-prim` library

- Add a known-key `cstringLength#` to `GHC.CString` that is eligible
  for constant folding by a built-in rule.

- A new primop, `keepAlive#`, has been introduced to replace `touch#` in
  controlling object lifetime without the soundness issues affecting the latter
  (see :ghc-ticket:`17760`)

### `ghc` library


- The type of the `getAnnotations` function has changed to better reflect
  the fact that it returns two different kinds of annotations, those on
  names and those on modules: ::

     getAnnotations :: Typeable a
                    => ([Word8] -> a) -> ModGuts
                    -> CoreM (ModuleEnv [a], NameEnv [a])

- The meaning of the `hs_fixds` field of `HsGroup` has changed slightly.
  It now only contains fixity signatures defined for top-level declarations
  and class methods defined *outside* of the class itself. Previously,
  `hs_fixds` would also contain fixity signatures for class methods defined
  *inside* the class, such as the fixity signature for `m` in the following
  example: ::

    class C a where
      infixl 4 `m`
      m :: a -> a -> a

  If you wish to attain the previous behavior of `hs_fixds`, use the new
  `hsGroupTopLevelFixitySigs` function, which collects all top-level fixity
  signatures, including those for class methods defined inside classes.

- The `Exception` module was boiled down acknowledging the existence of
  the `exceptions` dependency. In particular, the `ExceptionMonad`
  class is not a proper class anymore, but a mere synonym for `MonadThrow`,
  `MonadCatch`, `MonadMask` (all from `exceptions`) and `MonadIO`.
  All of `g*`-functions from the module (`gtry`, `gcatch`, etc.) are
  erased, and their `exceptions`-alternatives are meant to be used in the
  GHC code instead.

- `parseModule` is now the only parser entry point that deals with Haddock
  comments. The other entry points (`parseDeclaration`, `parseExpression`,
  etc) do not insert the Haddock comments into the AST.


### `base` library


- `ForeignPtrContents` has a new nullary data constructor `FinalPtr`.
  `FinalPtr` is intended for turning a primitive string literal into a
  `ForeignPtr`.  Unlike `PlainForeignPtr`, `FinalPtr` does not have
  a finalizer. Replacing `PlainForeignPtr` that has `NoFinalizers` with
  `FinalPtr` reduces allocations, reduces the size of compiled binaries,
  and unlocks important Core-to-Core optimizations. `FinalPtr` will be used
  in an upcoming `bytestring` release to improve the performance of
  `ByteString` literals created with `OverloadedStrings`.

### Build system

#### Bootstrapping requirements

- GHC now requires a C compiler which supports
  `__atomic_op_n` builtins. This raises the requirement for GCC to 4.7.


#### Included libraries

The package database provided with this distribution also contains a number of
packages other than GHC itself. See the changelogs provided with these packages
for further change information.

ghc-package-list

libraries/array/array.cabal:             Dependency of `ghc` library
libraries/base/base.cabal:               Core library
libraries/binary/binary.cabal:           Dependency of `ghc` library
libraries/bytestring/bytestring.cabal:   Dependency of `ghc` library
libraries/Cabal/Cabal/Cabal.cabal:       Dependency of `ghc-pkg` utility
libraries/containers/containers/containers.cabal:   Dependency of `ghc` library
libraries/deepseq/deepseq.cabal:         Dependency of `ghc` library
libraries/directory/directory.cabal:     Dependency of `ghc` library
libraries/exceptions/exceptions.cabal:   Dependency of `ghc` and `haskeline` library
libraries/filepath/filepath.cabal:       Dependency of `ghc` library
compiler/ghc.cabal:                      The compiler itself
libraries/ghci/ghci.cabal:               The REPL interface
libraries/ghc-boot/ghc-boot.cabal:       Internal compiler library
libraries/ghc-boot-th/ghc-boot-th.cabal: Internal compiler library
libraries/ghc-compact/ghc-compact.cabal: Core library
libraries/ghc-heap/ghc-heap.cabal:       GHC heap-walking library
libraries/ghc-prim/ghc-prim.cabal:       Core library
libraries/haskeline/haskeline.cabal:     Dependency of `ghci` executable
libraries/hpc/hpc.cabal:                 Dependency of `hpc` executable
libraries/integer-gmp/integer-gmp.cabal: Core library
libraries/libiserv/libiserv.cabal:       Internal compiler library
libraries/mtl/mtl.cabal:                 Dependency of `Cabal` library
libraries/parsec/parsec.cabal:           Dependency of `Cabal` library
libraries/pretty/pretty.cabal:           Dependency of `ghc` library
libraries/process/process.cabal:         Dependency of `ghc` library
libraries/stm/stm.cabal:                 Dependency of `haskeline` library
libraries/template-haskell/template-haskell.cabal:     Core library
libraries/terminfo/terminfo.cabal:       Dependency of `haskeline` library
libraries/text/text.cabal:               Dependency of `Cabal` library
libraries/time/time.cabal:               Dependency of `ghc` library
libraries/transformers/transformers.cabal: Dependency of `ghc` library
libraries/unix/unix.cabal:               Dependency of `ghc` library
libraries/Win32/Win32.cabal:             Dependency of `ghc` library
libraries/xhtml/xhtml.cabal:             Dependency of `haddock` executable

# Safe

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/safe_haskell.html

6.18. Safe Haskell
  6.18.1. Uses of Safe Haskell
    6.18.1.1. Strict type-safety (good style)
    6.18.1.2. Building secure systems (restricted IO Monads)
  6.18.2. Safe Language
    6.18.2.1. Safe Overlapping Instances
  6.18.3. Safe Imports
  6.18.4. Trust and Safe Haskell Modes
    6.18.4.1. Trust check (-fpackage-trust disabled)
    6.18.4.2. Trust check (-fpackage-trust enabled)
    6.18.4.3. Example
    6.18.4.4. Trustworthy Requirements
    6.18.4.5. Package Trust
  6.18.5. Safe Haskell Inference
  6.18.6. Safe Haskell Flag Summary
  6.18.7. Safe Compilation


## Safe Haskell

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/safe_haskell.html

*Safe Haskell* is an extension to the Haskell language that is implemented in GHC as of version 7.2. It allows for *unsafe code to be securely included in a trusted code base* by restricting the features of GHC Haskell the code is allowed to use. Put simply, it makes the types of programs trustable.

While a primary use case of Safe Haskell is *running untrusted code*, Safe Haskell doesn't provide this directly. Instead, Safe Haskell provides *strict type safety*. Without Safe Haskell, GHC allows many exceptions to the type system which can subvert any abstractions. By providing strict type safety, Safe Haskell enables developers to build their own library level sandbox mechanisms to run untrusted code.

While Safe Haskell is an extension, *it actually runs in the background for every compilation with GHC*. It does this to track the type violations of modules to infer their safety, even when they aren't explicitly using Safe Haskell.

The design of Safe Haskell covers the following aspects:
* A safe language dialect of Haskell that provides stricter guarantees about the code. *It allows types and module boundaries to be trusted*.
* A safe import extension that *specifies that the module being imported must be trusted*.
* A definition of trust (or safety) and how it operates, along with ways of defining and changing the trust of modules and packages.

Safe Haskell, however, *does not offer compilation safety*. During compilation time it is possible for arbitrary processes to be launched, using for example the custom pre-processor flag. This can be manipulated to either compromise a user's system at compilation time, or to modify the source code just before compilation to try to alter Safe Haskell flags.

Safe Haskell has been designed with two use cases in mind:
1. Enforcing strict type safety at compile time
2. Compiling and executing untrusted code

## Strict type-safety (enforcing good style)

Haskell offers a powerful type system and separation of pure and effectual functions through the `IO` monad. However, there are several loop holes in the type system, the most obvious being the `unsafePerformIO :: IO a -> a` function. The safe language dialect of Safe Haskell *disallows the use of such functions*. This can be useful restriction as it makes Haskell code easier to analyse and reason about. It also codifies the existing culture in the Haskell community of trying to avoid unsafe functions unless absolutely necessary. As such, using the safe language (through the `-XSafe` flag) can be thought of as a way of *enforcing good style*, similar to the function of `-Wall`.

## Building secure systems (restricted IO Monads)

Systems such as information flow control security, capability based security systems and DSLs for working with encrypted data, etc., can be built in the Haskell language as a library. However they require guarantees about the properties of Haskell that aren't true in general due to the presence of functions like `unsafePerformIO`. *Safe Haskell gives users enough guarantees about the type system to allow them to build such secure systems*.

As an example, let's define an interface for a plugin system where the plugin authors are untrusted, possibly malicious third-parties. We do this by restricting the plugin interface to pure functions or to a *restricted IO monad* that we have defined. The restricted IO monad will only allow a safe subset of IO actions to be executed. We define the plugin interface so that it requires the plugin module, `Danger`, to export a single computation, `Danger`.`runMe`, of type `RIO ()`, where `RIO` is a monad defined as follows:

(...)


## 6.18.2. Safe Language

The Safe Haskell safe language, enabled by `-XSafe`, guarantees the following properties:

* **Referential transparency** - The types can be trusted. Any pure function, is guaranteed to be pure. Evaluating them is deterministic and won't cause any side effects. Functions in the IO monad are still allowed and behave as usual. So, for example, the `unsafePerformIO :: IO a -> a` function is disallowed in the safe language to enforce this property.

* **Module boundary control** - Only symbols that are publicly available through other module export lists can be accessed in the safe language. Values using data constructors not exported by the defining module, cannot be examined or created. As such, if a module M establishes some invariants through careful use of its export list, then code written in the safe language that imports M is guaranteed to respect those invariants.

* **Semantic consistency** - For any module that imports a module written in the safe language, expressions that compile both with and without the safe import have the same meaning in both cases. That is, importing a module written in the safe language cannot change the meaning of existing code that isn't dependent on that module. So, for example, there are some restrictions placed on the use of OverlappingInstances, as these can violate this property.

* **Strict subset** - The safe language is strictly a subset of Haskell as implemented by GHC. Any expression that compiles in the safe language has the same meaning as it does when compiled in normal Haskell.

> These 4 properties guarantee that in the safe language you can trust the types, can trust that module export lists are respected, and can trust that code that successfully compiles has the same meaning as it normally would.

To achieve these properties, in the safe language dialect we disable completely the following features:

* *TemplateHaskell* - Can be used to gain access to constructors and abstract data types that weren't exported by a module, subverting module boundaries.

Furthermore, we restrict the following features:

* *ForeignFunctionInterface* - Foreign import declarations that import a function with a non-IO type are disallowed.

* *RULES* - Rewrite rules defined in a module M compiled with Safe are dropped. Rules defined in Trustworthy modules that M imports are still valid and will fire as usual.

* *OverlappingInstances* - There is no restriction on the creation of overlapping instances, but we do restrict their use at a particular call site. This is a detailed restriction, please refer to Safe Overlapping Instances for details.

* *GeneralisedNewtypeDeriving* - GND is not allowed in the safe language. This is due to the ability of it to violate module boundaries when module authors forget to put nominal role annotations on their types as appropriate. For this reason, the `Data.Coerce` module is also considered unsafe (we are hoping to find a better solution here in the future).

* *GHC.Generics* - Hand crafted instances of the `Generic` type class are not allowed in Safe Haskell. Such instances aren't strictly unsafe, but there is an important invariant that a Generic instance should adhere to the structure of the data type for which the instance is defined, and allowing manually implemented Generic instances would break that invariant. Derived instances (through the `DeriveGeneric` extension) are still allowed. Note that the only allowed deriving strategy for deriving Generic under Safe Haskell is `stock`, as another strategy (e.g., `anyclass`) would produce an instance that violates the invariant.

## 6.18.3. Safe Imports

Safe Haskell enables a small extension to the usual import syntax of Haskell, adding a safe keyword:

impdecl -> import [safe] [qualified] modid [as modid] [impspec]

When used, the module being imported with the `safe` keyword must be a trusted module, otherwise a compilation error will occur. The safe import extension is enabled by either of the `-XSafe`, `-XTrustworthy` or `-XUnsafe` flags. When the `-XSafe` flag is used, the `safe` keyword is allowed but meaningless, as every import is treated as a safe import.

## 6.18.4. Trust and Safe Haskell Modes

Safe Haskell introduces the following *3 language flags*:

* `Safe` - Enables the safe language dialect, asking GHC to guarantee trust. The safe language dialect requires that all imports be trusted or a compilation error will occur. Safe Haskell will also infer this safety type for modules automatically when possible. Please refer to section Safe Haskell Inference for more details of this.

* `Trustworthy` - Means that while this module may invoke unsafe functions internally, the module's author claims that it exports an API that can't be used in an unsafe way. This doesn't enable the safe language. It does however restrict the resolution of overlapping instances to only allow safe overlapping instances. The trust guarantee is provided by the module author, not GHC. An import statement with the safe keyword results in a compilation error if the imported module is not trusted. An import statement without the keyword behaves as usual and can import any module whether trusted or not.

* `Unsafe` - Marks the module being compiled as unsafe so that modules compiled using Safe can't import it. You may want to explicitly mark a module unsafe when it exports internal constructors that can be used to violate invariants.

While these are flags, they also correspond to Safe Haskell module types that a module can have. You can think of using these as declaring an explicit contract (or type) that a module must have. If it is invalid, then compilation will fail. GHC will also infer the correct type for Safe Haskell, please refer to section Safe Haskell Inference for more details.

The procedure to check if a module is trusted or not depends on if the `-fpackage-trust` flag is present. The check is similar in both cases with the -fpackage-trust flag enabling an extra requirement for trustworthy modules to be regarded as trusted.


## 6.18.5. Safe Haskell Inference

In the case where a module is compiled without one of `Safe`, `Trustworthy` or `Unsafe` being used, GHC will try to figure out itself if the module can be considered safe. This safety inference will never mark a module as trustworthy, only as either unsafe or as safe. GHC uses a simple method to determine this for a module M: If M would compile without error under the Safe flag, then M is marked as safe. Otherwise, it is marked as unsafe.

When should you use Safe Haskell inference and when should you use an explicit Safe flag? The later case should be used when you have a hard requirement that the module be safe. This is most useful for the Uses of Safe Haskell of Safe Haskell: running untrusted code. Safe inference is meant to be used by ordinary Haskell programmers. Users who probably don’t care about Safe Haskell.

Haskell library authors have a choice. Most should just use Safe inference. Assuming you avoid any unsafe features of the language then your modules will be marked safe.

Inferred vs. Explicit has the following trade-offs:

* **Inferred** — This works well and adds no dependencies on the Safe Haskell type of any modules in other packages. It does mean that the Safe Haskell type of your own modules could change without warning if a dependency changes. One way to deal with this is through the use of Safe Haskell warning flags that will warn if GHC infers a Safe Haskell type different from expected.

* **Explicit** — This gives your library a stable Safe Haskell type that others can depend on. However, it will increase the chance of compilation failure when your package dependencies change.

## 6.18.6. Safe Haskell Flag Summary

In summary, Safe Haskell consists of the following 3 language flags:

* `Safe` since: 7.2.1
Restricts the module to the safe language. All of the module’s direct imports must be trusted, but the module itself need not reside in a trusted package, because the compiler vouches for its trustworthiness. The “safe” keyword is allowed but meaningless in import statements, as regardless, every import is required to be safe.
- Module Trusted — Yes
- Haskell Language — Restricted to Safe Language
- Imported Modules — All forced to be safe imports, all must be trusted.

* `Trustworthy` Since: 7.2.1
This establishes that the module is trusted, but the guarantee is provided by the module’s author. A client of this module then specifies that they trust the module author by specifying they trust the package containing the module. Trustworthy doesn’t restrict the module to the safe language. It does however restrict the resolution of overlapping instances to only allow safe overlapping instances. It also allows the use of the safe import keyword.
- Module Trusted — Yes.
- Module Trusted (-fpackage-trust enabled) — Yes but only if the package the module resides in is also trusted.
- Haskell Language — Unrestricted, except only safe overlapping instances allowed.
- Imported Modules — Under control of module author which ones must be trusted.

* `Unsafe` Since: 7.4.1
Mark a module as unsafe so that it can’t be imported by code compiled with Safe. Also enable the Safe Import extension so that a module can require a dependency to be trusted.
- Module Trusted — No
- Haskell Language — Unrestricted
- Imported Modules — Under control of module author which ones must be trusted.


A flag to disable Safe Haskell checks:

`-fno-safe-haskell`

This flag can be enabled to override any declared safety property of the module (Safe, Unsafe, Trustworthy) so compilation proceeds as if none of these flags were specified. This is particularly useful when compiling using plugins, which usually results in the compiled modules being marked as unsafe.

And one general flag:

`-fpackage-trust`

When enabled, turn on an extra check for a trustworthy module M, requiring the package that M resides in be considered trusted, for M to be considered trusted.

And 5 warning flags:

1. `-Wunsafe`
Issue a warning if the module being compiled is regarded to be unsafe. Should be used to check the safety type of modules when using safe inference.

2. `-Wsafe`
Issue a warning if the module being compiled is regarded to be safe. Should be used to check the safety type of modules when using safe inference. If the module is explicitly marked as safe then no warning will be issued.

3. `-Wtrustworthy-safe`
Issue a warning if the module being compiled is marked as -XTrustworthy but it could instead be marked as -XSafe , a more informative bound. Can be used to detect once a Safe Haskell bound can be improved as dependencies are updated.

4. `-Winferred-safe-imports` Since: 8.10.1
The module A below is annotated to be explicitly Safe, but it imports Safe-Inferred module.

```hs
{-# LANGUAGE Safe #-}
module A where

import B (double)

quad :: Int -> Int
quad = double . double


module B where

double :: Int -> Int
double n = n + n
```

The inferred status is volatile: if an unsafe import is added to the module B, it will cause compilation error of A. When -Winferred-safe-imports is enabled, the compiler will emit a warning about this. This option is off by default.

5. `-Wmissing-safe-haskell-mode` Since: 8.10.1
The compiler will warn when none of Safe, Trustworthy or Unsafe is specified. This option is off by default.


## 6.18.7. Safe Compilation

GHC includes a variety of flags that allow arbitrary processes to be run at compilation time. One such example is the custom pre-processor flag. Another is the ability of Template Haskell to execute Haskell code at compilation time, including IO actions. Safe Haskell does not address this danger (although, Template Haskell is a disallowed feature).

Due to this, it is suggested that when compiling untrusted source code that has had no manual inspection done, the following precautions be taken:

- Compile in a sandbox, such as a chroot or similar container technology. Or simply as a user with very reduced system access.
- Compile untrusted code with the `-XSafe` flag being specified on the command line. This will ensure that modifications to the source being compiled can’t disable the use of the Safe Language as the command line flag takes precedence over a source level pragma.
- Ensure that all untrusted code is imported as a safe import and that the `-fpackage-trust` flag is used with packages from untrusted sources being marked as untrusted.

There is a more detailed discussion of the issues involved in compilation safety and some potential solutions on the GHC Wiki.

Additionally, the use of annotations is forbidden, as that would allow bypassing Safe Haskell restrictions. See #10826 for details.

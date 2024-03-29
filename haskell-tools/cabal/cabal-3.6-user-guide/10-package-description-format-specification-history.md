---
created: 2021-08-20T03:05:43 (UTC +02:00)
tags: []
source: https://cabal.readthedocs.io/en/latest/file-format-changelog.html
author: 
---

# 10. Package Description Format Specification History - Cabal 3.6.0.0 User's Guide

> ## Excerpt
> Package descriptions need to specify the version of the
specification they need to be interpreted in via the
cabal-version declaration. The following list describes
changes that occurred in each version of the cabal specification
relative to the respective preceding published version.

---
[Package descriptions][1] need to specify the version of the specification they need to be interpreted in via the [`cabal-version`][2] declaration. The following list describes changes that occurred in each version of the cabal specification relative to the respective preceding _published_ version.

Note

The sequence of specification version numbers is _not_ contiguous because it's synchronised with the version of the `Cabal` library. As a consequence, only _even_ versions are considered proper published versions of the specification as _odd_ versions of the `Cabal` library denote unreleased development branches which have no stability guarantee.

## 10.1. `cabal-version: 3.x`[¶][3]

-   Added fields [`extra-libraries-static`][4] and [`extra-lib-dirs-static`][5] to allow Haskell libraries to remember linker flags needed for fully static linking of system libraries into executables. The existing field [`pkgconfig-depends`][6] can used to append the relevant output of `pkg-config --libs --static` to these new fields automatically. When [`extra-libraries-static`][7] is not given, it defaults to [`extra-libraries`][8]. When [`extra-lib-dirs-static`][9] is not given, it defaults to [`extra-lib-dirs`][10].
    

## 10.2. `cabal-version: 3.6`[¶][11]

-   License fields use identifiers from SPDX License List version `3.10 2020-08-03`
    
-   Add [`hsc2hs-options`][12]
    

## 10.3. `cabal-version: 3.4`[¶][13]

-   License fields use identifiers from SPDX License List version `3.9 2020-05-15`
    
-   Dependencies to sublibraries have to be specified explicitly, even for current package. This way you can have an internal library with the same name as some external dependency.
    
-   Remove `-any` and `-none` syntax for version ranges Use `>=0` and `<0` respectively.
    
-   [`default-language`][14] is optional. The Default value is to use the compiler's default language.
    
-   [`mixins`][15] field allow specifying a sublibrary.
    

## 10.4. `cabal-version: 3.0`[¶][16]

-   Added the [`extra-dynamic-library-flavours`][17] field to specify non-trivial variants of dynamic flavours. It is [`extra-library-flavours`][18] but for shared libraries. Mainly useful for GHC's RTS library.
    
-   Free text fields (e.g. [`description`][19]) preserve empty lines and indentation. In other words, you don't need to add dots for blank lines.
    
-   License fields use identifiers from SPDX License List version `3.6 2019-07-10`
    
-   Remove deprecated `hs-source-dir`, [`extensions`][20] and [`build-tools`][21] fields.
    
-   Common stanzas are now allowed also in the beginning of conditional sections. In other words, the following is valid
    
    > library
    >     import deps
    > 
    >     if flag(foo)
    >         import foo-deps
    
-   Allow redundant leading or trailing commas in package fields with optional commas, such as [`library:exposed-modules`][22]
    
-   Require fields with optional commas to consistently omit or place commas between elements.
    
-   Changed the behavior of [`extra-bundled-libraries`][23] field. The naming convention of dynamic library files (e.g. generated by a custom build script) has changed. For library names prefixed with "C", the dynamic library file name(s) must be of the form lib<library-name>.<dyn-library-extension>\* instead of the old libC<library-name>-ghc<ghc-flavour><ghc-version>.<dyn-library-extension>
    
-   New set-notation syntax for `==` and `^>=` operators, see [`build-depends`][24] field documentation for examples.
    
-   Allow more whitespace in [`mixins`][25] field
    
-   Wildcards are disallowed in [`pkgconfig-depends`][26], Yet the pkgconfig format is relaxed to accept e.g. versions like `1.1.0h`.
    
-   New [`autogen-includes`][27] for specifying [`install-includes`][28] which are autogenerated (e.g. by a `configure` script).
    
-   New [`asm-sources`][29] and [`asm-options`][30] fields added for supporting bundled foreign routines implemented in assembler.
    
-   New [`cmm-sources`][31] and [`cmm-options`][32] fields added for supporting bundled foreign primops implemented in C-.
    

## 10.5. `cabal-version: 2.4`[¶][33]

-   Wildcard matching has been expanded. All previous wildcard expressions are still valid; some will match strictly more files than before. Specifically:
    
    -   Double-star (`**`) wildcards are now accepted for recursive matching immediately before the final slash; they must be followed by a filename wildcard (e.g., `foo/**/*.html` is valid; `foo/**/bar/*.html` and `foo/**/**/*.html`, `foo/**/bar.html` are all invalid). As `**` was an error in globs before, this does not affect any existing `.cabal` files that previously worked.
        
    -   Wildcards now match when the pattern's extensions form a suffix of the candidate file's extension, rather than requiring strict equality (e.g., previously `*.html` did not match `foo.en.html`, but now it does).
        
-   License fields use identifiers from SPDX License List version `3.2 2018-07-10`
    

## 10.6. `cabal-version: 2.2`[¶][34]

-   New [`common`][35] stanzas and [`import`][36] pseudo-field added.
    
-   New [`library:virtual-modules`][37] field added.
    
-   New [`cxx-sources`][38] and [`cxx-options`][39] fields added for suppporting bundled foreign routines implemented in C++.
    
-   New [`extra-bundled-libraries`][40] field for specifying additional custom library objects to be installed.
    
-   Extended `if` control structure with support for `elif` keyword.
    
-   Changed default rules of [`build-type`][41] field to infer "build-type:" for "Simple"/"Custom" automatically.
    
-   [`license`][42] field syntax changed to require SPDX expression syntax (using SPDX license list version `3.0 2017-12-28`).
    
-   Allow redundant leading or trailing commas in package fields (which require commas) such as [`build-depends`][43].
    

## 10.7. `cabal-version: 2.0`[¶][44]

-   New [`library:signatures`][45] and [`mixins`][46] fields added for supporting [Backpack][47].
    
-   New [`build-tool-depends`][48] field added for adding build-time dependencies of executable components.
    
-   New [`custom-setup:autogen-modules`][49] field added for declaring modules which are generated at build time.
    
-   Support for new [PVP][50] caret-style version operator (`^>=`) added to [`build-depends`][51].
    
-   Add support for new [`foreign-library`][52] stanza.
    
-   Add support for [internal library stanzas][53].
    
-   New CPP Macro `CURRENT_PACKAGE_VERSION`.
    

## 10.8. `cabal-version: 1.24`[¶][54]

-   New [`custom-setup`][55] stanza and [`custom-setup:setup-depends`][56] field added for specifying dependencies of custom `Setup.hs` scripts.
    
-   CPP Macros `VERSION_$pkgname` and `MIN_VERSION_$pkgname` are now also generated for the current package.
    
-   New CPP Macros `CURRENT_COMPONENT_ID` and `CURRENT_PACKAGE_KEY`.
    
-   New [`extra-framework-dirs`][57] field added for specifying extra locations to find OS X frameworks.
    

## 10.9. `cabal-version: 1.22`[¶][58]

-   New [`library:reexported-modules`][59] field.
    
-   Support for `-none` version constraint added to [`build-depends`][60].
    
-   New [`license`][61] type `ISC` added.
    

## 10.10. `cabal-version: 1.20`[¶][62]

-   Add support for new [`license-files`][63] field for declaring multiple license documents.
    
-   New CPP Macro `MIN_TOOL_VERSION_$buildtool`.
    
-   New [`license`][64] types `BSD2` and `MPL-2.0` added.
    

## 10.11. `cabal-version: 1.18`[¶][65]

-   Add support for new [`extra-doc-files`][66] field for specifying extra file assets referenced by the Haddock documentation.
    
-   New [`license`][67] type `AGPL` and `AGPL-3` added.
    
-   Add support for specifying a C/C++/obj-C source file in [`executable:main-is`][68] field.
    
-   Add `getSysconfDir` operation to `Paths_` API.
    

## 10.12. `cabal-version: 1.16`[¶][69]

Todo

this needs to be researched; there were only few changes between 1.12 and 1.18;

## 10.13. `cabal-version: 1.12`[¶][70]

-   Change syntax of [`cabal-version`][71] to support the new recommended `cabal-version: x.y` style
    

[1]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-desc
[2]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"
[3]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-3-x "Permalink to this headline"
[4]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-libraries-static "package.cabal extra-libraries-static field"
[5]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-lib-dirs-static "package.cabal extra-lib-dirs-static field"
[6]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-pkgconfig-depends "package.cabal pkgconfig-depends field"
[7]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-libraries-static "package.cabal extra-libraries-static field"
[8]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-libraries "package.cabal extra-libraries field"
[9]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-lib-dirs-static "package.cabal extra-lib-dirs-static field"
[10]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-lib-dirs "package.cabal extra-lib-dirs field"
[11]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-3-6 "Permalink to this headline"
[12]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hsc2hs-options "package.cabal hsc2hs-options field"
[13]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-3-4 "Permalink to this headline"
[14]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-language "package.cabal default-language field(since version: 1.12)"
[15]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "package.cabal mixins field(since version: 2.0)"
[16]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-3-0 "Permalink to this headline"
[17]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-dynamic-library-flavours "package.cabal extra-dynamic-library-flavours field"
[18]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-library-flavours "package.cabal extra-library-flavours field"
[19]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-description "package.cabal description field"
[20]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extensions "package.cabal extensions field(removed in: 3.0; deprecated since: 1.12)"
[21]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[22]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-exposed-modules "package.cabal library section exposed-modules: field"
[23]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-bundled-libraries "package.cabal extra-bundled-libraries field(since version: 2.2)"
[24]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[25]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "package.cabal mixins field(since version: 2.0)"
[26]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-pkgconfig-depends "package.cabal pkgconfig-depends field"
[27]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-includes "package.cabal autogen-includes field(since version: 3.0)"
[28]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[29]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-asm-sources "package.cabal asm-sources field(since version: 3.0)"
[30]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-asm-options "package.cabal asm-options field(since version: 3.0)"
[31]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cmm-sources "package.cabal cmm-sources field(since version: 3.0)"
[32]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cmm-options "package.cabal cmm-options field(since version: 3.0)"
[33]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-2-4 "Permalink to this headline"
[34]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-2-2 "Permalink to this headline"
[35]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-common-common "package.cabal common section (since version: 2.2)"
[36]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-import "package.cabal import field"
[37]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-virtual-modules "package.cabal library section virtual-modules: field(since version: 2.2)"
[38]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-sources "package.cabal cxx-sources field(since version: 2.2)"
[39]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-options "package.cabal cxx-options field(since version: 2.2)"
[40]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-bundled-libraries "package.cabal extra-bundled-libraries field(since version: 2.2)"
[41]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "package.cabal build-type field"
[42]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license "package.cabal license field"
[43]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[44]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-2-0 "Permalink to this headline"
[45]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures "package.cabal library section signatures: field(since version: 2.0)"
[46]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "package.cabal mixins field(since version: 2.0)"
[47]: https://cabal.readthedocs.io/en/latest/cabal-package.html#backpack
[48]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[49]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-modules "package.cabal autogen-modules field(since version: 2.0)"
[50]: http://pvp.haskell.org/
[51]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[52]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-foreign-library-foreign-library "package.cabal foreign-library section (since version: 2.0)"
[53]: https://cabal.readthedocs.io/en/latest/cabal-package.html#sublibs
[54]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-1-24 "Permalink to this headline"
[55]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-custom-setup-custom-setup "package.cabal custom-setup section (since version: 1.24)"
[56]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-custom-setup-setup-depends "package.cabal custom-setup section setup-depends: field(since version: 1.24)"
[57]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-framework-dirs "package.cabal extra-framework-dirs field(since version: 1.24)"
[58]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-1-22 "Permalink to this headline"
[59]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-reexported-modules "package.cabal library section reexported-modules: field(since version: 1.22)"
[60]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[61]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license "package.cabal license field"
[62]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-1-20 "Permalink to this headline"
[63]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license-files "package.cabal license-files field(since version: 1.20)"
[64]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license "package.cabal license field"
[65]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-1-18 "Permalink to this headline"
[66]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-doc-files "package.cabal extra-doc-files field(since version: 1.18)"
[67]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license "package.cabal license field"
[68]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-executable-main-is "package.cabal executable section main-is: field"
[69]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-1-16 "Permalink to this headline"
[70]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#cabal-version-1-12 "Permalink to this headline"
[71]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"

---
created: 2021-08-20T03:03:12 (UTC +02:00)
tags: []
source: https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html
author: 
---

# 11. Field Syntax Reference — Cabal 3.6.0.0 User's Guide

> ## Excerpt
> Field syntax is described as they are in the latest cabal file format version.

---
## 11.1. Notation[¶][1]

Field syntax is described as they are in the latest cabal file format version.

-   terminals are enclosed in quotes and type set in typewriter script:
    
-   non-terminals are type set in italic:
    
-   character sets are type set resembling regular expression notation:
    
    Character set complements have c superscript:
    
-   repetition is type set using regular expression inspired notation. Superscripts tell how many time to repeat: The generic notation is ∈\[n…5\], however there are common shorthands: ∗ for ∈\[0…∞\] (`many`), + for ∈\[1…∞\] (`some`), ? for ∈\[0…1\] (`optional`).
    
    Subscripts tell the used separator:
    
    Would be `digit(\.digit)*` in common regex syntax.
    
-   alternatives are listed in braces separated by vertical bar:
    
    In case of multiple alternatives, the stacked notation is used
    
-   parenthesis are used only for grouping:
    
-   any amount of spaces, and at least single space are type set using ∘ and ∙ respectively. They may appear standalone, not only as binary operators.
    
-   While notation is heavily regular expression inspired, there are also fixed points, which allow represent recursive grammars
    
    fixexprindigit∣expr∘‘‘+"∘expr∣‘‘("∘expr∘‘‘)"
    

## 11.2. Lists[¶][2]

Many fields in cabal file format are lists. There are three variations:

Space separated

Are used for lists of things with simple grammars, for example [`ghc-options`][3]

Comma separated

Are used for lists of things with complicated grammars, for example [`build-depends`][4] There can be leading or trailing comma (but not both) since `cabal-version: 2.2`. Note, the comma cannot exist alone.

commalist(element)\={element(∘‘‘,"∘)∗∣‘‘,"∘element(∘‘‘,"∘)+∣element(∘‘‘,"∘)+∘‘‘,"}

Optional comma separated

Surprisingly many fields can have optional comma separator. Since `cabal-version: 3.0` comma usage have to be consistent, in other words either used everywhere or nowhere. It’s recommended to avoid using comma in these fields, an example field is [`default-extensions`][5].

optcommalist(element)\={element∙∗element(∘‘‘,"∘)∗‘‘,"∘element(∘‘‘,"∘)+element(∘‘‘,"∘)+∘‘‘,"}

## 11.3. Non-terminals[¶][6]

In the syntax definitions below the following non-terminal symbols are used:

hs-string

String as in Haskell; it’s recommended to avoid using Haskell-specific escapes.

‘‘""⁡{\[‘‘""⁡‘‘\\"\]c∣{‘‘\\&"‘‘\\\\"{‘‘\\n"∣escapes}‘‘\\"⁡\[‘‘0"⁡⋯‘‘9"\]‘‘\\o"⁡\[‘‘0"⁡⋯‘‘7"\]‘‘\\x"⁡\[‘‘0"⁡⋯‘‘9"⁡‘‘A"⁡⋯‘‘F"⁡‘‘a"⁡⋯‘‘f"\]{‘‘\\^@"∣control}{‘‘\\NUL"∣ascii}}}∗‘‘""

unqual-name

Unqualified component names are used for package names, component names etc. but not flag names. Unqualified component name consist of components separated by dash, each component is non-empty alphanumeric string, with at least one alphabetic character. In other words, component may not look like a number.

(alpha\-num∗alpha⁡alpha\-num∗)‘‘\-"+

module-name

Haskell module name as recognized by Cabal parser.

(upper⁡{alpha\-num∣\[‘‘'"⁡‘‘\_"\]}∗)‘‘."+

version

Version is to first approximation numbers separated by dots, where leading zero is not allowed and each version digit is consists at most of nine characters.

{‘‘0"∣\[‘‘1"⁡⋯‘‘9"\]\[‘‘0"⁡⋯‘‘9"\]∈\[0…8\]}‘‘."+

version-range

Version range syntax is recursive. Also note the set syntax added in `cabal-version: 3.0`, set cannot be empty.

fixversion\-rangein{‘‘\=\="∘version‘‘\>"∘version‘‘<"∘version‘‘<\="∘version‘‘\>\="∘version‘‘^\>\="∘version‘‘\=\="∘{‘‘0"∣\[‘‘1"⁡⋯‘‘9"\]\[‘‘0"⁡⋯‘‘9"\]∈\[0…8\]}‘‘."+‘‘.\*"version\-range∘‘‘||"∘version\-rangeversion\-range∘‘‘&&"∘version\-range‘‘("∘version\-range∘‘‘)"‘‘\=\="∘‘‘{"∘version(∘‘‘,"∘)+∘‘‘}"‘‘^\>\="∘‘‘{"∘version(∘‘‘,"∘)+∘‘‘}"}

## 11.4. Build info fields[¶][7]

asm-options

-   Monoidal field
    
-   Available since `cabal-version: 3.0`.
    
-   Documentation of [`asm-options`][8]
    

asm-sources

-   Monoidal field
    
-   Available since `cabal-version: 3.0`.
    
-   Documentation of [`asm-sources`][9]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

autogen-includes

-   Monoidal field
    
-   Available since `cabal-version: 3.0`.
    
-   Documentation of [`autogen-includes`][10]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

autogen-modules

-   Monoidal field
    
-   Available since `cabal-version: 2.0`.
    
-   Documentation of [`autogen-modules`][11]
    

commalist((upper⁡{alpha\-num∣\[‘‘'"⁡‘‘\_"\]}∗)‘‘."+)

build-depends

-   Monoidal field
    
-   Documentation of [`build-depends`][12]
    

commalist(pkg\-name⁡(‘‘:"⁡{unqual\-name∣‘‘{"∘unqual\-name(∘‘‘,"∘)+∘‘‘}"})?(∘version\-range)?)

build-tool-depends

-   Monoidal field
    
-   Documentation of [`build-tool-depends`][13]
    

build-tools

-   Monoidal field
    
-   Deprecated since `cabal-version: 2.0`: Please use ‘build-tool-depends’ field
    
-   Removed in `cabal-version: 3.0`: Please use ‘build-tool-depends’ field.
    

buildable

-   Boolean field
    
-   Default: `True`
    
-   Documentation of [`buildable`][14]
    

c-sources

-   Monoidal field
    
-   Documentation of [`c-sources`][15]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

cc-options

-   Monoidal field
    
-   Documentation of [`cc-options`][16]
    

cmm-options

-   Monoidal field
    
-   Available since `cabal-version: 3.0`.
    
-   Documentation of [`cmm-options`][17]
    

cmm-sources

-   Monoidal field
    
-   Available since `cabal-version: 3.0`.
    
-   Documentation of [`cmm-sources`][18]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

cpp-options

-   Monoidal field
    
-   Documentation of [`cpp-options`][19]
    

cxx-options

-   Monoidal field
    
-   Available since `cabal-version: 2.2`.
    
-   Documentation of [`cxx-options`][20]
    

cxx-sources

-   Monoidal field
    
-   Available since `cabal-version: 2.2`.
    
-   Documentation of [`cxx-sources`][21]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

default-extensions

-   Monoidal field
    
-   Available since `cabal-version: 1.10`.
    
-   Documentation of [`default-extensions`][22]
    

default-language

-   Optional field
    
-   Available since `cabal-version: 1.10`.
    
-   Documentation of [`default-language`][23]
    

{‘‘Haskell98"∣‘‘Haskell2010"}

extensions

-   Monoidal field
    
-   Deprecated since `cabal-version: 1.12`: Please use ‘default-extensions’ or ‘other-extensions’ fields.
    
-   Removed in `cabal-version: 3.0`: Please use ‘default-extensions’ or ‘other-extensions’ fields.
    

extra-bundled-libraries

-   Monoidal field
    
-   Documentation of [`extra-bundled-libraries`][24]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-dynamic-library-flavours

-   Monoidal field
    
-   Available since `cabal-version: 3.0`.
    
-   Documentation of [`extra-dynamic-library-flavours`][25]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-framework-dirs

-   Monoidal field
    
-   Documentation of [`extra-framework-dirs`][26]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-ghci-libraries

-   Monoidal field
    
-   Documentation of [`extra-ghci-libraries`][27]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-lib-dirs

-   Monoidal field
    
-   Documentation of [`extra-lib-dirs`][28]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-lib-dirs-static

-   Monoidal field
    
-   Documentation of [`extra-lib-dirs-static`][29]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-libraries

-   Monoidal field
    
-   Documentation of [`extra-libraries`][30]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-library-flavours

-   Monoidal field
    
-   Documentation of [`extra-library-flavours`][31]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-libraries

-   Monoidal field
    
-   Documentation of [`extra-libraries-static`][32]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

frameworks

-   Monoidal field
    
-   Documentation of [`frameworks`][33]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

ghc-options

-   Monoidal field
    
-   Documentation of [`ghc-options`][34]
    

ghc-prof-options

-   Monoidal field
    
-   Documentation of [`ghc-prof-options`][35]
    

ghc-shared-options

-   Monoidal field
    
-   Documentation of [`ghc-shared-options`][36]
    

ghcjs-options

-   Monoidal field
    
-   Documentation of [`ghcjs-options`][37]
    

ghcjs-prof-options

-   Monoidal field
    
-   Documentation of [`ghcjs-prof-options`][38]
    

ghcjs-shared-options

-   Monoidal field
    
-   Documentation of [`ghcjs-shared-options`][39]
    

hs-source-dir

-   Monoidal field
    
-   Deprecated since `cabal-version: 1.2`: Please use ‘hs-source-dirs’
    
-   Removed in `cabal-version: 3.0`: Please use ‘hs-source-dirs’ field.
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

hs-source-dirs

-   Monoidal field
    
-   Documentation of [`hs-source-dirs`][40]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

include-dirs

-   Monoidal field
    
-   Documentation of [`include-dirs`][41]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

includes

-   Monoidal field
    
-   Documentation of [`includes`][42]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

install-includes

-   Monoidal field
    
-   Documentation of [`install-includes`][43]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

js-sources

-   Monoidal field
    
-   Documentation of [`js-sources`][44]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

ld-options

-   Monoidal field
    
-   Documentation of [`ld-options`][45]
    

mixins

-   Monoidal field
    
-   Available since `cabal-version: 2.0`.
    
-   Documentation of [`mixins`][46]
    

commalist(package\-name⁡(‘‘:"⁡library\-name)?(∙{∣‘‘hiding"∘‘‘("∘module\-name(∘‘‘,"∘)∗∘‘‘)"∣‘‘("∘(module\-name⁡(∙‘‘as"∙module\-name)?)(∘‘‘,"∘)∗∘‘‘)"}(∘‘‘requires"∙{∣‘‘hiding"∘‘‘("∘module\-name(∘‘‘,"∘)∗∘‘‘)"∣‘‘("∘(module\-name⁡(∙‘‘as"∙module\-name)?)(∘‘‘,"∘)∗∘‘‘)"})?)?)

other-extensions

-   Monoidal field
    
-   Available since `cabal-version: 1.10`.
    
-   Documentation of [`other-extensions`][47]
    

other-languages

-   Monoidal field
    
-   Available since `cabal-version: 1.10`.
    
-   Documentation of [`other-languages`][48]
    

optcommalist{‘‘Haskell98"∣‘‘Haskell2010"}

other-modules

-   Monoidal field
    
-   Documentation of [`other-modules`][49]
    

commalist((upper⁡{alpha\-num∣\[‘‘'"⁡‘‘\_"\]}∗)‘‘."+)

pkgconfig-depends

-   Monoidal field
    
-   Documentation of [`pkgconfig-depends`][50]
    

virtual-modules

-   Monoidal field
    
-   Available since `cabal-version: 2.2`.
    
-   Documentation of [`virtual-modules`][51]
    

commalist((upper⁡{alpha\-num∣\[‘‘'"⁡‘‘\_"\]}∗)‘‘."+)

## 11.5. Package description fields[¶][52]

author

-   Free text field
    
-   Documentation of [`author`][53]
    

bug-reports

-   Free text field
    
-   Documentation of [`bug-reports`][54]
    

build-type

-   Optional field
    
-   Documentation of [`build-type`][55]
    

{‘‘Simple"‘‘Configure"‘‘Custom"‘‘Make"‘‘Default"}

cabal-version

-   Optional field
    
-   Default: `>=1.0`
    
-   Documentation of [`cabal-version`][56]
    

category

-   Free text field
    
-   Documentation of [`category`][57]
    

copyright

-   Free text field
    
-   Documentation of [`copyright`][58]
    

data-dir

-   Optional field
    
-   Default: `""`
    
-   Documentation of [`data-dir`][59]
    

{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

data-files

-   Monoidal field
    
-   Documentation of [`data-files`][60]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

description

-   Free text field
    
-   Documentation of [`description`][61]
    

extra-doc-files

-   Monoidal field
    
-   Documentation of [`extra-doc-files`][62]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-source-files

-   Monoidal field
    
-   Documentation of [`extra-source-files`][63]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

extra-tmp-files

-   Monoidal field
    
-   Documentation of [`extra-tmp-files`][64]
    

commalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

homepage

-   Free text field
    
-   Documentation of [`homepage`][65]
    

license

-   Optional field
    
-   Default: `NONE`
    
-   Documentation of [`license`][66]
    

license-file

-   Monoidal field
    
-   Documentation of [`license-file`][67]
    

optcommalist{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

maintainer

-   Free text field
    
-   Documentation of [`maintainer`][68]
    

name

-   Required field
    
-   Documentation of [`name`][69]
    

package-url

-   Free text field
    
-   Documentation of [`package-url`][70]
    

stability

-   Free text field
    
-   Documentation of [`stability`][71]
    

synopsis

-   Free text field
    
-   Documentation of [`synopsis`][72]
    

tested-with

-   Monoidal field
    
-   Documentation of [`tested-with`][73]
    

version

-   Required field
    
-   Documentation of [`version`][74]
    

{‘‘0"∣\[‘‘1"⁡⋯‘‘9"\]\[‘‘0"⁡⋯‘‘9"\]∈\[0…8\]}‘‘."+

## 11.6. Test-suite fields[¶][75]

main-is

-   Optional field
    
-   Documentation of [`test-suite:main-is`][76]
    

{hs\-string∣\[‘‘ "⁡‘‘,"\]c+}

test-module

-   Optional field
    
-   Documentation of [`test-suite:test-module`][77]
    

(upper⁡{alpha\-num∣\[‘‘'"⁡‘‘\_"\]}∗)‘‘."+

type

-   Optional field
    
-   Documentation of [`test-suite:type`][78]
    

{‘‘exitcode\-stdio\-1.0"∣‘‘detailed\-0.9"}

[1]: https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html#notation "Permalink to this headline"
[2]: https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html#lists "Permalink to this headline"
[3]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-options "package.cabal ghc-options field"
[4]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[5]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-extensions "package.cabal default-extensions field(since version: 1.12)"
[6]: https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html#non-terminals "Permalink to this headline"
[7]: https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html#build-info-fields "Permalink to this headline"
[8]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-asm-options "package.cabal asm-options field(since version: 3.0)"
[9]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-asm-sources "package.cabal asm-sources field(since version: 3.0)"
[10]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-includes "package.cabal autogen-includes field(since version: 3.0)"
[11]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-modules "package.cabal autogen-modules field(since version: 2.0)"
[12]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[13]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[14]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-buildable "package.cabal buildable field"
[15]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-c-sources "package.cabal c-sources field"
[16]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cc-options "package.cabal cc-options field"
[17]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cmm-options "package.cabal cmm-options field(since version: 3.0)"
[18]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cmm-sources "package.cabal cmm-sources field(since version: 3.0)"
[19]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cpp-options "package.cabal cpp-options field"
[20]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-options "package.cabal cxx-options field(since version: 2.2)"
[21]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-sources "package.cabal cxx-sources field(since version: 2.2)"
[22]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-extensions "package.cabal default-extensions field(since version: 1.12)"
[23]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-language "package.cabal default-language field(since version: 1.12)"
[24]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-bundled-libraries "package.cabal extra-bundled-libraries field(since version: 2.2)"
[25]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-dynamic-library-flavours "package.cabal extra-dynamic-library-flavours field"
[26]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-framework-dirs "package.cabal extra-framework-dirs field(since version: 1.24)"
[27]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-ghci-libraries "package.cabal extra-ghci-libraries field"
[28]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-lib-dirs "package.cabal extra-lib-dirs field"
[29]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-lib-dirs-static "package.cabal extra-lib-dirs-static field"
[30]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-libraries "package.cabal extra-libraries field"
[31]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-library-flavours "package.cabal extra-library-flavours field"
[32]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-libraries-static "package.cabal extra-libraries-static field"
[33]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-frameworks "package.cabal frameworks field"
[34]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-options "package.cabal ghc-options field"
[35]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-prof-options "package.cabal ghc-prof-options field"
[36]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-shared-options "package.cabal ghc-shared-options field"
[37]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghcjs-options "package.cabal ghcjs-options field"
[38]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghcjs-prof-options "package.cabal ghcjs-prof-options field"
[39]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghcjs-shared-options "package.cabal ghcjs-shared-options field"
[40]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hs-source-dirs "package.cabal hs-source-dirs field"
[41]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-include-dirs "package.cabal include-dirs field"
[42]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-includes "package.cabal includes field"
[43]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[44]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-js-sources "package.cabal js-sources field"
[45]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ld-options "package.cabal ld-options field"
[46]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "package.cabal mixins field(since version: 2.0)"
[47]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-extensions "package.cabal other-extensions field(since version: 1.12)"
[48]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-languages "package.cabal other-languages field(since version: 1.12)"
[49]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-modules "package.cabal other-modules field"
[50]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-pkgconfig-depends "package.cabal pkgconfig-depends field"
[51]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-virtual-modules "package.cabal virtual-modules field(since version: 2.2)"
[52]: https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html#package-description-fields "Permalink to this headline"
[53]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-author "package.cabal author field"
[54]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-bug-reports "package.cabal bug-reports field"
[55]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "package.cabal build-type field"
[56]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"
[57]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-category "package.cabal category field"
[58]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-copyright "package.cabal copyright field"
[59]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-dir "package.cabal data-dir field"
[60]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-files "package.cabal data-files field"
[61]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-description "package.cabal description field"
[62]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-doc-files "package.cabal extra-doc-files field(since version: 1.18)"
[63]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-source-files "package.cabal extra-source-files field"
[64]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-tmp-files "package.cabal extra-tmp-files field"
[65]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-homepage "package.cabal homepage field"
[66]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license "package.cabal license field"
[67]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license-file "package.cabal license-file field"
[68]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-maintainer "package.cabal maintainer field"
[69]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-name "package.cabal name field"
[70]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-package-url "package.cabal package-url field"
[71]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-stability "package.cabal stability field"
[72]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-synopsis "package.cabal synopsis field"
[73]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-tested-with "package.cabal tested-with field"
[74]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-version "package.cabal version field"
[75]: https://cabal.readthedocs.io/en/latest/buildinfo-fields-reference.html#test-suite-fields "Permalink to this headline"
[76]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-test-suite-main-is "package.cabal test-suite section main-is: field"
[77]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-test-suite-test-module "package.cabal test-suite section test-module: field"
[78]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-test-suite-type "package.cabal test-suite section type: field"

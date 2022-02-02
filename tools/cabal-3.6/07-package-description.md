---
created: 2021-08-20T03:05:26 (UTC +02:00)
tags: []
source: https://cabal.readthedocs.io/en/latest/cabal-package.html
author: author: freeform¶
---

# 7. Package Description — Cabal 3.6.0.0 User's Guide

> ## Excerpt
> The Cabal package is the unit of distribution. When installed, its
purpose is to make available:

---
The Cabal package is the unit of distribution. When installed, its purpose is to make available:

-   One or more Haskell programs.
    
-   At most one library, exposing a number of Haskell modules.
    

However having both a library and executables in a package does not work very well; if the executables depend on the library, they must explicitly list all the modules they directly or indirectly import from that library. Fortunately, starting with Cabal 1.8.0.4, executables can also declare the package that they are in as a dependency, and Cabal will treat them as if they were in another package that depended on the library.

Internally, the package may consist of much more than a bunch of Haskell modules: it may also have C source code and header files, source code meant for preprocessing, documentation, test cases, auxiliary tools etc.

A package is identified by a globally-unique _package name_, which consists of one or more alphanumeric words separated by hyphens. To avoid ambiguity, each of these words should contain at least one letter. Chaos will result if two distinct packages with the same name are installed on the same system. A particular version of the package is distinguished by a _version number_, consisting of a sequence of one or more integers separated by dots. These can be combined to form a single text string called the _package ID_, using a hyphen to separate the name from the version, e.g. “`HUnit-1.1`”.

Note

Packages are not part of the Haskell language; they simply populate the hierarchical space of module names. In GHC 6.6 and later a program may contain multiple modules with the same name if they come from separate packages; in all other current Haskell systems packages may not overlap in the modules they provide, including hidden modules.

## 7.1. Creating a package[¶][1]

Suppose you have a directory hierarchy containing the source files that make up your package. You will need to add two more files to the root directory of the package:

`_package-name_.cabal`

a Unicode UTF-8 text file containing a package description. For details of the syntax of this file, see the section on [package descriptions][2].

`Setup.hs`

a single-module Haskell program to perform various setup tasks (with the interface described in the section on [Building and installing packages][3]). This module should import only modules that will be present in all Haskell implementations, including modules of the Cabal library. The content of this file is determined by the [`build-type`][4] setting in the `.cabal` file. In most cases it will be trivial, calling on the Cabal library to do most of the work.

Once you have these, you can create a source bundle of this directory for distribution. Building of the package is discussed in the section on [Building and installing packages][5].

One of the purposes of Cabal is to make it easier to build a package with different Haskell implementations. So it provides abstractions of features present in different Haskell implementations and wherever possible it is best to take advantage of these to increase portability. Where necessary however it is possible to use specific features of specific implementations. For example one of the pieces of information a package author can put in the package’s `.cabal` file is what language extensions the code uses. This is far preferable to specifying flags for a specific compiler as it allows Cabal to pick the right flags for the Haskell implementation that the user picks. It also allows Cabal to figure out if the language extension is even supported by the Haskell implementation that the user picks. Where compiler-specific options are needed however, there is an “escape hatch” available. The developer can specify implementation-specific options and more generally there is a configuration mechanism to customise many aspects of how a package is built depending on the Haskell implementation, the Operating system, computer architecture and user-specified configuration flags.

name:     Foo
version:  1.0

library
 build-depends:   base \>= 4 && < 5
 exposed-modules: Foo
 extensions:      ForeignFunctionInterface
 ghc-options:     -Wall
  if os(windows)
 build-depends: Win32 \>= 2.1 && < 2.6

### 7.1.1. Example: A package containing a simple library[¶][6]

The HUnit package contains a file `HUnit.cabal` containing:

name:           HUnit
version:        1.1.1
synopsis:       A unit testing framework for Haskell
homepage:       http://hunit.sourceforge.net/
category:       Testing
author:         Dean Herington
license:        BSD3
license-file:   LICENSE
cabal-version:  1.12
build-type:     Simple

library
 build-depends:      base \>= 2 && < 4
 exposed-modules:    Test.HUnit.Base, Test.HUnit.Lang,
                      Test.HUnit.Terminal, Test.HUnit.Text, Test.HUnit
 default-extensions: CPP

and the following `Setup.hs`:

import Distribution.Simple
main \= defaultMain

### 7.1.2. Example: A package containing executable programs[¶][7]

name:           TestPackage
version:        0.0
synopsis:       Small package with two programs
author:         Angela Author
license:        BSD3
build-type:     Simple
cabal-version:  \>= 1.8

executable program1
 build-depends:  HUnit \>= 1.1.1 && < 1.2
 main-is:        main.hs
 hs-source-dirs: prog1

executable program2
 \-- A different main.hs because of hs-source-dirs.
 main-is:        main.hs
 build-depends:  HUnit \>= 1.1.1 && < 1.2
 hs-source-dirs: prog2
 other-modules:  Utils

with `Setup.hs` the same as above.

### 7.1.3. Example: A package containing a library and executable programs[¶][8]

name:            TestPackage
version:         0.0
synopsis:        Package with library and two programs
license:         BSD3
author:          Angela Author
build-type:      Simple
cabal-version:   \>= 1.8

library
 build-depends:   HUnit \>= 1.1.1 && < 1.2
 hs-source-dirs:  lib
 exposed-modules: A, B, C

executable program1
 main-is:         main.hs
 hs-source-dirs:  prog1
 other-modules:   D, E

executable program2
 \-- A different main.hs because of hs-source-dirs.
 main-is:         main.hs
 \-- No bound on internal libraries.
 build-depends:   TestPackage
 hs-source-dirs:  prog2
 other-modules:   Utils

with `Setup.hs` the same as above. Note that any library modules required (directly or indirectly) by an executable must be listed again.

The trivial setup script used in these examples uses the _simple build infrastructure_ provided by the Cabal library (see [Distribution.Simple][9]). The simplicity lies in its interface rather that its implementation. It automatically handles preprocessing with standard preprocessors, and builds packages for all the Haskell implementations.

The simple build infrastructure can also handle packages where building is governed by system-dependent parameters, if you specify a little more (see the section on [system-dependent parameters][10]). A few packages require [more elaborate solutions][11].

## 7.2. Package descriptions[¶][12]

The package description file must have a name ending in “`.cabal`”. It must be a Unicode text file encoded using valid UTF-8. There must be exactly one such file in the directory. The first part of the name is usually the package name, and some of the tools that operate on Cabal packages require this; specifically, Hackage rejects packages which don’t follow this rule.

In the package description file, lines whose first non-whitespace characters are “`--`” are treated as comments and ignored.

This file should contain a number global property descriptions and several sections.

-   The [package properties][13] describe the package as a whole, such as name, license, author, etc.
    
-   Optionally, a number of _configuration flags_ can be declared. These can be used to enable or disable certain features of a package. (see the section on [configurations][14]).
    
-   The (optional) library section specifies the [library][15] properties and relevant [build information][16].
    
-   Following is an arbitrary number of executable sections which describe an executable program and relevant [build information][17].
    

Each section consists of a number of property descriptions in the form of field/value pairs, with a syntax roughly like mail message headers.

-   Case is not significant in field names, but is significant in field values.
    
-   To continue a field value, indent the next line relative to the field name.
    
-   Field names may be indented, but all field values in the same section must use the same indentation.
    
-   Tabs are _not_ allowed as indentation characters due to a missing standard interpretation of tab width.
    
-   Before Cabal 3.0, to get a blank line in a field value, use an indented “`.`”
    

The syntax of the value depends on the field. Field types include:

_token_, _filename_, _directory_

Either a sequence of one or more non-space non-comma characters, or a quoted string in Haskell 98 lexical syntax. The latter can be used for escaping whitespace, for example: `ghc-options: -Wall "-with-rtsopts=-T -I1"`. Unless otherwise stated, relative filenames and directories are interpreted from the package root directory.

_freeform_, _URL_, _address_

An arbitrary, uninterpreted string.

_identifier_

A letter followed by zero or more alphanumerics or underscores.

_compiler_

A compiler flavor (one of: `GHC`, `UHC` or `LHC`) followed by a version range. For example, `GHC ==6.10.3`, or `LHC >=0.6 && <0.8`.

### 7.2.1. Modules and preprocessors[¶][18]

Haskell module names listed in the [`library:exposed-modules`][19] and [`library:other-modules`][20] fields may correspond to Haskell source files, i.e. with names ending in “`.hs`” or “`.lhs`”, or to inputs for various Haskell preprocessors. The simple build infrastructure understands the extensions:

-   `.gc` ([greencard][21])
    
-   `.chs` ([c2hs][22])
    
-   `.hsc` ([hsc2hs][23])
    
-   `.y` and `.ly` ([happy][24])
    
-   `.x` ([alex][25])
    
-   `.cpphs` ([cpphs][26])
    

When building, Cabal will automatically run the appropriate preprocessor and compile the Haskell module it produces. For the `c2hs` and `hsc2hs` preprocessors, Cabal will also automatically add, compile and link any C sources generated by the preprocessor (produced by `hsc2hs`’s `#def` feature or `c2hs`’s auto-generated wrapper functions). Dependencies on pre-processors are specified via the [`build-tools`][27] or [`build-tool-depends`][28] fields.

Some fields take lists of values, which are optionally separated by commas, except for the [`build-depends`][29] field, where the commas are mandatory.

Some fields are marked as required. All others are optional, and unless otherwise specified have empty default values.

### 7.2.2. Package properties[¶][30]

These fields may occur in the first top-level properties section and describe the package as a whole:

`name``:` _package-name (required)_[¶][31]

The unique name of the package, without the version number.

As pointed out in the section on [package descriptions][32], some tools require the package-name specified for this field to match the package description’s file-name `_package-name_.cabal`.

Package names are case-sensitive and must match the regular expression (i.e. alphanumeric “words” separated by dashes; each alphanumeric word must contain at least one letter): `[[:digit:]]*[[:alpha:]][[:alnum:]]*(-[[:digit:]]*[[:alpha:]][[:alnum:]]*)*`.

Or, expressed in [ABNF][33]:

package-name      \= package-name-part \*("-" package-name-part)
package-name-part \= \*DIGIT UALPHA \*UALNUM

UALNUM \= UALPHA / DIGIT
UALPHA \= ... ; set of alphabetic Unicode code-points

Note

Hackage restricts package names to the ASCII subset.

`version``:` _numbers (required)_[¶][34]

The package version number, usually consisting of a sequence of natural numbers separated by dots, i.e. as the regular expression `[0-9]+([.][0-9]+)*` or expressed in [ABNF][35]:

package-version \= 1\*DIGIT \*("." 1\*DIGIT)

`cabal-version``:` _x.y\[.z\]_[¶][36]

The version of the Cabal specification that this package description uses. The Cabal specification does slowly evolve (see also [Package Description Format Specification History][37]), introducing new features and occasionally changing the meaning of existing features. Specifying which version of the specification you are using enables programs which process the package description to know what syntax to expect and what each part means.

The version number you specify will affect both compatibility and behaviour. Most tools (including the Cabal library and the `cabal` program) understand a range of versions of the Cabal specification. Older tools will of course only work with older versions of the Cabal specification that was known at the time. Most of the time, tools that are too old will recognise this fact and produce a suitable error message. Likewise, `cabal check` will tell you whether the version number is sufficiently high for the features you use in the package description.

As for behaviour, new versions of the Cabal specification can change the meaning of existing syntax. This means if you want to take advantage of the new meaning or behaviour then you must specify the newer Cabal version. Tools are expected to use the meaning and behaviour appropriate to the version given in the package description.

In particular, the syntax of package descriptions changed significantly with Cabal version 1.2 and the [`cabal-version`][38] field is now required. Files written in the old syntax are still recognized, so if you require compatibility with very old Cabal versions then you may write your package description file using the old syntax. Please consult the user’s guide of an older Cabal version for a description of that syntax.

Starting with `cabal-version: 2.2` this field is only valid if fully contained in the very first line of a package description and ought to adhere to the [ABNF][39] grammar

newstyle-spec-version-decl \= "cabal-version" \*WS ":" \*WS newstyle-spec-version \*WS

newstyle-spec-version      \= NUM "." NUM \[ "." NUM \]

NUM    \= DIGIT0 / DIGITP 1\*DIGIT0
DIGIT0 \= %x30-39
DIGITP \= %x31-39
WS     \= %20

Note

For package descriptions using a format prior to `cabal-version: 1.12` the legacy syntax resembling a version range syntax

needs to be used.

This legacy syntax is supported up until `cabal-version: >= 2.0` it is however strongly recommended to avoid using the legacy syntax. See also [#4899][40].

`build-type``:` _identifier_[¶][41]

Default value

`Custom` or `Simple`

The type of build used by this package. Build types are the constructors of the [BuildType][42] type. This field is optional and when missing, its default value is inferred according to the following rules:

> -   When [`cabal-version`][43] is set to `2.2` or higher, the default is `Simple` unless a [`custom-setup`][44] exists, in which case the inferred default is `Custom`.
>     
> -   For lower [`cabal-version`][45] values, the default is `Custom` unconditionally.
>     

If the build type is anything other than `Custom`, then the `Setup.hs` file _must_ be exactly the standardized content discussed below. This is because in these cases, `cabal` will ignore the `Setup.hs` file completely, whereas other methods of package management, such as `runhaskell Setup.hs [CMD]`, still rely on the `Setup.hs` file.

For build type `Simple`, the contents of `Setup.hs` must be:

import Distribution.Simple
main \= defaultMain

For build type `Configure` (see the section on [system-dependent parameters][46] below), the contents of `Setup.hs` must be:

import Distribution.Simple
main \= defaultMainWithHooks autoconfUserHooks

For build type `Make` (see the section on [more complex packages][47] below), the contents of `Setup.hs` must be:

import Distribution.Make
main \= defaultMain

For build type `Custom`, the file `Setup.hs` can be customized, and will be used both by `cabal` and other tools.

For most packages, the build type `Simple` is sufficient.

`license``:` _SPDX expression_[¶][48]

Default value

`NONE`

The type of license under which this package is distributed.

Starting with `cabal-version: 2.2` the `license` field takes a (case-sensitive) SPDX expression such as

license: Apache-2.0 AND (MIT OR GPL-2.0-or-later)

See [SPDX IDs: How to use][49] for more examples of SPDX expressions.

The version of the [list of SPDX license identifiers][50] is a function of the [`cabal-version`][51] value as defined in the following table:

  

Cabal specification version

SPDX license list version

`cabal-version: 2.2`

`3.0 2017-12-28`

`cabal-version: 2.4`

`3.2 2018-07-10`

**Pre-SPDX Legacy Identifiers**

The license identifier in the table below are defined for `cabal-version: 2.0` and previous versions of the Cabal specification.

  

[`license`][52] identifier

Note

`GPL` `GPL-2` `GPL-3`

`LGPL` `LGPL-2.1` `LGPL-3`

`AGPL` `AGPL-3`

since 1.18

`BSD2`

since 1.20

`BSD3`

`MIT`

`ISC`

since 1.22

`MPL-2.0`

since 1.20

`Apache` `Apache-2.0`

`PublicDomain`

`AllRightsReserved`

`OtherLicense`

`license-file``:` _filename_[¶][53]

See [`license-files`][54].

`license-files``:` _filename list_[¶][55]

Since

Cabal 1.20

The name of a file(s) containing the precise copyright license for this package. The license file(s) will be installed with the package.

If you have multiple license files then use the [`license-files`][56] field instead of (or in addition to) the [`license-file`][57] field.

`copyright``:` _freeform_[¶][58]

The content of a copyright notice, typically the name of the holder of the copyright on the package and the year(s) from which copyright is claimed. For example:

copyright: (c) 2006-2007 Joe Bloggs

The original author of the package.

Remember that `.cabal` files are Unicode, using the UTF-8 encoding.

`maintainer``:` _address_[¶][59]

The current maintainer or maintainers of the package. This is an e-mail address to which users should send bug reports, feature requests and patches.

`stability``:` _freeform_[¶][60]

The stability level of the package, e.g. `alpha`, `experimental`, `provisional`, `stable`.

`homepage``:` _URL_[¶][61]

The package homepage.

`bug-reports``:` _URL_[¶][62]

The URL where users should direct bug reports. This would normally be either:

-   A `mailto:` URL, e.g. for a person or a mailing list.
    
-   An `http:` (or `https:`) URL for an online bug tracking system.
    

For example Cabal itself uses a web-based bug tracking system

bug-reports: https://github.com/haskell/cabal/issues

`package-url``:` _URL_[¶][63]

The location of a source bundle for the package. The distribution should be a Cabal package.

`synopsis``:` _freeform_[¶][64]

A very short description of the package, for use in a table of packages. This is your headline, so keep it short (one line) but as informative as possible. Save space by not including the package name or saying it’s written in Haskell.

`description``:` _freeform_[¶][65]

Description of the package. This may be several paragraphs, and should be aimed at a Haskell programmer who has never heard of your package before.

For library packages, this field is used as prologue text by [runhaskell Setup.hs haddock][66] and thus may contain the same markup as [Haddock][67] documentation comments.

`category``:` _freeform_[¶][68]

A classification category for future use by the package catalogue [Hackage][69]. These categories have not yet been specified, but the upper levels of the module hierarchy make a good start.

`tested-with``:` _compiler list_[¶][70]

A list of compilers and versions against which the package has been tested (or at least built). The value of this field is not used by Cabal and is rather intended as extra metadata for use by third party tooling, such as e.g. CI tooling.

Here’s a typical usage example

tested-with: GHC \== 8.6.3, GHC \== 8.4.4, GHC \== 8.2.2, GHC \== 8.0.2,
             GHC \== 7.10.3, GHC \== 7.8.4, GHC \== 7.6.3, GHC \== 7.4.2

which can (starting with Cabal 3.0) also be written using the more concise set notation syntax

tested-with: GHC \== { 8.6.3, 8.4.4, 8.2.2, 8.0.2, 7.10.3, 7.8.4, 7.6.3, 7.4.2 }

`data-files``:` _filename list_[¶][71]

A list of files to be installed for run-time use by the package. This is useful for packages that use a large amount of static data, such as tables of values or code templates. Cabal provides a way to [find these files at run-time][72].

A limited form of `*` wildcards in file names, for example `data-files: images/*.png` matches all the `.png` files in the `images` directory. `data-files: audio/**/*.mp3` matches all the `.mp3` files in the `audio` directory, including subdirectories.

The specific limitations of this wildcard syntax are

-   `*` wildcards are only allowed in place of the file name, not in the directory name or file extension. It must replace the whole file name (e.g., `*.html` is allowed, but `chapter-*.html` is not). If a wildcard is used, it must be used with an extension, so `data-files: data/*` is not allowed.
    
-   Prior to Cabal 2.4, when matching a wildcard plus extension, a file’s full extension must match exactly, so `*.gz` matches `foo.gz` but not `foo.tar.gz`. This restriction has been lifted when `cabal-version: 2.4` or greater so that `*.gz` does match `foo.tar.gz`
    
-   `*` wildcards will not match if the file name is empty (e.g., `*.html` will not match `foo/.html`).
    
-   `**` wildcards can only appear as the final path component before the file name (e.g., `data/**/images/*.jpg` is not allowed). If a `**` wildcard is used, then the file name must include a `*` wildcard (e.g., `data/**/README.rst` is not allowed).
    
-   A wildcard that does not match any files is an error.
    

The reason for providing only a very limited form of wildcard is to concisely express the common case of a large number of related files of the same file type without making it too easy to accidentally include unwanted files.

On efficiency: if you use `**` patterns, the directory tree will be walked starting with the parent directory of the `**`. If that’s the root of the project, this might include `.git/`, `dist-newstyle/`, or other large directories! To avoid this behaviour, put the files that wildcards will match against in their own folder.

`**` wildcards are available starting in Cabal 2.4.

`data-dir``:` _directory_[¶][73]

The directory where Cabal looks for data files to install, relative to the source directory. By default, Cabal will look in the source directory itself.

A list of additional files to be included in source distributions built with [runhaskell Setup.hs sdist][74]. As with [`data-files`][75] it can use a limited form of `*` wildcards in file names.

Since

Cabal 1.18

A list of additional files to be included in source distributions, and also copied to the html directory when Haddock documentation is generated. As with [`data-files`][76] it can use a limited form of `*` wildcards in file names.

A list of additional files or directories to be removed by [runhaskell Setup.hs clean][77]. These would typically be additional files created by additional hooks, such as the scheme described in the section on [system-dependent parameters][78]

### 7.2.3. Library[¶][79]

`library` _name_[¶][80]

Build information for libraries.

Currently, there can only be one publicly exposed library in a package, and its name is the same as package name set by global [`name`][81] field. In this case, the `name` argument to the [`library`][82] section must be omitted.

Starting with Cabal 2.0, private internal sub-library components can be defined by setting the `name` field to a name different from the current package’s name; see section on [Internal Libraries][83] for more information.

The library section should contain the following fields:

`exposed-modules``:` _identifier list_[¶][84]

Required

if this package contains a library

A list of modules added by this package.

`virtual-modules``:` _identifier list_[¶][85]

Since

Cabal 2.2

A list of virtual modules provided by this package. Virtual modules are modules without a source file. See for example the `GHC.Prim` module from the `ghc-prim` package. Modules listed here will not be built, but still end up in the list of `exposed-modules` in the installed package info when the package is registered in the package database.

`exposed``:` _boolean_[¶][86]

Default value

`True`

Some Haskell compilers (notably GHC) support the notion of packages being “exposed” or “hidden” which means the modules they provide can be easily imported without always having to specify which package they come from. However this only works effectively if the modules provided by all exposed packages do not overlap (otherwise a module import would be ambiguous).

Almost all new libraries use hierarchical module names that do not clash, so it is very uncommon to have to use this field. However it may be necessary to set `exposed: False` for some old libraries that use a flat module namespace or where it is known that the exposed modules would clash with other common modules.

`visibility``:` _visibility specifiers_[¶][87]

Since

3.0

Default value

`private` for internal libraries. Cannot be set for public library.

Cabal recognizes `public` and `private` here…

Multiple public libraries…

`reexported-modules``:` _exportlist_[¶][88]

Since

Cabal 1.22

Supported only in GHC 7.10 and later. A list of modules to _reexport_ from this package. The syntax of this field is `orig-pkg:Name as NewName` to reexport module `Name` from `orig-pkg` with the new name `NewName`. We also support abbreviated versions of the syntax: if you omit `as NewName`, we’ll reexport without renaming; if you omit `orig-pkg`, then we will automatically figure out which package to reexport from, if it’s unambiguous.

Reexported modules are useful for compatibility shims when a package has been split into multiple packages, and they have the useful property that if a package provides a module, and another package reexports it under the same name, these are not considered a conflict (as would be the case with a stub module.) They can also be used to resolve name conflicts.

`signatures``:` _signature list_[¶][89]

Since

Cabal 2.0

Supported only in GHC 8.2 and later. A list of [module signatures][90] required by this package.

Module signatures are part of the [Backpack][91] extension to the Haskell module system.

Packages that do not export any modules and only export required signatures are called “signature-only packages”, and their signatures are subjected to [signature thinning][92].

The library section may also contain build information fields (see the section on [build information][93]).

**Internal Libraries**

Cabal 2.0 and later support “internal libraries”, which are extra named libraries (as opposed to the usual unnamed library section). For example, suppose that your test suite needs access to some internal modules in your library, which you do not otherwise want to export. You could put these modules in an internal library, which the main library and the test suite [`build-depends`][94] upon. Then your Cabal file might look something like this:

cabal-version:  2.0
name:           foo
version:        0.1.0.0
license:        BSD3
build-type:     Simple

library foo-internal
 exposed-modules: Foo.Internal
 \-- NOTE: no explicit constraints on base needed
 \--       as they're inherited from the 'library' stanza
 build-depends: base

library
 exposed-modules: Foo.Public
 build-depends: foo-internal, base \>= 4.3 && < 5

test-suite test-foo
 type:       exitcode-stdio-1.0
 main-is:    test-foo.hs
 \-- NOTE: no constraints on 'foo-internal' as same-package
 \--       dependencies implicitly refer to the same package instance
 build-depends: foo-internal, base

Internal libraries are also useful for packages that define multiple executables, but do not define a publicly accessible library. Internal libraries are only visible internally in the package (so they can only be added to the [`build-depends`][95] of same-package libraries, executables, test suites, etc.) Internal libraries locally shadow any packages which have the same name; consequently, don’t name an internal library with the same name as an external dependency if you need to be able to refer to the external dependency in a [`build-depends`][96] declaration.

Shadowing can be used to vendor an external dependency into a package and thus emulate _private dependencies_. Below is an example based on a real-world use case:

cabal-version: 2.2
name: haddock-library
version: 1.6.0

library
 build-depends:
    , base         ^>= 4.11.1.0
    , bytestring   ^>= 0.10.2.0
    , containers   ^>= 0.4.2.1 || ^>= 0.5.0.0
    , transformers ^>= 0.5.0.0

 hs-source-dirs:       src

 \-- internal sub-lib
 build-depends:        attoparsec

 exposed-modules:
    Documentation.Haddock

library attoparsec
 build-depends:
    , base         ^>= 4.11.1.0
    , bytestring   ^>= 0.10.2.0
    , deepseq      ^>= 1.4.0.0

 hs-source-dirs:       vendor/attoparsec-0.13.1.0

 \-- NB: haddock-library needs only small part of lib:attoparsec
 \--     internally, so we only bundle that subset here
 exposed-modules:
    Data.Attoparsec.ByteString
    Data.Attoparsec.Combinator

 other-modules:
    Data.Attoparsec.Internal

 ghc-options: -funbox-strict-fields -Wall -fwarn-tabs -O2

### 7.2.4. Opening an interpreter session[¶][97]

While developing a package, it is often useful to make its code available inside an interpreter session. This can be done with the `repl` command:

The name comes from the acronym [REPL][98], which stands for “read-eval-print-loop”. By default `cabal repl` loads the first component in a package. If the package contains several named components, the name can be given as an argument to `repl`. The name can be also optionally prefixed with the component’s type for disambiguation purposes. Example:

$ cabal repl foo
$ cabal repl exe:foo
$ cabal repl test:bar
$ cabal repl bench:baz

#### 7.2.4.1. Freezing dependency versions[¶][99]

If a package is built in several different environments, such as a development environment, a staging environment and a production environment, it may be necessary or desirable to ensure that the same dependency versions are selected in each environment. This can be done with the `freeze` command:

The command writes the selected version for all dependencies to the `cabal.config` file. All environments which share this file will use the dependency versions specified in it.

#### 7.2.4.2. Generating dependency version bounds[¶][100]

Cabal also has the ability to suggest dependency version bounds that conform to [Package Versioning Policy][101], which is a recommended versioning system for publicly released Cabal packages. This is done by running the `gen-bounds` command:

For example, given the following dependencies specified in [`build-depends`][102]:

build-depends:
  foo \== 0.5.2
  bar \== 1.1

`gen-bounds` will suggest changing them to the following:

build-depends:
  foo \>= 0.5.2 && < 0.6
  bar \>= 1.1 && < 1.2

#### 7.2.4.3. Listing outdated dependency version bounds[¶][103]

Manually updating dependency version bounds in a `.cabal` file or a freeze file can be tedious, especially when there’s a lot of dependencies. The `cabal outdated` command is designed to help with that. It will print a list of packages for which there is a new version on Hackage that is outside the version bound specified in the `build-depends` field. The `outdated` command can also be configured to act on the freeze file (both old- and v2-style) and ignore major (or all) version bumps on Hackage for a subset of dependencies.

The following flags are supported by the `outdated` command:

`--freeze-file`

Read dependency version bounds from the freeze file (`cabal.config`) instead of the package description file (`$PACKAGENAME.cabal`). `--v1-freeze-file` is an alias for this flag starting in Cabal 2.4.

`--v2-freeze-file`

since

2.4

Read dependency version bounds from the v2-style freeze file (by default, `cabal.project.freeze`) instead of the package description file. `--new-freeze-file` is an alias for this flag that can be used with pre-2.4 `cabal`.

`--project-file` _PROJECTFILE_

since

2.4

Read dependendency version bounds from the v2-style freeze file related to the named project file (i.e., `$PROJECTFILE.freeze`) instead of the package desctription file. If multiple `--project-file` flags are provided, only the final one is considered. This flag must only be passed in when `--new-freeze-file` is present.

`--simple-output`

Print only the names of outdated dependencies, one per line.

`--exit-code`

Exit with a non-zero exit code when there are outdated dependencies.

`-q, --quiet`

Don’t print any output. Implies `-v0` and `--exit-code`.

`--ignore` _PACKAGENAMES_

Don’t warn about outdated dependency version bounds for the packages in this list.

`--minor` _\[PACKAGENAMES\]_

Ignore major version bumps for these packages. E.g. if there’s a version 2.0 of a package `pkg` on Hackage and the freeze file specifies the constraint `pkg == 1.9`, `cabal outdated --freeze --minor=pkg` will only consider the `pkg` outdated when there’s a version of `pkg` on Hackage satisfying `pkg > 1.9 && < 2.0`. `--minor` can also be used without arguments, in that case major version bumps are ignored for all packages.

Examples:

$ cd /some/package
$ cabal outdated
Outdated dependencies:
haskell-src-exts <1.17 (latest: 1.19.1)
language-javascript <0.6 (latest: 0.6.0.9)
unix ==2.7.2.0 (latest: 2.7.2.1)

$ cabal outdated --simple-output
haskell-src-exts
language-javascript
unix

$ cabal outdated --ignore\=haskell-src-exts
Outdated dependencies:
language-javascript <0.6 (latest: 0.6.0.9)
unix ==2.7.2.0 (latest: 2.7.2.1)

$ cabal outdated --ignore\=haskell-src-exts,language-javascript,unix
All dependencies are up to date.

$ cabal outdated --ignore\=haskell-src-exts,language-javascript,unix -q
$ echo $?
0

$ cd /some/other/package
$ cabal outdated --freeze-file
Outdated dependencies:
HTTP ==4000.3.3 (latest: 4000.3.4)
HUnit ==1.3.1.1 (latest: 1.5.0.0)

$ cabal outdated --freeze-file --ignore\=HTTP --minor\=HUnit
Outdated dependencies:
HUnit ==1.3.1.1 (latest: 1.3.1.2)

### 7.2.5. Executables[¶][104]

`executable` _name_[¶][105]

Executable sections (if present) describe executable programs contained in the package and must have an argument after the section label, which defines the name of the executable. This is a freeform argument but may not contain spaces.

The executable may be described using the following fields, as well as build information fields (see the section on [build information][106]).

`main-is``:` _filename (required)_[¶][107]

The name of the `.hs` or `.lhs` file containing the `Main` module. Note that it is the `.hs` filename that must be listed, even if that file is generated using a preprocessor. The source file must be relative to one of the directories listed in [`hs-source-dirs`][108]. Further, while the name of the file may vary, the module itself must be named `Main`.

Starting with `cabal-version: 1.18` this field supports specifying a C, C++, or objC source file as the main entry point.

`scope``:` _token_[¶][109]

Since

Cabal 2.0

Whether the executable is `public` (default) or `private`, i.e. meant to be run by other programs rather than the user. Private executables are installed into $libexecdir/$libexecsubdir.

#### 7.2.5.1. Running executables[¶][110]

You can have Cabal build and run your executables by using the `run` command:

$ cabal run EXECUTABLE \[\-- EXECUTABLE\_FLAGS\]

This command will configure, build and run the executable `EXECUTABLE`. The double dash separator is required to distinguish executable flags from `run`’s own flags. If there is only one executable defined in the whole package, the executable’s name can be omitted. See the output of `cabal help run` for a list of options you can pass to `cabal run`.

### 7.2.6. Test suites[¶][111]

`test-suite` _name_[¶][112]

Test suite sections (if present) describe package test suites and must have an argument after the section label, which defines the name of the test suite. This is a freeform argument, but may not contain spaces. It should be unique among the names of the package’s other test suites, the package’s executables, and the package itself. Using test suite sections requires at least Cabal version 1.9.2.

The test suite may be described using the following fields, as well as build information fields (see the section on [build information][113]).

`type``:` _interface (required)_[¶][114]

The interface type and version of the test suite. Cabal supports two test suite interfaces, called `exitcode-stdio-1.0` and `detailed-0.9`. Each of these types may require or disallow other fields as described below.

Test suites using the `exitcode-stdio-1.0` interface are executables that indicate test failure with a non-zero exit code when run; they may provide human-readable log information through the standard output and error channels. The `exitcode-stdio-1.0` type requires the `main-is` field.

`main-is``:` _filename_[¶][115]

Required

`exitcode-stdio-1.0`

Disallowed

`detailed-0.9`

The name of the `.hs` or `.lhs` file containing the `Main` module. Note that it is the `.hs` filename that must be listed, even if that file is generated using a preprocessor. The source file must be relative to one of the directories listed in [`hs-source-dirs`][116]. This field is analogous to the `main-is` field of an executable section.

Test suites using the `detailed-0.9` interface are modules exporting the symbol `tests :: IO [Test]`. The `Test` type is exported by the module `Distribution.TestSuite` provided by Cabal. For more details, see the example below.

The `detailed-0.9` interface allows Cabal and other test agents to inspect a test suite’s results case by case, producing detailed human- and machine-readable log files. The `detailed-0.9` interface requires the [`test-module`][117] field.

`test-module``:` _identifier_[¶][118]

Required

`detailed-0.9`

Disallowed

`exitcode-stdio-1.0`

The module exporting the `tests` symbol.

#### 7.2.6.1. Example: Package using `exitcode-stdio-1.0` interface[¶][119]

The example package description and executable source file below demonstrate the use of the `exitcode-stdio-1.0` interface.

foo.cabal[¶][120]

Name:           foo
Version:        1.0
License:        BSD3
Cabal-Version:  \>= 1.9.2
Build-Type:     Simple

Test-Suite test-foo
 type:       exitcode-stdio-1.0
 main-is:    test-foo.hs
 build-depends: base \>= 4 && < 5

test-foo.hs[¶][121]

module Main where

import System.Exit (exitFailure)

main \= do
    putStrLn "This test always fails!"
    exitFailure

#### 7.2.6.2. Example: Package using `detailed-0.9` interface[¶][122]

The example package description and test module source file below demonstrate the use of the `detailed-0.9` interface. The test module also develops a simple implementation of the interface set by `Distribution.TestSuite`, but in actual usage the implementation would be provided by the library that provides the testing facility.

bar.cabal[¶][123]

Name:           bar
Version:        1.0
License:        BSD3
Cabal-Version:  \>= 1.9.2
Build-Type:     Simple

Test-Suite test-bar
 type:       detailed-0.9
 test-module: Bar
 build-depends: base \>= 4 && < 5, Cabal \>= 1.9.2 && < 2

Bar.hs[¶][124]

module Bar ( tests ) where

import Distribution.TestSuite

tests :: IO \[Test\]
tests \= return \[ Test succeeds, Test fails \]
  where
    succeeds \= TestInstance
        { run \= return $ Finished Pass
        , name \= "succeeds"
        , tags \= \[\]
        , options \= \[\]
        , setOption \= \\\_ \_ \-> Right succeeds
        }
    fails \= TestInstance
        { run \= return $ Finished $ Fail "Always fails!"
        , name \= "fails"
        , tags \= \[\]
        , options \= \[\]
        , setOption \= \\\_ \_ \-> Right fails
        }

#### 7.2.6.3. Running test suites[¶][125]

You can have Cabal run your test suites using its built-in test runner:

$ cabal configure --enable-tests
$ cabal build
$ cabal test

See the output of `cabal help test` for a list of options you can pass to `cabal test`.

### 7.2.7. Benchmarks[¶][126]

`benchmark` _name_[¶][127]

Since

Cabal 1.9.2

Benchmark sections (if present) describe benchmarks contained in the package and must have an argument after the section label, which defines the name of the benchmark. This is a freeform argument, but may not contain spaces. It should be unique among the names of the package’s other benchmarks, the package’s test suites, the package’s executables, and the package itself. Using benchmark sections requires at least Cabal version 1.9.2.

The benchmark may be described using the following fields, as well as build information fields (see the section on [build information][128]).

`type``:` _interface (required)_[¶][129]

The interface type and version of the benchmark. At the moment Cabal only support one benchmark interface, called `exitcode-stdio-1.0`.

Benchmarks using the `exitcode-stdio-1.0` interface are executables that indicate failure to run the benchmark with a non-zero exit code when run; they may provide human-readable information through the standard output and error channels.

`main-is``:` _filename_[¶][130]

Required

`exitcode-stdio-1.0`

The name of the `.hs` or `.lhs` file containing the `Main` module. Note that it is the `.hs` filename that must be listed, even if that file is generated using a preprocessor. The source file must be relative to one of the directories listed in [`hs-source-dirs`][131]. This field is analogous to the `main-is` field of an executable section. Further, while the name of the file may vary, the module itself must be named `Main`.

#### 7.2.7.1. Example: Package using `exitcode-stdio-1.0` interface[¶][132]

The example package description and executable source file below demonstrate the use of the `exitcode-stdio-1.0` interface.

foo.cabal[¶][133]

Name:           foo
Version:        1.0
License:        BSD3
Cabal-Version:  \>= 1.9.2
Build-Type:     Simple

Benchmark bench-foo
 type:       exitcode-stdio-1.0
 main-is:    bench-foo.hs
 build-depends: base \>= 4 && < 5, time \>= 1.1 && < 1.7

bench-foo.hs[¶][134]

{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Time.Clock

fib 0 \= 1
fib 1 \= 1
fib n \= fib (n\-1) + fib (n\-2)

main \= do
    start <- getCurrentTime
    let !r \= fib 20
    end <- getCurrentTime
    putStrLn $ "fib 20 took " ++ show (diffUTCTime end start)

#### 7.2.7.2. Running benchmarks[¶][135]

You can have Cabal run your benchmark using its built-in benchmark runner:

$ cabal configure --enable-benchmarks
$ cabal build
$ cabal bench

See the output of `cabal help bench` for a list of options you can pass to `cabal bench`.

### 7.2.8. Foreign libraries[¶][136]

Foreign libraries are system libraries intended to be linked against programs written in C or other “foreign” languages. They come in two primary flavours: dynamic libraries (`.so` files on Linux, `.dylib` files on OSX, `.dll` files on Windows, etc.) are linked against executables when the executable is run (or even lazily during execution), while static libraries (`.a` files on Linux/OSX, `.lib` files on Windows) get linked against the executable at compile time.

Foreign libraries only work with GHC 7.8 and later.

A typical stanza for a foreign library looks like

foreign-library myforeignlib
 type:                native-shared
 lib-version-info:    6:3:2

  if os(Windows)
 options: standalone
 mod-def-file: MyForeignLib.def

 other-modules:       MyForeignLib.SomeModule
                       MyForeignLib.SomeOtherModule
 build-depends:       base \>=4.7 && <4.9
 hs-source-dirs:      src
 c-sources:           csrc/MyForeignLibWrapper.c
 default-language:    Haskell2010

`foreign-library` _name_[¶][137]

Since

Cabal 2.0

Build information for [foreign libraries][138].

`type``:` _foreign library type_[¶][139]

Cabal recognizes `native-static` and `native-shared` here, although we currently only support building native-shared libraries.

`options``:` _foreign library option list_[¶][140]

Options for building the foreign library, typically specific to the specified type of foreign library. Currently we only support `standalone` here. A standalone dynamic library is one that does not have any dependencies on other (Haskell) shared libraries; without the `standalone` option the generated library would have dependencies on the Haskell runtime library (`libHSrts`), the base library (`libHSbase`), etc. Currently, `standalone` _must_ be used on Windows and _must not_ be used on any other platform.

`mod-def-file``:` _filename_[¶][141]

This option can only be used when creating dynamic Windows libraries (that is, when using `native-shared` and the `os` is `Windows`). If used, it must be a path to a _module definition file_. The details of module definition files are beyond the scope of this document; see the [GHC][142] manual for some details and some further pointers.

`lib-version-info``:` _current:revision:age_[¶][143]

This field is currently only used on Linux.

This field specifies a Libtool-style version-info field that sets an appropriate ABI version for the foreign library. Note that the three numbers specified in this field do not directly specify the actual ABI version: `6:3:2` results in library version `4.2.3`.

With this field set, the SONAME of the library is set, and symlinks are installed.

How you should bump this field on an ABI change depends on the breakage you introduce:

-   Programs using the previous version may use the new version as drop-in replacement, and programs using the new version can also work with the previous one. In other words, no recompiling nor relinking is needed. In this case, bump `revision` only, don’t touch current nor age.
    
-   Programs using the previous version may use the new version as drop-in replacement, but programs using the new version may use APIs not present in the previous one. In other words, a program linking against the new version may fail with “unresolved symbols” if linking against the old version at runtime: set revision to 0, bump current and age.
    
-   Programs may need to be changed, recompiled, and relinked in order to use the new version. Bump current, set revision and age to 0.
    

Also refer to the Libtool documentation on the version-info field.

`lib-version-linux``:` _version_[¶][144]

This field is only used on Linux.

Specifies the library ABI version directly for foreign libraries built on Linux: so specifying `4.2.3` causes a library `libfoo.so.4.2.3` to be built with SONAME `libfoo.so.4`, and appropriate symlinks `libfoo.so.4` and `libfoo.so` to be installed.

Note that typically foreign libraries should export a way to initialize and shutdown the Haskell runtime. In the example above, this is done by the `csrc/MyForeignLibWrapper.c` file, which might look something like

#include <stdlib.h>
#include "HsFFI.h"

HsBool myForeignLibInit(void){
 int argc \= 2;
 char \*argv\[\] \= { "+RTS", "-A32m", NULL };
 char \*\*pargv \= argv;

 // Initialize Haskell runtime
 hs\_init(&argc, &pargv);

 // do any other initialization here and
 // return false if there was a problem
 return HS\_BOOL\_TRUE;
}

void myForeignLibExit(void){
 hs\_exit();
}

With modern ghc regular libraries are installed in directories that contain package keys. This isn’t usually a problem because the package gets registered in ghc’s package DB and so we can figure out what the location of the library is. Foreign libraries however don’t get registered, which means that we’d have to have a way of finding out where a platform library got installed (other than by searching the `lib/` directory). Instead, we install foreign libraries in `~/.cabal/lib`, much like we install executables in `~/.cabal/bin`.

### 7.2.9. Build information[¶][145]

The following fields may be optionally present in a library, executable, test suite or benchmark section, and give information for the building of the corresponding library or executable. See also the sections on [system-dependent parameters][146] and [configurations][147] for a way to supply system-dependent values for these fields.

`build-depends``:` _library list_[¶][148]

Declares the _library_ dependencies required to build the current package component; see [`build-tool-depends`][149] for declaring build-time _tool_ dependencies. External library dependencies should be annotated with a version constraint.

**Library Names**

External libraries are identified by the package’s name they’re provided by (currently a package can only publicly expose its main library component; in future, packages with multiple exposed public library components will be supported and a syntax for referring to public sub-libraries will be provided).

In order to specify an intra-package dependency on an internal library component you can use the unqualified name of the library component. Note that locally defined sub-library names shadow external package names of the same name. See section on [Internal Libraries][150] for examples and more information.

**Version Constraints**

Version constraints use the operators `==, >=, >, <, <=` and a version number. Multiple constraints can be combined using `&&` or `||`. If no version constraint is specified, any version is assumed to be acceptable. For example:

library
 build-depends:
    base \>= 2,
    foo \>= 1.2.3 && < 1.3,
    bar

Dependencies like `foo >= 1.2.3 && < 1.3` turn out to be very common because it is recommended practice for package versions to correspond to API versions (see [PVP][151]).

Since Cabal 1.6, there is a special wildcard syntax to help with such ranges

build-depends: foo \==1.2.\*

It is only syntactic sugar. It is exactly equivalent to `foo >= 1.2 && < 1.3`.

Warning

A potential pitfall of the wildcard syntax is that the constraint `nats == 1.0.*` doesn’t match the release `nats-1` because the version `1` is lexicographically less than `1.0`. This is not an issue with the caret-operator `^>=` described below.

Starting with Cabal 2.0, there’s a new version operator to express [PVP][152]\-style major upper bounds conveniently, and is inspired by similar syntactic sugar found in other language ecosystems where it’s often called the “Caret” operator:

build-depends:
  foo ^>= 1.2.3.4,
  bar ^>= 1

This allows to assert the positive knowledge that this package is _known_ to be semantically compatible with the releases `foo-1.2.3.4` and `bar-1` respectively. The information encoded via such `^>=`\-assertions is used by the cabal solver to infer version constraints describing semantically compatible version ranges according to the [PVP][153] contract (see below).

Another way to say this is that `foo < 1.3` expresses _negative_ information, i.e. “`foo-1.3` or `foo-1.4.2` will _not_ be compatible”; whereas `foo ^>= 1.2.3.4` asserts the _positive_ information that “`foo-1.2.3.4` is _known_ to be compatible” and (in the absence of additional information) according to the [PVP][154] contract we can (positively) infer right away that all versions satisfying `foo >= 1.2.3.4 && < 1.3` will be compatible as well.

Note

More generally, the [PVP][155] contract implies that we can safely relax the lower bound to `>= 1.2`, because if we know that `foo-1.2.3.4` is semantically compatible, then so is `foo-1.2` (if it typechecks). But we’d need to perform additional static analysis (i.e. perform typechecking) in order to know if our package in the role of an API consumer will successfully typecheck against the dependency `foo-1.2`. But since we cannot do this analysis during constraint solving and to keep things simple, we pragmatically use `foo >= 1.2.3.4` as the initially inferred approximation for the lower bound resulting from the assertion `foo ^>= 1.2.3.4`. If further evidence becomes available that e.g. `foo-1.2` typechecks, one can simply revise the dependency specification to include the assertion `foo ^>= 1.2`.

The subtle but important difference in signaling allows tooling to treat explicitly expressed `<`\-style constraints and inferred (`^>=`\-style) upper bounds differently. For instance, [`allow-newer`][156]’s `^`\-modifier allows to relax only `^>=`\-style bounds while leaving explicitly stated `<`\-constraints unaffected.

Ignoring the signaling intent, the default syntactic desugaring rules are

-   `^>= x` == `>= x && < x.1`
    
-   `^>= x.y` == `>= x.y && < x.(y+1)`
    
-   `^>= x.y.z` == `>= x.y.z && < x.(y+1)`
    
-   `^>= x.y.z.u` == `>= x.y.z.u && < x.(y+1)`
    
-   etc.
    

Note

One might expect the desugaring to truncate all version components below (and including) the patch-level, i.e. `^>= x.y.z.u` == `>= x.y.z && < x.(y+1)`, as the major and minor version components alone are supposed to uniquely identify the API according to the [PVP][157]. However, by designing `^>=` to be closer to the `>=` operator, we avoid the potentially confusing effect of `^>=` being more liberal than `>=` in the presence of patch-level versions.

Consequently, the example declaration above is equivalent to

build-depends:
  foo \>= 1.2.3.4 && < 1.3,
  bar \>= 1 && < 1.1

Note

Prior to Cabal 1.8, `build-depends` specified in each section were global to all sections. This was unintentional, but some packages were written to depend on it, so if you need your [`build-depends`][158] to be local to each section, you must specify at least `Cabal-Version: >= 1.8` in your `.cabal` file.

Note

Cabal 1.20 experimentally supported module thinning and renaming in `build-depends`; however, this support has since been removed and should not be used.

Starting with Cabal 3.0, a set notation for the `==` and `^>=` operator is available. For instance,

tested-with: GHC \== 8.6.3, GHC \== 8.4.4, GHC \== 8.2.2, GHC \== 8.0.2,
             GHC \== 7.10.3, GHC \== 7.8.4, GHC \== 7.6.3, GHC \== 7.4.2

build-depends: network ^>= 2.6.3.6 || ^>= 2.7.0.2 || ^>= 2.8.0.0 || ^>= 3.0.1.0

can be then written in a more convenient and concise form

tested-with: GHC \== { 8.6.3, 8.4.4, 8.2.2, 8.0.2, 7.10.3, 7.8.4, 7.6.3, 7.4.2 }

build-depends: network ^>= { 2.6.3.6, 2.7.0.2, 2.8.0.0, 3.0.1.0 }

`other-modules``:` _identifier list_[¶][159]

A list of modules used by the component but not exposed to users. For a library component, these would be hidden modules of the library. For an executable, these would be auxiliary modules to be linked with the file named in the `main-is` field.

`hs-source-dir``:` _directory list_[¶][160]

Removed

Cabal 3.0

Deprecated

Cabal 2.0

Default value

`.`

Root directories for the module hierarchy.

Deprecated in favor of [`hs-source-dirs`][161].

`hs-source-dirs``:` _directory list_[¶][162]

Default value

`.`

Root directories for the module hierarchy.

Note

Components can share source directories but modules found there will be recompiled even if other components already built them, i.e., if a library and an executable share a source directory and the executable depends on the library and imports its `Foo` module, `Foo` will be compiled twice, once as part of the library and again for the executable.

`default-extensions``:` _identifier list_[¶][163]

Since

Cabal 1.12

> A list of Haskell extensions used by every module. These determine corresponding compiler options enabled for all files. Extension names are the constructors of the [Extension][164] type. For example, `CPP` specifies that Haskell source files are to be preprocessed with a C preprocessor.

`other-extensions``:` _identifier list_[¶][165]

Since

Cabal 1.12

> A list of Haskell extensions used by some (but not necessarily all) modules. From GHC version 6.6 onward, these may be specified by placing a `LANGUAGE` pragma in the source files affected e.g.
> 
> {-# LANGUAGE CPP, MultiParamTypeClasses #-}
> 
> In Cabal-1.24 the dependency solver will use this and [`default-extensions`][166] information. Cabal prior to 1.24 will abort compilation if the current compiler doesn’t provide the extensions.
> 
> If you use some extensions conditionally, using CPP or conditional module lists, it is good to replicate the condition in [`other-extensions`][167] declarations:
> 
> other-extensions: CPP
> if impl(ghc \>= 7.5)
>  other-extensions: PolyKinds
> 
> You could also omit the conditionally used extensions, as they are for information only, but it is recommended to replicate them in [`other-extensions`][168] declarations.

`default-language``:` _identifier_[¶][169]

Since

Cabal 1.12

TBW

`other-languages``:` _identifier_[¶][170]

Since

Cabal 1.12

TBW

`extensions``:` _identifier list_[¶][171]

Removed

Cabal 3.0

Deprecated

Cabal 1.12

Deprecated in favor of [`default-extensions`][172].

`build-tool-depends``:` _package:executable list_[¶][173]

Since

Cabal 2.0

A list of Haskell executables needed to build this component. Executables are provided during the whole duration of the component, so this field can be used for executables needed during [`test-suite`][174] as well.

Each is specified by the package containing the executable and the name of the executable itself, separated by a colon, and optionally followed by a version bound.

All executables defined in the given Cabal file are termed as _internal_ dependencies as opposed to the rest which are _external_ dependencies.

Each of the two is handled differently:

1.  External dependencies can (and should) contain a version bound like conventional [`build-depends`][175] dependencies.
    
2.  Internal dependencies should not contain a version bound, as they will be always resolved within the same configuration of the package in the build plan. Specifically, version bounds that include the package’s version will be warned for being extraneous, and version bounds that exclude the package’s version will raise an error for being impossible to follow.
    

For example (1) using a test-suite to make sure README.md Haskell snippets are tested using [markdown-unlit][176]:

build-tool-depends: markdown-unlit:markdown-unlit \>= 0.5.0 && < 0.6

For example (2) using a test-suite to test executable behaviour in the same package:

build-tool-depends: mypackage:executable

Cabal tries to make sure that all specified programs are atomically built and prepended on the `$PATH` shell variable before building the component in question, but can only do so for Nix-style builds. Specifically:

1.  For Nix-style local builds, both internal and external dependencies.
    
2.  For old-style builds, only for internal dependencies [1][177]. It’s up to the user to provide needed executables in this case under $PATH.
    

Note

[`build-tool-depends`][178] was added in Cabal 2.0, and it will be ignored (with a warning) with old versions of Cabal. See [`build-tools`][179] for more information about backwards compatibility.

`build-tools``:` _program list_[¶][180]

Removed

Cabal 3.0

Deprecated

Cabal 2.0

Deprecated in favor of [`build-tool-depends`][181], but [see below for backwards compatibility information][182].

A list of Haskell programs needed to build this component. Each may be followed by an optional version bound. Confusingly, each program in the list either refer to one of three things:

> 1.  Another executables in the same package (supported since Cabal 1.12)
>     
> 2.  Tool name contained in Cabal’s [hard-coded set of common tools][183]
>     
> 3.  A pre-built executable that should already be on the `PATH` (supported since Cabal 2.0)
>     

These cases are listed in order of priority: an executable in the package will override any of the hard-coded packages with the same name, and a hard-coded package will override any executable on the `PATH`.

In the first two cases, the list entry is desugared into a [`build-tool-depends`][184] entry. In the first case, the entry is desugared into a [`build-tool-depends`][185] entry by prefixing with `$pkg:`. In the second case, it is desugared by looking up the package and executable name in a hard-coded table. In either case, the optional version bound is passed through unchanged. Refer to the documentation for [`build-tool-depends`][186] to understand the desugared field’s meaning, along with restrictions on version bounds.

**Backward Compatibility**

Although this field is deprecated in favor of [`build-tool-depends`][187], there are some situations where you may prefer to use [`build-tools`][188] in cases (1) and (2), as it is supported by more versions of Cabal. In case (3), [`build-tool-depends`][189] is better for backwards-compatibility, as it will be ignored by old versions of Cabal; if you add the executable to [`build-tools`][190], a setup script built against old Cabal will choke. If an old version of Cabal is used, an end-user will have to manually arrange for the requested executable to be in your `PATH`.

**Set of Known Tool Names**

Identifiers specified in [`build-tools`][191] are desugared into their respective equivalent [`build-tool-depends`][192] form according to the table below. Consequently, a legacy specification such as:

build-tools: alex \>= 3.2.1 && < 3.3, happy \>= 1.19.5 && < 1.20

is simply desugared into the equivalent specification:

build-tool-depends: alex:alex \>= 3.2.1 && < 3.3, happy:happy \>= 1.19.5 && < 1.20

  

[`build-tools`][193] identifier

desugared [`build-tool-depends`][194] identifier

Note

`alex`

`alex:alex`

`c2hs`

`c2hs:c2hs`

`cpphs`

`cpphs:cpphs`

`greencard`

`greencard:greencard`

`haddock`

`haddock:haddock`

`happy`

`happy:happy`

`hsc2hs`

`hsc2hs:hsc2hs`

`hscolour`

`hscolour:hscolour`

`hspec-discover`

`hspec-discover:hspec-discover`

since Cabal 2.0

This built-in set can be programmatically extended via `Custom` setup scripts; this, however, is of limited use since the Cabal solver cannot access information injected by `Custom` setup scripts.

`buildable``:` _boolean_[¶][195]

Default value

`True`

Is the component buildable? Like some of the other fields below, this field is more useful with the slightly more elaborate form of the simple build infrastructure described in the section on [system-dependent parameters][196].

`ghc-options``:` _token list_[¶][197]

Additional options for GHC. You can often achieve the same effect using the [`default-extensions`][198] field, which is preferred.

Options required only by one module may be specified by placing an `OPTIONS_GHC` pragma in the source file affected.

As with many other fields, whitespace can be escaped by using Haskell string syntax. Example: `ghc-options: -Wcompat "-with-rtsopts=-T -I1" -Wall`.

`ghc-prof-options``:` _token list_[¶][199]

Additional options for GHC when the package is built with profiling enabled.

Note that as of Cabal-1.24, the default profiling detail level defaults to `exported-functions` for libraries and `toplevel-functions` for executables. For GHC these correspond to the flags `-fprof-auto-exported` and `-fprof-auto-top`. Prior to Cabal-1.24 the level defaulted to `none`. These levels can be adjusted by the person building the package with the `--profiling-detail` and `--library-profiling-detail` flags.

It is typically better for the person building the package to pick the profiling detail level rather than for the package author. So unless you have special needs it is probably better not to specify any of the GHC `-fprof-auto*` flags here. However if you wish to override the profiling detail level, you can do so using the [`ghc-prof-options`][200] field: use `-fno-prof-auto` or one of the other `-fprof-auto*` flags.

`ghc-shared-options``:` _token list_[¶][201]

Additional options for GHC when the package is built as shared library. The options specified via this field are combined with the ones specified via [`ghc-options`][202], and are passed to GHC during both the compile and link phases.

`ghcjs-options``:` _token list_[¶][203]

Like [`ghc-options`][204] but applies to GHCJS

`ghcjs-prof-options``:` _token list_[¶][205]

Like [`ghc-prof-options`][206] but applies to GHCJS

`ghcjs-shared-options``:` _token list_[¶][207]

Like [`ghc-shared-options`][208] but applies to GHCJS

`includes``:` _filename list_[¶][209]

A list of header files to be included in any compilations via C. This field applies to both header files that are already installed on the system and to those coming with the package to be installed. The former files should be found in absolute paths, while the latter files should be found in paths relative to the top of the source tree or relative to one of the directories listed in [`include-dirs`][210].

These files typically contain function prototypes for foreign imports used by the package. This is in contrast to [`install-includes`][211], which lists header files that are intended to be exposed to other packages that transitively depend on this library.

`install-includes``:` _filename list_[¶][212]

A list of header files from this package to be installed into `$libdir/includes` when the package is installed. Files listed in [`install-includes`][213] should be found in relative to the top of the source tree or relative to one of the directories listed in [`include-dirs`][214].

[`install-includes`][215] is typically used to name header files that contain prototypes for foreign imports used in Haskell code in this package, for which the C implementations are also provided with the package. For example, here is a `.cabal` file for a hypothetical `bindings-clib` package that bundles the C source code for `clib`:

include-dirs:     cbits
c-sources:        clib.c
install-includes: clib.h

Now any package that depends (directly or transitively) on the `bindings-clib` library can use `clib.h`.

Note that in order for files listed in [`install-includes`][216] to be usable when compiling the package itself, they need to be listed in the [`includes`][217] field as well.

`include-dirs``:` _directory list_[¶][218]

A list of directories to search for header files, when preprocessing with `c2hs`, `hsc2hs`, `cpphs` or the C preprocessor, and also when compiling via C. Directories can be absolute paths (e.g., for system directories) or paths that are relative to the top of the source tree. Cabal looks in these directories when attempting to locate files listed in [`includes`][219] and [`install-includes`][220].

`c-sources``:` _filename list_[¶][221]

A list of C source files to be compiled and linked with the Haskell files.

`cxx-sources``:` _filename list_[¶][222]

Since

Cabal 2.2

A list of C++ source files to be compiled and linked with the Haskell files. Useful for segregating C and C++ sources when supplying different command-line arguments to the compiler via the [`cc-options`][223] and the [`cxx-options`][224] fields. The files listed in the [`cxx-sources`][225] can reference files listed in the [`c-sources`][226] field and vice-versa. The object files will be linked appropriately.

`asm-sources``:` _filename list_[¶][227]

Since

Cabal 3.0

A list of assembly source files to be compiled and linked with the Haskell files.

`cmm-sources``:` _filename list_[¶][228]

Since

Cabal 3.0

A list of C– source files to be compiled and linked with the Haskell files.

`js-sources``:` _filename list_[¶][229]

A list of JavaScript source files to be linked with the Haskell files (only for JavaScript targets).

A list of extra libraries to link with (when not linking fully static executables).

A list of extra libraries to link with (when linking fully static executables).

A list of extra libraries to be used instead of ‘extra-libraries’ when the package is loaded with GHCi.

Since

Cabal 2.2

A list of libraries that are supposed to be copied from the build directory alongside the produced Haskell libraries. Note that you are under the obligation to produce those libraries in the build directory (e.g. via a custom setup). Libraries listed here will be included when `copy`\-ing packages and be listed in the `hs-libraries` of the package configuration in the package database. Library names must either be prefixed with “HS” or “C” and corresponding library file names must match:

> -   Libraries with name “HS<library-name>”:
>     
>     -   libHS<library-name>.a
>         
>     -   libHS<library-name>-ghc<ghc-flavour><ghc-version>.<dyn-library-extension>\*
>         
>     
> -   Libraries with name “C<library-name>”:
>     
>     -   libC<library-name>.a
>         
>     -   lib<library-name>.<dyn-library-extension>\*
>         
>     

A list of directories to search for libraries (when not linking fully static executables).

A list of directories to search for libraries (when linking fully static executables).

TBW

TBW

`cc-options``:` _token list_[¶][230]

Command-line arguments to be passed to the C compiler. Since the arguments are compiler-dependent, this field is more useful with the setup described in the section on [system-dependent parameters][231].

`cpp-options``:` _token list_[¶][232]

Command-line arguments for pre-processing Haskell code. Applies to Haskell source and other pre-processed Haskell source like .hsc .chs. Does not apply to C code, that’s what cc-options is for.

`cxx-options``:` _token list_[¶][233]

Since

Cabal 2.2

Command-line arguments to be passed to the compiler when compiling C++ code. The C++ sources to which these command-line arguments should be applied can be specified with the [`cxx-sources`][234] field. Command-line options for C and C++ can be passed separately to the compiler when compiling both C and C++ sources by segregating the C and C++ sources with the [`c-sources`][235] and [`cxx-sources`][236] fields respectively, and providing different command-line arguments with the [`cc-options`][237] and the [`cxx-options`][238] fields.

`cmm-options``:` _token list_[¶][239]

Since

Cabal 3.0

Command-line arguments to be passed to the compiler when compiling C– code. See also [`cmm-sources`][240].

`asm-options``:` _token list_[¶][241]

Since

Cabal 3.0

Command-line arguments to be passed to the assembler when compiling assembler code. See also [`asm-sources`][242].

`ld-options``:` _token list_[¶][243]

Command-line arguments to be passed to the linker. Since the arguments are compiler-dependent, this field is more useful with the setup described in the section on [system-dependent parameters][244].

`hsc2hs-options``:` _token list_[¶][245]

`:` _since 3.6_[¶][246]

Command-line arguments to be passed to `hsc2hs`.

`pkgconfig-depends``:` _package list_[¶][247]

A list of [pkg-config][248] packages, needed to build this package. They can be annotated with versions, e.g. `gtk+-2.0 >= 2.10, cairo >= 1.0`. If no version constraint is specified, any version is assumed to be acceptable. Cabal uses `pkg-config` to find if the packages are available on the system and to find the extra compilation and linker options needed to use the packages.

If you need to bind to a C library that supports `pkg-config` then it is much preferable to use this field rather than hard code options into the other fields. `pkg-config --list-all` will show you all supported libraries. Depending on your system you may need to adjust `PKG_CONFIG_PATH`.

`frameworks``:` _token list_[¶][249]

On Darwin/MacOS X, a list of frameworks to link to. See Apple’s developer documentation for more details on frameworks. This entry is ignored on all other platforms.

Since

Cabal 1.24

On Darwin/MacOS X, a list of directories to search for frameworks. This entry is ignored on all other platforms.

`mixins``:` _mixin list_[¶][250]

Since

Cabal 2.0

Supported only in GHC 8.2 and later. A list of packages mentioned in the [`build-depends`][251] field, each optionally accompanied by a list of module and module signature renamings. A valid mixin obeys the following syntax:

Mixin ::\= PackageName IncludeRenaming
IncludeRenaming ::\= ModuleRenaming { "requires" ModuleRenaming }
ModuleRenaming ::\=
    {\- empty -}
  | "(" Renaming "," ... "," Renaming ")"
  | "hiding" "(" ModuleName "," ... "," ModuleName ")"
Renaming ::\=
    ModuleName
  | ModuleName "as" ModuleName

The simplest mixin syntax is simply the name of a package mentioned in the [`build-depends`][252] field. For example:

library
 build-depends:
    foo ^>= 1.2.3
 mixins:
    foo

But this doesn’t have any effect. More interesting is to use the mixin entry to rename one or more modules from the package, like this:

library
 mixins:
    foo (Foo.Bar as AnotherFoo.Bar, Foo.Baz as AnotherFoo.Baz)

Note that renaming a module like this will hide all the modules that are not explicitly named.

Modules can also be hidden:

library:
 mixins:
    foo hiding (Foo.Bar)

Hiding modules exposes everything that is not explicitly hidden.

Note

The current version of Cabal suffers from an infelicity in how the entries of [`mixins`][253] are parsed: an entry will fail to parse if the provided renaming clause has whitespace after the opening parenthesis. This will be fixed in future versions of Cabal.

See issues [#5150][254], [#4864][255], and [#5293][256].

There can be multiple mixin entries for a given package, in effect creating multiple copies of the dependency:

library
 mixins:
    foo (Foo.Bar as AnotherFoo.Bar, Foo.Baz as AnotherFoo.Baz),
    foo (Foo.Bar as YetAnotherFoo.Bar)

The `requires` clause is used to rename the module signatures required by a package:

library
 mixins:
    foo (Foo.Bar as AnotherFoo.Bar) requires (Foo.SomeSig as AnotherFoo.SomeSig)

Signature-only packages don’t have any modules, so only the signatures can be renamed, with the following syntax:

library
 mixins:
    sigonly requires (SigOnly.SomeSig as AnotherSigOnly.SomeSig)

See the [`library:signatures`][257] field for more details.

Mixin packages are part of the [Backpack][258] extension to the Haskell module system.

The matching of the module signatures required by a [`build-depends`][259] dependency with the implementation modules present in another dependency is triggered by a coincidence of names. When the names of the signature and of the implementation are already the same, the matching is automatic. But when the names don’t coincide, or we want to instantiate a signature in two different ways, adding mixin entries that perform renamings becomes necessary.

Warning

[Backpack][260] has the limitation that implementation modules that instantiate signatures required by a [`build-depends`][261] dependency can’t reside in the same component that has the dependency. They must reside in a different package dependency, or at least in a separate internal library.

### 7.2.10. Configurations[¶][262]

Library and executable sections may include conditional blocks, which test for various system parameters and configuration flags. The flags mechanism is rather generic, but most of the time a flag represents certain feature, that can be switched on or off by the package user. Here is an example package description file using configurations:

#### 7.2.10.1. Example: A package containing a library and executable programs[¶][263]

Name: Test1
Version: 0.0.1
Cabal-Version: \>= 1.8
License: BSD3
Author:  Jane Doe
Synopsis: Test package to test configurations
Category: Example
Build-Type: Simple

Flag Debug
 Description: Enable debug support
 Default:     False
 Manual:      True

Flag WebFrontend
 Description: Include API for web frontend.
 Default:     False
 Manual:      True

Flag NewDirectory
 description: Whether to build against @directory \>= 1.2@
 \-- This is an automatic flag which the solver will
 \-- assign automatically while searching for a solution

Library
 Build-Depends:   base \>= 4.2 && < 4.9
 Exposed-Modules: Testing.Test1
 Extensions:      CPP

 GHC-Options: -Wall
  if flag(Debug)
 CPP-Options: -DDEBUG
    if !os(windows)
 CC-Options: "-DDEBUG"
    else
 CC-Options: "-DNDEBUG"

  if flag(WebFrontend)
 Build-Depends: cgi \>= 0.42 && < 0.44
 Other-Modules: Testing.WebStuff
 CPP-Options: -DWEBFRONTEND

    if flag(NewDirectory)
 build-depends: directory \>= 1.2 && < 1.4
 Build-Depends: time \>= 1.0 && < 1.9
    else
 build-depends: directory \== 1.1.\*
 Build-Depends: old-time \>= 1.0 && < 1.2

Executable test1
 Main-is: T1.hs
 Other-Modules: Testing.Test1
 Build-Depends: base \>= 4.2 && < 4.9

  if flag(debug)
 CC-Options: "-DDEBUG"
 CPP-Options: -DDEBUG

#### 7.2.10.2. Layout[¶][264]

Flags, conditionals, library and executable sections use layout to indicate structure. This is very similar to the Haskell layout rule. Entries in a section have to all be indented to the same level which must be more than the section header. Tabs are not allowed to be used for indentation.

As an alternative to using layout you can also use explicit braces `{}`. In this case the indentation of entries in a section does not matter, though different fields within a block must be on different lines. Here is a bit of the above example again, using braces:

#### 7.2.10.3. Example: Using explicit braces rather than indentation for layout[¶][265]

Name: Test1
Version: 0.0.1
Cabal-Version: \>= 1.8
License: BSD3
Author:  Jane Doe
Synopsis: Test package to test configurations
Category: Example
Build-Type: Simple

Flag Debug {
 Description: Enable debug support
 Default:     False
 Manual:      True
}

Library {
 Build-Depends:   base \>= 4.2 && < 4.9
 Exposed-Modules: Testing.Test1
 Extensions:      CPP
  if flag(debug) {
 CPP-Options: -DDEBUG
    if !os(windows) {
 CC-Options: "-DDEBUG"
    } else {
 CC-Options: "-DNDEBUG"
    }
  }
}

#### 7.2.10.4. Configuration Flags[¶][266]

`flag` _name_[¶][267]

Flag section declares a flag which can be used in [conditional blocks][268].

Flag names are case-insensitive and must match `[[:alnum:]_][[:alnum:]_-]*` regular expression, or expressed as [ABNF][269]:

flag-name \= (UALNUM / "\_") \*(UALNUM / "\_" / "-")

UALNUM \= UALPHA / DIGIT
UALPHA \= ... ; set of alphabetic Unicode code-points

Note

Hackage accepts ASCII-only flags, `[a-zA-Z0-9_][a-zA-Z0-9_-]*` regexp.

`description``:` _freeform_[¶][270]

The description of this flag.

`default``:` _boolean_[¶][271]

Default value

`True`

The default value of this flag.

Note

This value may be [overridden in several ways][272]. The rationale for having flags default to True is that users usually want new features as soon as they are available. Flags representing features that are not (yet) recommended for most users (such as experimental features or debugging support) should therefore explicitly override the default to False.

`manual``:` _boolean_[¶][273]

Default value

`False`

Since

1.6

By default, Cabal will first try to satisfy dependencies with the default flag value and then, if that is not possible, with the negated value. However, if the flag is manual, then the default value (which can be overridden by commandline flags) will be used.

### 7.2.11. Conditional Blocks[¶][274]

Conditional blocks may appear anywhere inside a library or executable section. They have to follow rather strict formatting rules. Conditional blocks must always be of the shape

if condition
   property-descriptions-or-conditionals

or

if condition
     property-descriptions-or-conditionals
else
     property-descriptions-or-conditionals

Note that the `if` and the condition have to be all on the same line.

Since Cabal 2.2 conditional blocks support `elif` construct.

if condition1
     property-descriptions-or-conditionals
elif condition2
     property-descriptions-or-conditionals
else
     property-descriptions-or-conditionals

#### 7.2.11.1. Conditions[¶][275]

Conditions can be formed using boolean tests and the boolean operators `||` (disjunction / logical “or”), `&&` (conjunction / logical “and”), or `!` (negation / logical “not”). The unary `!` takes highest precedence, `||` takes lowest. Precedence levels may be overridden through the use of parentheses. For example, `os(darwin) && !arch(i386) || os(freebsd)` is equivalent to `(os(darwin) && !(arch(i386))) || os(freebsd)`.

The following tests are currently supported.

`os(_name_)`

Tests if the current operating system is _name_. The argument is tested against `System.Info.os` on the target system. There is unfortunately some disagreement between Haskell implementations about the standard values of `System.Info.os`. Cabal canonicalises it so that in particular `os(windows)` works on all implementations. If the canonicalised os names match, this test evaluates to true, otherwise false. The match is case-insensitive.

`arch(_name_)`

Tests if the current architecture is _name_. The argument is matched against `System.Info.arch` on the target system. If the arch names match, this test evaluates to true, otherwise false. The match is case-insensitive.

`impl(_compiler_)`

Tests for the configured Haskell implementation. An optional version constraint may be specified (for example `impl(ghc >= 6.6.1)`). If the configured implementation is of the right type and matches the version constraint, then this evaluates to true, otherwise false. The match is case-insensitive.

Note that including a version constraint in an `impl` test causes it to check for two properties:

-   The current compiler has the specified name, and
    
-   The compiler’s version satisfied the specified version constraint
    

As a result, `!impl(ghc >= x.y.z)` is not entirely equivalent to `impl(ghc < x.y.z)`. The test `!impl(ghc >= x.y.z)` checks that:

-   The current compiler is not GHC, or
    
-   The version of GHC is earlier than version x.y.z.
    

`flag(_name_)`

Evaluates to the current assignment of the flag of the given name. Flag names are case insensitive. Testing for flags that have not been introduced with a flag section is an error.

`true`

Constant value true.

`false`

Constant value false.

#### 7.2.11.2. Resolution of Conditions and Flags[¶][276]

If a package descriptions specifies configuration flags the package user can [control these in several ways][277]. If the user does not fix the value of a flag, Cabal will try to find a flag assignment in the following way.

-   For each flag specified, it will assign its default value, evaluate all conditions with this flag assignment, and check if all dependencies can be satisfied. If this check succeeded, the package will be configured with those flag assignments.
    
-   If dependencies were missing, the last flag (as by the order in which the flags were introduced in the package description) is tried with its alternative value and so on. This continues until either an assignment is found where all dependencies can be satisfied, or all possible flag assignments have been tried.
    

To put it another way, Cabal does a complete backtracking search to find a satisfiable package configuration. It is only the dependencies specified in the [`build-depends`][278] field in conditional blocks that determine if a particular flag assignment is satisfiable ([`build-tools`][279] are not considered). The order of the declaration and the default value of the flags determines the search order. Flags overridden on the command line fix the assignment of that flag, so no backtracking will be tried for that flag.

If no suitable flag assignment could be found, the configuration phase will fail and a list of missing dependencies will be printed. Note that this resolution process is exponential in the worst case (i.e., in the case where dependencies cannot be satisfied). There are some optimizations applied internally, but the overall complexity remains unchanged.

### 7.2.12. Meaning of field values when using conditionals[¶][280]

During the configuration phase, a flag assignment is chosen, all conditionals are evaluated, and the package description is combined into a flat package descriptions. If the same field is declared both inside a conditional and outside then they are combined using the following rules.

-   Boolean fields are combined using conjunction (logical “and”).
    
-   List fields are combined by appending the inner items to the outer items, for example
    
    other-extensions: CPP
    if impl(ghc)
     other-extensions: MultiParamTypeClasses
    
    when compiled using GHC will be combined to
    
    other-extensions: CPP, MultiParamTypeClasses
    
    Similarly, if two conditional sections appear at the same nesting level, properties specified in the latter will come after properties specified in the former.
    
-   All other fields must not be specified in ambiguous ways. For example
    
    Main-is: Main.hs
    if flag(useothermain)
     Main-is: OtherMain.hs
    
    will lead to an error. Instead use
    
    if flag(useothermain)
     Main-is: OtherMain.hs
    else
     Main-is: Main.hs
    

### 7.2.13. Common stanzas[¶][281]

`common` _name_[¶][282]

Since

Cabal 2.2

Starting with Cabal-2.2 it’s possible to use common build info stanzas.

common deps
 build-depends: base ^>= 4.11
 ghc-options: -Wall

common test-deps
 build-depends: tasty ^>= 0.12.0.1

library
 import: deps
 exposed-modules: Foo

test-suite tests
 import: deps, test-deps
 type: exitcode-stdio-1.0
 main-is: Tests.hs
 build-depends: foo

-   You can use [build information][283] fields in common stanzas.
    
-   Common stanzas must be defined before use.
    
-   Common stanzas can import other common stanzas.
    
-   You can import multiple stanzas at once. Stanza names must be separated by commas.
    
-   `import` must be the first field in a section. Since Cabal 3.0 imports are also allowed inside conditionals.
    

Note

The name import was chosen, because there is `includes` field.

`import``:` _token-list_[¶][284]

TBW

### 7.2.14. Source Repositories[¶][285]

`source-repository` [¶][286]

Since

Cabal 1.6

It is often useful to be able to specify a source revision control repository for a package. Cabal lets you specify this information in a relatively structured form which enables other tools to interpret and make effective use of the information. For example the information should be sufficient for an automatic tool to checkout the sources.

Cabal supports specifying different information for various common source control systems. Obviously not all automated tools will support all source control systems.

Cabal supports specifying repositories for different use cases. By declaring which case we mean automated tools can be more useful. There are currently two kinds defined:

-   The `head` kind refers to the latest development branch of the package. This may be used for example to track activity of a project or as an indication to outside developers what sources to get for making new contributions.
    
-   The `this` kind refers to the branch and tag of a repository that contains the sources for this version or release of a package. For most source control systems this involves specifying a tag, id or hash of some form and perhaps a branch. The purpose is to be able to reconstruct the sources corresponding to a particular package version. This might be used to indicate what sources to get if someone needs to fix a bug in an older branch that is no longer an active head branch.
    

You can specify one kind or the other or both. As an example here are the repositories for the Cabal library. Note that the `this` kind of repository specifies a tag.

source-repository head
 type:     darcs
 location: http://darcs.haskell.org/cabal/

source-repository this
 type:     darcs
 location: http://darcs.haskell.org/cabal-branches/cabal-1.6/
 tag:      1.6.1

The exact fields are as follows:

`type``:` _token_[¶][287]

The name of the source control system used for this repository. The currently recognised types are:

-   `darcs`
    
-   `git`
    
-   `svn`
    
-   `cvs`
    
-   `mercurial` (or alias `hg`)
    
-   `bazaar` (or alias `bzr`)
    
-   `arch`
    
-   `monotone`
    

This field is required.

`location``:` _URL_[¶][288]

The location of the repository. The exact form of this field depends on the repository type. For example:

-   for darcs: `http://code.haskell.org/foo/`
    
-   for git: `git://github.com/foo/bar.git`
    
-   for CVS: `anoncvs@cvs.foo.org:/cvs`
    

This field is required.

`module``:` _token_[¶][289]

CVS requires a named module, as each CVS server can host multiple named repositories.

This field is required for the CVS repository type and should not be used otherwise.

`branch``:` _token_[¶][290]

Many source control systems support the notion of a branch, as a distinct concept from having repositories in separate locations. For example CVS, SVN and git use branches while darcs uses different locations for different branches. If you need to specify a branch to identify a your repository then specify it in this field.

This field is optional.

`tag``:` _token_[¶][291]

A tag identifies a particular state of a source repository. The tag can be used with a `this` repository kind to identify the state of a repository corresponding to a particular package version or release. The exact form of the tag depends on the repository type.

This field is required for the `this` repository kind.

`subdir``:` _directory_[¶][292]

Some projects put the sources for multiple packages under a single source repository. This field lets you specify the relative path from the root of the repository to the top directory for the package, i.e. the directory containing the package’s `.cabal` file.

This field is optional. It defaults to empty which corresponds to the root directory of the repository.

### 7.2.15. Downloading a package’s source[¶][293]

The `cabal get` command allows to access a package’s source code - either by unpacking a tarball downloaded from Hackage (the default) or by checking out a working copy from the package’s source repository.

$ cabal get \[FLAGS\] PACKAGES

The `get` command supports the following options:

`-d --destdir` _PATH_

Where to place the package source, defaults to (a subdirectory of) the current directory.

`-s --source-repository` _\[head|this|…\]_

Fork the package’s source repository using the appropriate version control system. The optional argument allows to choose a specific repository kind.

`--index-state` _\[HEAD|@<unix-timestamp>|<iso8601-utc-timestamp>\]_

Use source package index state as it existed at a previous time. Accepts unix-timestamps (e.g. `@1474732068`), ISO8601 UTC timestamps (e.g. `2016-09-24T17:47:48Z`), or `HEAD` (default). This determines which package versions are available as well as which `.cabal` file revision is selected (unless `--pristine` is used).

`--pristine`

Unpack the original pristine tarball, rather than updating the `.cabal` file with the latest revision from the package archive.

## 7.3. Custom setup scripts[¶][294]

Since Cabal 1.24, custom `Setup.hs` are required to accurately track their dependencies by declaring them in the `.cabal` file rather than rely on dependencies being implicitly in scope. Please refer to [this article][295] for more details.

As of Cabal library version 3.0, `defaultMain*` variants implement support for response files. Custom `Setup.hs` files that do not use one of these main functions are required to implement their own support, such as by using `GHC.ResponseFile.getArgsWithResponseFiles`.

Declaring a `custom-setup` stanza also enables the generation of `MIN_VERSION_package_(A,B,C)` CPP macros for the Setup component.

`custom-setup` [¶][296]

Since

Cabal 1.24

The optional [`custom-setup`][297] stanza contains information needed for the compilation of custom `Setup.hs` scripts,

custom-setup
 setup-depends:
    base  \>= 4.5 && < 4.11,
    Cabal \>= 1.14 && < 1.25

`setup-depends``:` _package list_[¶][298]

Since

Cabal 1.24

The dependencies needed to compile `Setup.hs`. See the [`build-depends`][299] field for a description of the syntax expected by this field.

If the field is not specified the implicit package set will be used. The package set contains packages bundled with GHC (i.e. `base`, `bytestring`) and specifically `Cabal`. The specific bounds are put on `Cabal` dependency: lower-bound is inferred from [`cabal-version`][300], and the upper-bound is `< 1.25`.

`Cabal` version is additionally restricted by GHC, with absolute minimum being `1.20`, and for example `Custom` builds with GHC-8.10 require at least `Cabal-3.2`.

### 7.3.1. Backward compatibility and `custom-setup`[¶][301]

Versions prior to Cabal 1.24 don’t recognise `custom-setup` stanzas, and will behave agnostic to them (except for warning about an unknown section). Consequently, versions prior to Cabal 1.24 can’t ensure the declared dependencies `setup-depends` are in scope, and instead whatever is registered in the current package database environment will become eligible (and resolved by the compiler) for the `Setup.hs` module.

The availability of the `MIN_VERSION_package_(A,B,C)` CPP macros inside `Setup.hs` scripts depends on the condition that either

-   a `custom-setup` section has been declared (or `cabal v2-build` is being used which injects an implicit hard-coded `custom-setup` stanza if it’s missing), or
    
-   GHC 8.0 or later is used (which natively injects package version CPP macros)
    

Consequently, if you need to write backward compatible `Setup.hs` scripts using CPP, you should declare a `custom-setup` stanza and use the pattern below:

{-# LANGUAGE CPP #-}
import Distribution.Simple

#if defined(MIN\_VERSION\_Cabal)
\-- version macros are available and can be used as usual
# if MIN\_VERSION\_Cabal(a,b,c)
\-- code specific to lib:Cabal >= a.b.c
# else
\-- code specific to lib:Cabal < a.b.c
# endif
#else
# warning Enabling heuristic fall\-back. Please upgrade cabal\-install to 1.24 or later if Setup.hs fails to compile.

\-- package version macros not available; except for exotic environments,
\-- you can heuristically assume that lib:Cabal's version is correlated
\-- with \_\_GLASGOW\_HASKELL\_\_, and specifically since we can assume that
\-- GHC < 8.0, we can assume that lib:Cabal is version 1.22 or older.
#endif

main \= ...

The simplified (heuristic) CPP pattern shown below is useful if all you need is to distinguish `Cabal < 2.0` from `Cabal >= 2.0`.

{-# LANGUAGE CPP #-}
import Distribution.Simple

#if !defined(MIN\_VERSION\_Cabal)
# define MIN\_VERSION\_Cabal(a,b,c) 0
#endif

#if MIN\_VERSION\_Cabal(2,0,0)
\-- code for lib:Cabal >= 2.0
#else
\-- code for lib:Cabal < 2.0
#endif

main \= ...

## 7.4. Autogenerated modules and includes[¶][302]

Modules that are built automatically at setup, created with a custom setup script, must appear on [`other-modules`][303] for the library, executable, test-suite or benchmark stanzas or also on [`library:exposed-modules`][304] for libraries to be used, but are not really on the package when distributed. This makes commands like sdist fail because the file is not found.

These special modules must appear again on the [`autogen-modules`][305] field of the stanza that is using them, besides [`other-modules`][306] or [`library:exposed-modules`][307]. With this there is no need to create complex build hooks for this poweruser case.

`autogen-modules``:` _module list_[¶][308]

Since

Cabal 2.0

Todo

document autogen-modules field

Right now [`executable:main-is`][309] modules are not supported on [`autogen-modules`][310].

Library
 default-language: Haskell2010
 build-depends: base
 exposed-modules:
        MyLibrary
        MyLibHelperModule
 other-modules:
        MyLibModule
 autogen-modules:
        MyLibHelperModule

Executable Exe
 default-language: Haskell2010
 main-is: Dummy.hs
 build-depends: base
 other-modules:
        MyExeModule
        MyExeHelperModule
 autogen-modules:
        MyExeHelperModule

`autogen-includes``:` _filename list_[¶][311]

Since

Cabal 3.0

A list of header files from this package which are autogenerated (e.g. by a `configure` script). Autogenerated header files are not packaged by `sdist` command.

## 7.5. Virtual modules[¶][312]

TBW

`virtual-modules``:` _module list_[¶][313]

Since

Cabal 2.2

TBW

## 7.6. Accessing data files from package code[¶][314]

The placement on the target system of files listed in the [`data-files`][315] field varies between systems, and in some cases one can even move packages around after installation (see [prefix independence][316]). To enable packages to find these files in a portable way, Cabal generates a module called `Paths__pkgname_` (with any hyphens in _pkgname_ replaced by underscores) during building, so that it may be imported by modules of the package. This module defines a function

getDataFileName :: FilePath \-> IO FilePath

If the argument is a filename listed in the [`data-files`][317] field, the result is the name of the corresponding file on the system on which the program is running.

Note

If you decide to import the `Paths__pkgname_` module then it _must_ be listed in the [`other-modules`][318] field just like any other module in your package and on [`autogen-modules`][319] as the file is autogenerated.

The `Paths__pkgname_` module is not platform independent, as any other autogenerated module, so it does not get included in the source tarballs generated by `sdist`.

The `Paths__pkgname_` module also includes some other useful functions and values, which record the version of the package and some other directories which the package has been configured to be installed into (e.g. data files live in `getDataDir`):

version :: Version

getBinDir :: IO FilePath
getLibDir :: IO FilePath
getDynLibDir :: IO FilePath
getDataDir :: IO FilePath
getLibexecDir :: IO FilePath
getSysconfDir :: IO FilePath

The actual location of all these directories can be individually overridden at runtime using environment variables of the form `pkg_name_var`, where `pkg_name` is the name of the package with all hyphens converted into underscores, and `var` is either `bindir`, `libdir`, `dynlibdir`, `datadir`, `libexedir` or `sysconfdir`. For example, the configured data directory for `pretty-show` is controlled with the `pretty_show_datadir` environment variable.

### 7.6.1. Accessing the package version[¶][320]

The aforementioned auto generated `Paths__pkgname_` module also exports the constant `version ::` [Version][321] which is defined as the version of your package as specified in the `version` field.

## 7.7. System-dependent parameters[¶][322]

For some packages, especially those interfacing with C libraries, implementation details and the build procedure depend on the build environment. The `build-type` `Configure` can be used to handle many such situations. In this case, `Setup.hs` should be:

import Distribution.Simple
main \= defaultMainWithHooks autoconfUserHooks

Most packages, however, would probably do better using the `Simple` build type and [configurations][323].

The [`build-type`][324] `Configure` differs from `Simple` in two ways:

-   The package root directory must contain a shell script called `configure`. The configure step will run the script. This `configure` script may be produced by [autoconf][325] or may be hand-written. The `configure` script typically discovers information about the system and records it for later steps, e.g. by generating system-dependent header files for inclusion in C source files and preprocessed Haskell source files. (Clearly this won’t work for Windows without MSYS or Cygwin: other ideas are needed.)
    
-   If the package root directory contains a file called _package_`.buildinfo` after the configuration step, subsequent steps will read it to obtain additional settings for [build information][326] fields,to be merged with the ones given in the `.cabal` file. In particular, this file may be generated by the `configure` script mentioned above, allowing these settings to vary depending on the build environment.
    

The build information file should have the following structure:

> _buildinfo_
> 
> `executable:` _name_ _buildinfo_
> 
> `executable:` _name_ _buildinfo_ …

where each _buildinfo_ consists of settings of fields listed in the section on [build information][327]. The first one (if present) relates to the library, while each of the others relate to the named executable. (The names must match the package description, but you don’t have to have entries for all of them.)

Neither of these files is required. If they are absent, this setup script is equivalent to `defaultMain`.

### 7.7.1. Example: Using autoconf[¶][328]

This example is for people familiar with the [autoconf][329] tools.

In the X11 package, the file `configure.ac` contains:

AC\_INIT(\[Haskell X11 package\], \[1.1\], \[libraries@haskell.org\], \[X11\])

\# Safety check: Ensure that we are in the correct source directory.
AC\_CONFIG\_SRCDIR(\[X11.cabal\])

\# Header file to place defines in
AC\_CONFIG\_HEADERS(\[include/HsX11Config.h\])

\# Check for X11 include paths and libraries
AC\_PATH\_XTRA
AC\_TRY\_CPP(\[#include <X11/Xlib.h>\],,\[no\_x=yes\])

\# Build the package if we found X11 stuff
if test "$no\_x" \= yes
then BUILD\_PACKAGE\_BOOL\=False
else BUILD\_PACKAGE\_BOOL\=True
fi
AC\_SUBST(\[BUILD\_PACKAGE\_BOOL\])

AC\_CONFIG\_FILES(\[X11.buildinfo\])
AC\_OUTPUT

Then the setup script will run the `configure` script, which checks for the presence of the X11 libraries and substitutes for variables in the file `X11.buildinfo.in`:

buildable: @BUILD\_PACKAGE\_BOOL@
cc-options: @X\_CFLAGS@
ld-options: @X\_LIBS@

This generates a file `X11.buildinfo` supplying the parameters needed by later stages:

buildable: True
cc-options:  -I/usr/X11R6/include
ld-options:  -L/usr/X11R6/lib

The `configure` script also generates a header file `include/HsX11Config.h` containing C preprocessor defines recording the results of various tests. This file may be included by C source files and preprocessed Haskell source files in the package.

Note

Packages using these features will also need to list additional files such as `configure`, templates for `.buildinfo` files, files named only in `.buildinfo` files, header files and so on in the [`extra-source-files`][330] field to ensure that they are included in source distributions. They should also list files and directories generated by `configure` in the [`extra-tmp-files`][331] field to ensure that they are removed by `setup clean`.

Quite often the files generated by `configure` need to be listed somewhere in the package description (for example, in the [`install-includes`][332] field). However, we usually don’t want generated files to be included in the source tarball. The solution is again provided by the `.buildinfo` file. In the above example, the following line should be added to `X11.buildinfo`:

install-includes: HsX11Config.h

In this way, the generated `HsX11Config.h` file won’t be included in the source tarball in addition to `HsX11Config.h.in`, but it will be copied to the right location during the install process. Packages that use custom `Setup.hs` scripts can update the necessary fields programmatically instead of using the `.buildinfo` file.

## 7.8. Conditional compilation[¶][333]

Sometimes you want to write code that works with more than one version of a dependency. You can specify a range of versions for the dependency in the [`build-depends`][334], but how do you then write the code that can use different versions of the API?

Haskell lets you preprocess your code using the C preprocessor (either the real C preprocessor, or `cpphs`). To enable this, add `extensions: CPP` to your package description. When using CPP, Cabal provides some pre-defined macros to let you test the version of dependent packages; for example, suppose your package works with either version 3 or version 4 of the `base` package, you could select the available version in your Haskell modules like this:

#if MIN\_VERSION\_base(4,0,0)
... code that works with base\-4 ...
#else
... code that works with base\-3 ...
#endif

In general, Cabal supplies a macro `MIN_VERSION_`_\`\`package\`\`_`_(A,B,C)` for each package depended on via [`build-depends`][335]. This macro is true if the actual version of the package in use is greater than or equal to `A.B.C` (using the conventional ordering on version numbers, which is lexicographic on the sequence, but numeric on each component, so for example 1.2.0 is greater than 1.0.3).

Since version 1.20, the `MIN_TOOL_VERSION_`_\`\`tool\`\`_ family of macros lets you condition on the version of build tools used to build the program (e.g. `hsc2hs`).

Since version 1.24, the macro `CURRENT_COMPONENT_ID`, which expands to the string of the component identifier that uniquely identifies this component. Furthermore, if the package is a library, the macro `CURRENT_PACKAGE_KEY` records the identifier that was passed to GHC for use in symbols and for type equality.

Since version 2.0, the macro `CURRENT_PACKAGE_VERSION` expands to the string version number of the current package.

Cabal places the definitions of these macros into an automatically-generated header file, which is included when preprocessing Haskell source code by passing options to the C preprocessor.

Cabal also allows to detect when the source code is being used for generating documentation. The `__HADDOCK_VERSION__` macro is defined only when compiling via [Haddock][336] instead of a normal Haskell compiler. The value of the `__HADDOCK_VERSION__` macro is defined as `A*1000 + B*10 + C`, where `A.B.C` is the Haddock version. This can be useful for working around bugs in Haddock or generating prettier documentation in some special cases.

## 7.9. More complex packages[¶][337]

For packages that don’t fit the simple schemes described above, you have a few options:

-   By using the [`build-type`][338] `Custom`, you can supply your own `Setup.hs` file, and customize the simple build infrastructure using _hooks_. These allow you to perform additional actions before and after each command is run, and also to specify additional preprocessors. A typical `Setup.hs` may look like this:
    
    import Distribution.Simple
    main \= defaultMainWithHooks simpleUserHooks { postHaddock \= posthaddock }
    
    posthaddock args flags desc info \= ....
    
    See `UserHooks` in [Distribution.Simple][339] for the details, but note that this interface is experimental, and likely to change in future releases.
    
    If you use a custom `Setup.hs` file you should strongly consider adding a [`custom-setup`][340] stanza with a [`custom-setup:setup-depends`][341] field to ensure that your setup script does not break with future dependency versions.
    
-   You could delegate all the work to `make`, though this is unlikely to be very portable. Cabal supports this with the [`build-type`][342] `Make` and a trivial setup library [Distribution.Make][343], which simply parses the command line arguments and invokes `make`. Here `Setup.hs` should look like this:
    
    import Distribution.Make
    main \= defaultMain
    
    The root directory of the package should contain a `configure` script, and, after that has run, a `Makefile` with a default target that builds the package, plus targets `install`, `register`, `unregister`, `clean`, `dist` and `docs`. Some options to commands are passed through as follows:
    
    -   The `--with-hc-pkg`, `--prefix`, `--bindir`, `--libdir`, `--dynlibdir`, `--datadir`, `--libexecdir` and `--sysconfdir` options to the `configure` command are passed on to the `configure` script. In addition the value of the `--with-compiler` option is passed in a `--with-hc` option and all options specified with `--configure-option=` are passed on.
        
    -   The `--destdir` option to the `copy` command becomes a setting of a `destdir` variable on the invocation of `make copy`. The supplied `Makefile` should provide a `copy` target, which will probably look like this:
        
        copy :
                $(MAKE) install prefix\=$(destdir)/$(prefix) \\
                                bindir\=$(destdir)/$(bindir) \\
                                libdir\=$(destdir)/$(libdir) \\
                                dynlibdir\=$(destdir)/$(dynlibdir) \\
                                datadir\=$(destdir)/$(datadir) \\
                                libexecdir\=$(destdir)/$(libexecdir) \\
                                sysconfdir\=$(destdir)/$(sysconfdir) \\
        
-   Finally, with the [`build-type`][344] `Custom`, you can also write your own setup script from scratch. It must conform to the interface described in the section on [building and installing packages][345], and you may use the Cabal library for all or part of the work. One option is to copy the source of `Distribution.Simple`, and alter it for your needs. Good luck.
    

## 7.10. Backpack[¶][346]

Cabal and GHC jointly support Backpack, an extension to Haskell’s module system which makes it possible to parametrize a package over some modules, which can be instantiated later arbitrarily by a user. This means you can write a library to be agnostic over some data representation, and then instantiate it several times with different data representations. Like C++ templates, instantiated packages are recompiled for each instantiation, which means you do not pay any runtime cost for parametrizing packages in this way. Backpack modules are somewhat experimental; while fully supported by cabal-install, they are currently [not supported by Stack][347].

A Backpack package is defined by use of the [`library:signatures`][348] field, or by (transitive) dependency on a package that defines some requirements. To define a parametrized package, define a signature file (file extension `hsig`) that specifies the signature of the module you want to parametrize over, and add it to your Cabal file in the [`library:signatures`][349] field.

.hsig[¶][350]

signature Str where

data Str

concat :: \[Str\] \-> Str

parametrized.cabal[¶][351]

cabal-version: 2.2
name: parametrized

library
 build-depends: base
 signatures: Str
 exposed-modules: MyModule

You can define any number of regular modules (e.g., `MyModule`) that import signatures and use them as regular modules.

If you are familiar with ML modules, you might now expect there to be some way to apply the parametrized package with an implementation of the `Str` module to get a concrete instantiation of the package. Backpack operates slightly differently with a concept of _mix-in linking_, where you provide an implementation of `Str` simply by bringing another module into scope with the same name as the requirement. For example, if you had a package `str-impl` that provided a module named `Str`, instantiating `parametrized` is as simple as just depending on both `str-impl` and `parametrized`:

combined.cabal[¶][352]

cabal-version: 2.2
name: combined

library
 build-depends: base, str-impl, parametrized

Note that due to technical limitations, you cannot directly define `Str` in the `combined` library; it must be placed in its own library (you can use [Internal Libraries][353] to conveniently define a sub-library).

However, a more common situation is that your names don’t match up exactly. The [`library:mixins`][354] field can be used to rename signatures and modules to line up names as necessary. If you have a requirement `Str` and an implementation `Data.Text`, you can line up the names in one of two ways:

-   Rename the requirement to match the implementation: `mixins: parametrized requires (Str as Data.Text)`
    
-   Rename the implementation to match the requirement: `mixins: text (Data.Text as Str)`
    

The [`library:mixins`][355] field can also be used to disambiguate between multiple instantiations of the same package; for each instantiation of the package, give it a separate entry in mixins with the requirements and provided modules renamed to be distinct.

.cabal[¶][356]

cabal-version: 2.2
name: double-combined

library
 build-depends: base, text, bytestring, parametrized
 mixins:
    parametrized (MyModule as MyModule.Text) requires (Str as Data.Text),
    parametrized (MyModule as MyModule.BS) requires (Str as Data.ByteString)

Intensive use of Backpack sometimes involves creating lots of small parametrized libraries; [Internal Libraries][357] can be used to define all of these libraries in a single package without having to create many separate Cabal packages. You may also find it useful to use [`library:reexported-modules`][358] to reexport instantiated libraries to Backpack-unware users (e.g., Backpack can be used entirely as an implementation detail.)

Backpack imposes a limitation on Template Haskell that goes beyond the usual TH stage restriction: it’s not possible to splice TH code imported from a compilation unit that is still “indefinite”, that is, a unit for which some module signatures still haven’t been matched with implementations. The reason is that indefinite units are typechecked, but not compiled, so there’s no actual TH code to run while splicing. Splicing TH code from a definite compilation unit into an indefinite one works normally.

For more information about Backpack, check out the [GHC wiki page][359].

Footnotes

[1][360]

Some packages (ab)use [`build-depends`][361] on old-style builds, but this has a few major drawbacks:

> -   using Nix-style builds it’s considered an error if you depend on a exe-only package via build-depends: the solver will refuse it.
>     
> -   it may or may not place the executable on `$PATH`.
>     
> -   it does not ensure the correct version of the package is installed, so you might end up overwriting versions with each other.
>     

[1]: https://cabal.readthedocs.io/en/latest/cabal-package.html#creating-a-package "Permalink to this headline"
[2]: https://cabal.readthedocs.io/en/latest/cabal-package.html#package-descriptions
[3]: https://cabal.readthedocs.io/en/latest/installing-packages.html#installing-packages
[4]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "package.cabal build-type field"
[5]: https://cabal.readthedocs.io/en/latest/installing-packages.html#installing-packages
[6]: https://cabal.readthedocs.io/en/latest/cabal-package.html#example-a-package-containing-a-simple-library "Permalink to this headline"
[7]: https://cabal.readthedocs.io/en/latest/cabal-package.html#example-a-package-containing-executable-programs "Permalink to this headline"
[8]: https://cabal.readthedocs.io/en/latest/cabal-package.html#example-a-package-containing-a-library-and-executable-programs "Permalink to this headline"
[9]: https://cabal.readthedocs.io/en/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html
[10]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id7
[11]: https://cabal.readthedocs.io/en/latest/morecomplexpackages
[12]: https://cabal.readthedocs.io/en/latest/cabal-package.html#package-descriptions "Permalink to this headline"
[13]: https://cabal.readthedocs.io/en/latest/cabal-package.html#package-properties
[14]: https://cabal.readthedocs.io/en/latest/cabal-package.html#configurations
[15]: https://cabal.readthedocs.io/en/latest/cabal-package.html#library
[16]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[17]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[18]: https://cabal.readthedocs.io/en/latest/cabal-package.html#modules-and-preprocessors "Permalink to this headline"
[19]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-exposed-modules "package.cabal library section exposed-modules: field"
[20]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-modules "package.cabal other-modules field"
[21]: http://hackage.haskell.org/package/greencard
[22]: http://hackage.haskell.org/package/c2hs
[23]: http://hackage.haskell.org/package/hsc2hs
[24]: http://www.haskell.org/happy/
[25]: http://www.haskell.org/alex/
[26]: http://projects.haskell.org/cpphs/
[27]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[28]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[29]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[30]: https://cabal.readthedocs.io/en/latest/cabal-package.html#package-properties "Permalink to this headline"
[31]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-name "Permalink to this definition"
[32]: https://cabal.readthedocs.io/en/latest/cabal-package.html#package-descriptions
[33]: https://tools.ietf.org/html/rfc5234
[34]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-version "Permalink to this definition"
[35]: https://tools.ietf.org/html/rfc5234
[36]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "Permalink to this definition"
[37]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html#spec-history
[38]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"
[39]: https://tools.ietf.org/html/rfc5234
[40]: https://github.com/haskell/cabal/issues/4899
[41]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "Permalink to this definition"
[42]: https://cabal.readthedocs.io/en/release/cabal-latest/doc/API/Cabal/Distribution-PackageDescription.html#t:BuildType
[43]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"
[44]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-custom-setup-custom-setup "package.cabal custom-setup section (since version: 1.24)"
[45]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"
[46]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id7
[47]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id8
[48]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license "Permalink to this definition"
[49]: https://spdx.org/ids-how
[50]: https://spdx.org/licenses/
[51]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"
[52]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license "package.cabal license field"
[53]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license-file "Permalink to this definition"
[54]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license-files "package.cabal license-files field(since version: 1.20)"
[55]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license-files "Permalink to this definition"
[56]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license-files "package.cabal license-files field(since version: 1.20)"
[57]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-license-file "package.cabal license-file field"
[58]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-copyright "Permalink to this definition"
[59]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-maintainer "Permalink to this definition"
[60]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-stability "Permalink to this definition"
[61]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-homepage "Permalink to this definition"
[62]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-bug-reports "Permalink to this definition"
[63]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-package-url "Permalink to this definition"
[64]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-synopsis "Permalink to this definition"
[65]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-description "Permalink to this definition"
[66]: https://cabal.readthedocs.io/en/latest/setup-commands.html#setup-haddock
[67]: http://www.haskell.org/haddock/
[68]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-category "Permalink to this definition"
[69]: http://hackage.haskell.org/
[70]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-tested-with "Permalink to this definition"
[71]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-files "Permalink to this definition"
[72]: https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code
[73]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-dir "Permalink to this definition"
[74]: https://cabal.readthedocs.io/en/latest/setup-commands.html#setup-sdist
[75]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-files "package.cabal data-files field"
[76]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-files "package.cabal data-files field"
[77]: https://cabal.readthedocs.io/en/latest/setup-commands.html#setup-clean
[78]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id7
[79]: https://cabal.readthedocs.io/en/latest/cabal-package.html#library "Permalink to this headline"
[80]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-library-library "Permalink to this definition"
[81]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-name "package.cabal name field"
[82]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-library-library "package.cabal library section "
[83]: https://cabal.readthedocs.io/en/latest/cabal-package.html#sublibs
[84]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-exposed-modules "Permalink to this definition"
[85]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-virtual-modules "Permalink to this definition"
[86]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-exposed "Permalink to this definition"
[87]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-visibility "Permalink to this definition"
[88]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-reexported-modules "Permalink to this definition"
[89]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures "Permalink to this definition"
[90]: https://downloads.haskell.org/~ghc/master/users-guide/separate_compilation.html#module-signatures
[91]: https://cabal.readthedocs.io/en/latest/cabal-package.html#backpack
[92]: https://wiki.haskell.org/Module_signature#How_to_use_a_signature_package
[93]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[94]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[95]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[96]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[97]: https://cabal.readthedocs.io/en/latest/cabal-package.html#opening-an-interpreter-session "Permalink to this headline"
[98]: http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop
[99]: https://cabal.readthedocs.io/en/latest/cabal-package.html#freezing-dependency-versions "Permalink to this headline"
[100]: https://cabal.readthedocs.io/en/latest/cabal-package.html#generating-dependency-version-bounds "Permalink to this headline"
[101]: http://pvp.haskell.org/
[102]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[103]: https://cabal.readthedocs.io/en/latest/cabal-package.html#listing-outdated-dependency-version-bounds "Permalink to this headline"
[104]: https://cabal.readthedocs.io/en/latest/cabal-package.html#executables "Permalink to this headline"
[105]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-executable-executable "Permalink to this definition"
[106]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[107]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-executable-main-is "Permalink to this definition"
[108]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hs-source-dirs "package.cabal hs-source-dirs field"
[109]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-executable-scope "Permalink to this definition"
[110]: https://cabal.readthedocs.io/en/latest/cabal-package.html#running-executables "Permalink to this headline"
[111]: https://cabal.readthedocs.io/en/latest/cabal-package.html#test-suites "Permalink to this headline"
[112]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-test-suite-test-suite "Permalink to this definition"
[113]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[114]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-test-suite-type "Permalink to this definition"
[115]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-test-suite-main-is "Permalink to this definition"
[116]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hs-source-dirs "package.cabal hs-source-dirs field"
[117]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-test-suite-test-module "package.cabal test-suite section test-module: field"
[118]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-test-suite-test-module "Permalink to this definition"
[119]: https://cabal.readthedocs.io/en/latest/cabal-package.html#example-package-using-exitcode-stdio-1-0-interface "Permalink to this headline"
[120]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id10 "Permalink to this code"
[121]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id11 "Permalink to this code"
[122]: https://cabal.readthedocs.io/en/latest/cabal-package.html#example-package-using-detailed-0-9-interface "Permalink to this headline"
[123]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id12 "Permalink to this code"
[124]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id13 "Permalink to this code"
[125]: https://cabal.readthedocs.io/en/latest/cabal-package.html#running-test-suites "Permalink to this headline"
[126]: https://cabal.readthedocs.io/en/latest/cabal-package.html#benchmarks "Permalink to this headline"
[127]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-benchmark-benchmark "Permalink to this definition"
[128]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[129]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-benchmark-type "Permalink to this definition"
[130]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-benchmark-main-is "Permalink to this definition"
[131]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hs-source-dirs "package.cabal hs-source-dirs field"
[132]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id1 "Permalink to this headline"
[133]: https://cabal.readthedocs.io/en/latest/cabal-package.html#foo-bench-cabal "Permalink to this code"
[134]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id14 "Permalink to this code"
[135]: https://cabal.readthedocs.io/en/latest/cabal-package.html#running-benchmarks "Permalink to this headline"
[136]: https://cabal.readthedocs.io/en/latest/cabal-package.html#foreign-libraries "Permalink to this headline"
[137]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-foreign-library-foreign-library "Permalink to this definition"
[138]: https://cabal.readthedocs.io/en/latest/cabal-package.html#foreign-libraries
[139]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-foreign-library-type "Permalink to this definition"
[140]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-foreign-library-options "Permalink to this definition"
[141]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-foreign-library-mod-def-file "Permalink to this definition"
[142]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/win32-dlls.html
[143]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-foreign-library-lib-version-info "Permalink to this definition"
[144]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-foreign-library-lib-version-linux "Permalink to this definition"
[145]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information "Permalink to this headline"
[146]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id7
[147]: https://cabal.readthedocs.io/en/latest/cabal-package.html#configurations
[148]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "Permalink to this definition"
[149]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[150]: https://cabal.readthedocs.io/en/latest/cabal-package.html#sublibs
[151]: http://pvp.haskell.org/
[152]: http://pvp.haskell.org/
[153]: http://pvp.haskell.org/
[154]: http://pvp.haskell.org/
[155]: http://pvp.haskell.org/
[156]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-allow-newer "cabal.project allow-newer field "
[157]: http://pvp.haskell.org/
[158]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[159]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-modules "Permalink to this definition"
[160]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hs-source-dir "Permalink to this definition"
[161]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hs-source-dirs "package.cabal hs-source-dirs field"
[162]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hs-source-dirs "Permalink to this definition"
[163]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-extensions "Permalink to this definition"
[164]: https://cabal.readthedocs.io/en/release/cabal-latest/doc/API/Cabal/Language-Haskell-Extension.html#t:Extension
[165]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-extensions "Permalink to this definition"
[166]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-extensions "package.cabal default-extensions field(since version: 1.12)"
[167]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-extensions "package.cabal other-extensions field(since version: 1.12)"
[168]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-extensions "package.cabal other-extensions field(since version: 1.12)"
[169]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-language "Permalink to this definition"
[170]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-languages "Permalink to this definition"
[171]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extensions "Permalink to this definition"
[172]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-extensions "package.cabal default-extensions field(since version: 1.12)"
[173]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "Permalink to this definition"
[174]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-test-suite-test-suite "package.cabal test-suite section "
[175]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[176]: http://hackage.haskell.org/package/markdown-unlit
[177]: https://cabal.readthedocs.io/en/latest/cabal-package.html#old-style-build-tool-depends
[178]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[179]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[180]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "Permalink to this definition"
[181]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[182]: https://cabal.readthedocs.io/en/latest/cabal-package.html#buildtoolsbc
[183]: https://cabal.readthedocs.io/en/latest/cabal-package.html#buildtoolsmap
[184]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[185]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[186]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[187]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[188]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[189]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[190]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[191]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[192]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[193]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[194]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[195]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-buildable "Permalink to this definition"
[196]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id7
[197]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-options "Permalink to this definition"
[198]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-default-extensions "package.cabal default-extensions field(since version: 1.12)"
[199]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-prof-options "Permalink to this definition"
[200]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-prof-options "package.cabal ghc-prof-options field"
[201]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-shared-options "Permalink to this definition"
[202]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-options "package.cabal ghc-options field"
[203]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghcjs-options "Permalink to this definition"
[204]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-options "package.cabal ghc-options field"
[205]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghcjs-prof-options "Permalink to this definition"
[206]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-prof-options "package.cabal ghc-prof-options field"
[207]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghcjs-shared-options "Permalink to this definition"
[208]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ghc-shared-options "package.cabal ghc-shared-options field"
[209]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-includes "Permalink to this definition"
[210]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-include-dirs "package.cabal include-dirs field"
[211]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[212]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "Permalink to this definition"
[213]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[214]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-include-dirs "package.cabal include-dirs field"
[215]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[216]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[217]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-includes "package.cabal includes field"
[218]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-include-dirs "Permalink to this definition"
[219]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-includes "package.cabal includes field"
[220]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[221]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-c-sources "Permalink to this definition"
[222]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-sources "Permalink to this definition"
[223]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cc-options "package.cabal cc-options field"
[224]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-options "package.cabal cxx-options field(since version: 2.2)"
[225]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-sources "package.cabal cxx-sources field(since version: 2.2)"
[226]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-c-sources "package.cabal c-sources field"
[227]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-asm-sources "Permalink to this definition"
[228]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cmm-sources "Permalink to this definition"
[229]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-js-sources "Permalink to this definition"
[230]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cc-options "Permalink to this definition"
[231]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id7
[232]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cpp-options "Permalink to this definition"
[233]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-options "Permalink to this definition"
[234]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-sources "package.cabal cxx-sources field(since version: 2.2)"
[235]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-c-sources "package.cabal c-sources field"
[236]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-sources "package.cabal cxx-sources field(since version: 2.2)"
[237]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cc-options "package.cabal cc-options field"
[238]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cxx-options "package.cabal cxx-options field(since version: 2.2)"
[239]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cmm-options "Permalink to this definition"
[240]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cmm-sources "package.cabal cmm-sources field(since version: 3.0)"
[241]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-asm-options "Permalink to this definition"
[242]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-asm-sources "package.cabal asm-sources field(since version: 3.0)"
[243]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-ld-options "Permalink to this definition"
[244]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id7
[245]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-hsc2hs-options "Permalink to this definition"
[246]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field- "Permalink to this definition"
[247]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-pkgconfig-depends "Permalink to this definition"
[248]: http://www.freedesktop.org/wiki/Software/pkg-config/
[249]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-frameworks "Permalink to this definition"
[250]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "Permalink to this definition"
[251]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[252]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[253]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "package.cabal mixins field(since version: 2.0)"
[254]: https://github.com/haskell/cabal/issues/5150
[255]: https://github.com/haskell/cabal/issues/4864
[256]: https://github.com/haskell/cabal/issues/5293
[257]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures "package.cabal library section signatures: field(since version: 2.0)"
[258]: https://cabal.readthedocs.io/en/latest/cabal-package.html#backpack
[259]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[260]: https://cabal.readthedocs.io/en/latest/cabal-package.html#backpack
[261]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[262]: https://cabal.readthedocs.io/en/latest/cabal-package.html#configurations "Permalink to this headline"
[263]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id3 "Permalink to this headline"
[264]: https://cabal.readthedocs.io/en/latest/cabal-package.html#layout "Permalink to this headline"
[265]: https://cabal.readthedocs.io/en/latest/cabal-package.html#example-using-explicit-braces-rather-than-indentation-for-layout "Permalink to this headline"
[266]: https://cabal.readthedocs.io/en/latest/cabal-package.html#configuration-flags "Permalink to this headline"
[267]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-flag-flag "Permalink to this definition"
[268]: https://cabal.readthedocs.io/en/latest/cabal-package.html#conditional-blocks
[269]: https://tools.ietf.org/html/rfc5234
[270]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-flag-description "Permalink to this definition"
[271]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-flag-default "Permalink to this definition"
[272]: https://cabal.readthedocs.io/en/latest/installing-packages.html#controlling-flag-assignments
[273]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-flag-manual "Permalink to this definition"
[274]: https://cabal.readthedocs.io/en/latest/cabal-package.html#conditional-blocks "Permalink to this headline"
[275]: https://cabal.readthedocs.io/en/latest/cabal-package.html#conditions "Permalink to this headline"
[276]: https://cabal.readthedocs.io/en/latest/cabal-package.html#resolution-of-conditions-and-flags "Permalink to this headline"
[277]: https://cabal.readthedocs.io/en/latest/installing-packages.html#controlling-flag-assignments
[278]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[279]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tools "package.cabal build-tools field(removed in: 3.0; deprecated since: 2.0)"
[280]: https://cabal.readthedocs.io/en/latest/cabal-package.html#meaning-of-field-values-when-using-conditionals "Permalink to this headline"
[281]: https://cabal.readthedocs.io/en/latest/cabal-package.html#common-stanzas "Permalink to this headline"
[282]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-common-common "Permalink to this definition"
[283]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[284]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-import "Permalink to this definition"
[285]: https://cabal.readthedocs.io/en/latest/cabal-package.html#source-repositories "Permalink to this headline"
[286]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-source-repository-source-repository "Permalink to this definition"
[287]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-source-repository-type "Permalink to this definition"
[288]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-source-repository-location "Permalink to this definition"
[289]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-source-repository-module "Permalink to this definition"
[290]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-source-repository-branch "Permalink to this definition"
[291]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-source-repository-tag "Permalink to this definition"
[292]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-source-repository-subdir "Permalink to this definition"
[293]: https://cabal.readthedocs.io/en/latest/cabal-package.html#downloading-a-package-s-source "Permalink to this headline"
[294]: https://cabal.readthedocs.io/en/latest/cabal-package.html#custom-setup-scripts "Permalink to this headline"
[295]: https://www.well-typed.com/blog/2015/07/cabal-setup-deps/
[296]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-custom-setup-custom-setup "Permalink to this definition"
[297]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-custom-setup-custom-setup "package.cabal custom-setup section (since version: 1.24)"
[298]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-custom-setup-setup-depends "Permalink to this definition"
[299]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[300]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-cabal-version "package.cabal cabal-version field"
[301]: https://cabal.readthedocs.io/en/latest/cabal-package.html#backward-compatibility-and-custom-setup "Permalink to this headline"
[302]: https://cabal.readthedocs.io/en/latest/cabal-package.html#autogenerated-modules-and-includes "Permalink to this headline"
[303]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-modules "package.cabal other-modules field"
[304]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-exposed-modules "package.cabal library section exposed-modules: field"
[305]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-modules "package.cabal autogen-modules field(since version: 2.0)"
[306]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-modules "package.cabal other-modules field"
[307]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-exposed-modules "package.cabal library section exposed-modules: field"
[308]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-modules "Permalink to this definition"
[309]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-executable-main-is "package.cabal executable section main-is: field"
[310]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-modules "package.cabal autogen-modules field(since version: 2.0)"
[311]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-includes "Permalink to this definition"
[312]: https://cabal.readthedocs.io/en/latest/cabal-package.html#virtual-modules "Permalink to this headline"
[313]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-virtual-modules "Permalink to this definition"
[314]: https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files-from-package-code "Permalink to this headline"
[315]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-files "package.cabal data-files field"
[316]: https://cabal.readthedocs.io/en/latest/installing-packages.html#prefix-independence
[317]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-data-files "package.cabal data-files field"
[318]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-other-modules "package.cabal other-modules field"
[319]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-autogen-modules "package.cabal autogen-modules field(since version: 2.0)"
[320]: https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-the-package-version "Permalink to this headline"
[321]: http://hackage.haskell.org/package/base/docs/Data-Version.html
[322]: https://cabal.readthedocs.io/en/latest/cabal-package.html#system-dependent-parameters "Permalink to this headline"
[323]: https://cabal.readthedocs.io/en/latest/cabal-package.html#configurations
[324]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "package.cabal build-type field"
[325]: http://www.gnu.org/software/autoconf/
[326]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[327]: https://cabal.readthedocs.io/en/latest/cabal-package.html#build-information
[328]: https://cabal.readthedocs.io/en/latest/cabal-package.html#example-using-autoconf "Permalink to this headline"
[329]: http://www.gnu.org/software/autoconf/
[330]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-source-files "package.cabal extra-source-files field"
[331]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-tmp-files "package.cabal extra-tmp-files field"
[332]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-install-includes "package.cabal install-includes field"
[333]: https://cabal.readthedocs.io/en/latest/cabal-package.html#conditional-compilation "Permalink to this headline"
[334]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[335]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[336]: http://www.haskell.org/haddock/
[337]: https://cabal.readthedocs.io/en/latest/cabal-package.html#more-complex-packages "Permalink to this headline"
[338]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "package.cabal build-type field"
[339]: https://cabal.readthedocs.io/en/release/cabal-latest/doc/API/Cabal/Distribution-Simple.html
[340]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-section-custom-setup-custom-setup "package.cabal custom-setup section (since version: 1.24)"
[341]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-custom-setup-setup-depends "package.cabal custom-setup section setup-depends: field(since version: 1.24)"
[342]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "package.cabal build-type field"
[343]: https://cabal.readthedocs.io/en/release/cabal-latest/doc/API/Cabal/Distribution-Make.html
[344]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-type "package.cabal build-type field"
[345]: https://cabal.readthedocs.io/en/latest/installing-packages.html
[346]: https://cabal.readthedocs.io/en/latest/cabal-package.html#backpack "Permalink to this headline"
[347]: https://github.com/commercialhaskell/stack/issues/2540
[348]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures "package.cabal library section signatures: field(since version: 2.0)"
[349]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-signatures "package.cabal library section signatures: field(since version: 2.0)"
[350]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id15 "Permalink to this code"
[351]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id16 "Permalink to this code"
[352]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id17 "Permalink to this code"
[353]: https://cabal.readthedocs.io/en/latest/cabal-package.html#sublibs
[354]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "package.cabal mixins field(since version: 2.0)"
[355]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-mixins "package.cabal mixins field(since version: 2.0)"
[356]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id18 "Permalink to this code"
[357]: https://cabal.readthedocs.io/en/latest/cabal-package.html#sublibs
[358]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-library-reexported-modules "package.cabal library section reexported-modules: field(since version: 1.22)"
[359]: https://gitlab.haskell.org/ghc/ghc/-/wikis/backpack
[360]: https://cabal.readthedocs.io/en/latest/cabal-package.html#id2
[361]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"

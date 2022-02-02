---
created: 2021-08-20T03:05:30 (UTC +02:00)
tags: []
source: https://cabal.readthedocs.io/en/latest/cabal-project.html
author: 
---

# 8. cabal.project Reference — Cabal 3.6.0.0 User's Guide

> ## Excerpt
> cabal.project files support a variety of options which configure the
details of your build. The general syntax of a cabal.project file is
similar to that of a Cabal file: there are a number of fields, some of
which live inside stanzas (groups of fields that apply to only part of a
project or can be referenced as a unit):

---
`cabal.project` files support a variety of options which configure the details of your build. The general syntax of a `cabal.project` file is similar to that of a Cabal file: there are a number of fields, some of which live inside stanzas (groups of fields that apply to only part of a project or can be referenced as a unit):

packages: \*/\*.cabal
with-compiler: /opt/ghc/8.0.1/bin/ghc

package cryptohash
 optimization: False

In general, the accepted field names coincide with the accepted command line flags that `cabal install` and other commands take. For example, `cabal v2-configure --enable-profiling` will write out a project file with `profiling: True`.

The full configuration of a project is determined by combining the following sources (later entries override earlier ones, except for appendable options):

1.  `~/.cabal/config` (the user-wide global configuration)
    
2.  `cabal.project` (the project configuration)
    
3.  `cabal.project.freeze` (the output of `cabal v2-freeze`)
    
4.  `cabal.project.local` (the output of `cabal v2-configure`)
    

## 8.1. Specifying the local packages[¶][1]

The following top-level options specify what the local packages of a project are:

`packages``:` _package location list (space or comma separated)_[¶][2]

Default value

`./*.cabal`

Specifies the list of package locations which contain the local packages to be built by this project. Package locations can take the following forms:

1.  They can specify a Cabal file, or a directory containing a Cabal file, e.g., `packages: Cabal cabal-install/cabal-install.cabal`.
    
2.  They can specify glob-style wildcards, which must match one or more (a) directories containing a (single) Cabal file, (b) Cabal files (extension `.cabal`), or (c) tarballs which contain Cabal packages (extension `.tar.gz`). For example, to match all Cabal files in all subdirectories, as well as the Cabal projects in the parent directories `foo` and `bar`, use `packages: */*.cabal ../{foo,bar}/`
    
3.  They can specify an `http`, `https` or `file` URL, representing the path to a remote tarball to be downloaded and built.
    

There is no command line variant of this field; see [#3585][3]. Note that the default value is only included if there is no `cabal.project` file. The field is appendable which means there would be no way to drop the default value if it was included.

`optional-packages``:` _package location list (space or comma-separated)_[¶][4]

Default value

empty

Like [`packages`][5], specifies a list of package locations containing local packages to be built. Unlike [`packages`][6], if we glob for a package, it is permissible for the glob to match against zero packages. The intended use-case for [`optional-packages`][7] is to make it so that vendored packages can be automatically picked up if they are placed in a subdirectory, but not error if there aren’t any.

There is no command line variant of this field.

\[STRIKEOUT:Specifies a list of external packages from Hackage which should be considered local packages.\] (Not implemented)

There is no command line variant of this field.

All local packages are _vendored_, in the sense that if other packages (including external ones from Hackage) depend on a package with the name of a local package, the local package is preferentially used. For subdirectories to be considered local packages, the following setting can be used:

packages: ./\*.cabal
optional-packages: ./\*/\*.cabal

…then any package can be vendored simply by making a checkout in the top-level project directory, as might be seen in this hypothetical directory layout:

foo.cabal
foo-helper/     # local package
unix/           # vendored external package

All of these options support globs. `cabal v2-build` has its own glob format:

-   Anywhere in a path, as many times as you like, you can specify an asterisk `*` wildcard. E.g., `*/*.cabal` matches all `.cabal` files in all immediate subdirectories. Like in glob(7), asterisks do not match hidden files unless there is an explicit period, e.g., `.*/foo.cabal` will match `.private/foo.cabal` (but `*/foo.cabal` will not).
    
-   You can use braces to specify specific directories; e.g., `{vendor,pkgs}/*.cabal` matches all Cabal files in the `vendor` and `pkgs` subdirectories.
    

Formally, the format is described by the following BNF:

Todo

convert globbing grammar to proper [ABNF][8] syntax

FilePathGlob    ::\= FilePathRoot FilePathGlobRel
FilePathRoot    ::\= {- empty -}        # relative to cabal.project
                  | "/"                # Unix root
                  | \[a-zA-Z\] ":" \[/\\\\\] # Windows root
                  | "~"                # home directory
FilePathGlobRel ::\= Glob "/"  FilePathGlobRel # Unix directory
                  | Glob "\\\\" FilePathGlobRel # Windows directory
                  | Glob         # file
                  | {- empty -}  # trailing slash
Glob      ::\= GlobPiece \*
GlobPiece ::\= "\*"            # wildcard
            | \[^\*{},/\\\\\] \*   # literal string
            | "\\\\" \[\*{},\]    # escaped reserved character
            | "{" Glob "," ... "," Glob "}" # union (match any of these)

### 8.1.1. Specifying Packages from Remote Version Control Locations[¶][9]

Starting with Cabal 2.4, there is now a stanza `source-repository-package` for specifying packages from an external version control.

packages: .

source-repository-package
 type: git
 location: https://github.com/hvr/HsYAML.git
 tag: e70cf0c171c9a586b62b3f75d72f1591e4e6aaa1

source-repository-package
 type: git
 location: https://github.com/well-typed/cborg
 tag: 3d274c14ca3077c3a081ba7ad57c5182da65c8c1
 subdir: cborg

source-repository-package
 type: git
 location: https://github.com/haskell/network.git
 tag: e76fdc753e660dfa615af6c8b6a2ad9ddf6afe70
 post-checkout-command: autoreconf -i

cabal-install 3.4 sdists the `source-repository-package` repositories and uses resulting tarballs as project packages. This allows sharing of packages across different projects.

`type``:` _VCS kind_[¶][10]

`location``:` _VCS location (usually URL)_[¶][11]

`type``:` _VCS tag_[¶][12]

`subdir``:` _subdirectory list_[¶][13]

Use one or more subdirectories of the repository.

`post-checkout-command``:` _command_[¶][14]

Run command in the checked out repository, prior sdisting.

## 8.2. Global configuration options[¶][15]

The following top-level configuration options are not specific to any package, and thus apply globally:

`verbose``:` _nat_[¶][16]

`--verbose``=n``,` `-vn` [¶][17]

Default value

1

Control the verbosity of `cabal` commands, valid values are from 0 to 3.

The command line variant of this field is `--verbose=2`; a short form `-v2` is also supported.

`jobs``:` _nat or $ncpus_[¶][18]

`--jobs``=n``,` `-jn` `,` `--jobs``=$ncpus`[¶][19]

Default value

1

Run _nat_ jobs simultaneously when building. If `$ncpus` is specified, run the number of jobs equal to the number of CPUs. Package building is often quite parallel, so turning on parallelism can speed up build times quite a bit!

The command line variant of this field is `--jobs=2`; a short form `-j2` is also supported; a bare `--jobs` or `-j` is equivalent to `--jobs=$ncpus`.

`keep-going``:` _boolean_[¶][20]

`--keep-going` [¶][21]

Default value

False

If true, after a build failure, continue to build other unaffected packages.

The command line variant of this field is `--keep-going`.

`--builddir``=DIR`[¶][22]

Specifies the name of the directory where build products for build will be stored; defaults to `dist-newstyle`. If a relative name is specified, this directory is resolved relative to the root of the project (i.e., where the `cabal.project` file lives.)

This option cannot be specified via a `cabal.project` file.

`--project-file``=FILE`[¶][23]

Specifies the name of the project file used to specify the rest of the top-level configuration; defaults to `cabal.project`. This name not only specifies the name of the main project file, but also the auxiliary project files `cabal.project.freeze` and `cabal.project.local`; for example, if you specify `--project-file=my.project`, then the other files that will be probed are `my.project.freeze` and `my.project.local`.

If the specified project file is a relative path, we will look for the file relative to the current working directory, and then for the parent directory, until the project file is found or we have hit the top of the user’s home directory.

This option cannot be specified via a `cabal.project` file.

`--store-dir``=DIR`[¶][24]

Specifies the name of the directory of the global package store.

## 8.3. Phase control[¶][25]

The following settings apply to commands that result in build actions (`build`, `run`, `repl`, `test`…), and control which phases of the build are executed.

`--dry-run`[¶][26]

Do not download, build, or install anything, only print what would happen.

`--only-configure`[¶][27]

Instead of performing a full build just run the configure step. Only accepted by the `build` command.

`--only-download`[¶][28]

Do not build anything, only fetch the packages.

`--only-dependencies`[¶][29]

Install only the dependencies necessary to build the given packages. Not accepted by the `repl` command.

## 8.4. Solver configuration options[¶][30]

The following settings control the behavior of the dependency solver:

`constraints``:` _constraints list (comma separated)_[¶][31]

`--constraint``="pkg > 2.0"`[¶][32]

Add extra constraints to the version bounds, flag settings, and other properties a solver can pick for a package. For example:

A package can be specified multiple times in `constraints`, in which case the specified constraints are intersected. This is useful, since the syntax does not allow you to specify multiple constraints at once. For example, to specify both version bounds and flag assignments, you would write:

constraints: bar \== 2.1,
             bar +foo -baz

Valid constraints take the same form as for the [constraint command line option][33].

`preferences``:` _preference (comma separated)_[¶][34]

`--preference``="pkg > 2.0"`[¶][35]

Like [`constraints`][36], but the solver will attempt to satisfy these preferences on a best-effort basis. The resulting install is locally optimal with respect to preferences; specifically, no single package could be replaced with a more preferred version that still satisfies the hard constraints.

Operationally, preferences can cause the solver to attempt certain version choices of a package before others, which can improve dependency solver runtime.

One way to use [`preferences`][37] is to take a known working set of constraints (e.g., via `cabal v2-freeze`) and record them as preferences. In this case, the solver will first attempt to use this configuration, and if this violates hard constraints, it will try to find the minimal number of upgrades to satisfy the hard constraints again.

The command line variant of this field is `--preference="pkg >= 2.0"`; to specify multiple preferences, pass the flag multiple times.

`allow-newer``:` _none, all or list of scoped package names (space or comma separated)_[¶][38]

`--allow-newer` `,` `--allow-newer``=[none``,` `all` `,` `[scope:][^]pkg]` [¶][39]

Default value

`none`

Allow the solver to pick more recent version of some packages than would normally be permitted by the [`build-depends`][40] bounds of packages in the install plan. This option may be useful if the dependency solver cannot otherwise find a valid install plan.

For example, to relax `pkg`s [`build-depends`][41] upper bound on `dep-pkg`, write a scoped package name of the form:

If the scope shall be limited to specific releases of `pkg`, the extended form as in

allow-newer: pkg-1.2.3:dep-pkg, pkg-1.1.2:dep-pkg

can be used to limit the relaxation of dependencies on `dep-pkg` by the `pkg-1.2.3` and `pkg-1.1.2` releases only.

The scoped syntax is recommended, as it is often only a single package whose upper bound is misbehaving. In this case, the upper bounds of other packages should still be respected; indeed, relaxing the bound can break some packages which test the selected version of packages.

The syntax also allows to prefix the dependee package with a modifier symbol to modify the scope/semantic of the relaxation transformation in a additional ways. Currently only one modifier symbol is defined, i.e. `^` (i.e. caret) which causes the relaxation to be applied only to `^>=` operators and leave all other version operators untouched.

However, in some situations (e.g., when attempting to build packages on a new version of GHC), it is useful to disregard _all_ upper-bounds, with respect to a package or all packages. This can be done by specifying just a package name, or using the keyword `all` to specify all packages:

\-- Disregard upper bounds involving the dependencies on
\-- packages bar, baz. For quux only, relax
\-- 'quux ^>= ...'-style constraints only.
allow-newer: bar, baz, ^quux

\-- Disregard all upper bounds when dependency solving
allow-newer: all

\-- Disregard all \`^>=\`-style upper bounds when dependency solving
allow-newer: ^all

For consistency, there is also the explicit wildcard scope syntax `*` (or its alphabetic synonym `all`). Consequently, the examples above are equivalent to the explicitly scoped variants:

allow-newer: all:bar, \*:baz, \*:^quux

allow-newer: \*:\*
allow-newer: all:all

allow-newer: \*:^\*
allow-newer: all:^all

In order to ignore all bounds specified by a package `pkg-1.2.3` you can combine scoping with a right-hand-side wildcard like so

\-- Disregard any upper bounds specified by pkg-1.2.3
allow-newer: pkg-1.2.3:\*

\-- Disregard only \`^>=\`-style upper bounds in pkg-1.2.3
allow-newer: pkg-1.2.3:^\*

[`allow-newer`][42] is often used in conjunction with a constraint (in the [`constraints`][43] field) forcing the usage of a specific, newer version of a package.

The command line variant of this field is e.g. `--allow-newer=bar`. A bare `--allow-newer` is equivalent to `--allow-newer=all`.

`allow-older``:` _none, all, list of scoped package names (space or comma separated)_[¶][44]

`--allow-older` `,` `--allow-older``=[none``,` `all` `,` `[scope:][^]pkg]` [¶][45]

Since

Cabal 2.0

Default value

`none`

Like [`allow-newer`][46], but applied to lower bounds rather than upper bounds.

The command line variant of this field is `--allow-older=all`. A bare `--allow-older` is equivalent to `--allow-older=all`.

`index-state``:` _HEAD, unix-timestamp, ISO8601 UTC timestamp._[¶][47]

Since

Cabal 2.0

Default value

`HEAD`

This allows to change the source package index state the solver uses to compute install-plans. This is particularly useful in combination with freeze-files in order to also freeze the state the package index was in at the time the install-plan was frozen.

\-- UNIX timestamp format example
index-state: @1474739268

\-- ISO8601 UTC timestamp format example
\-- This format is used by 'cabal v2-configure'
\-- for storing \`--index-state\` values.
index-state: 2016-09-24T17:47:48Z

\-- Specify different index-states per package repository
\-- Supported since 3.4
index-state:
  , hackage.haskell.org 2020-05-06T22:33:27Z
  , head.hackage 2020-04-29T04:11:05Z

`active-repositories``:` _reponame1, reponame2_[¶][48]

Synopsis

Specify active package repositories

Since

3.4

Default value

`:rest`

Specifies which of the package repositories defined in the configuration should be active. It’s also useful for specifying the order and the way active repositories are merged.

When searching for a certain version of a certain package name, the list of active repositories is searched last-to-first.

For example, suppose hackage.haskell.org has versions 1.0 and 2.0 of package X, and my-repository has version 2.0 of a similarly named package. Then, with the following configuration:

\-- Force my-repository to be the first repository considered
active-repositories:
  , hackage.haskell.org
  , my-repository

version 2.0 of X will come from my-repository, and version 1.0 will come from hackage.haskell.org.

If we want to make a repository the sole provider of certain packages, we can put it last in the active repositories list, and add the :override modifier.

For example, if we modify the previous example like this:

active-repositories:
  , hackage.haskell.org
  , my-repository:override

then version 1.0 of package X won’t be found in any case, because X is present in my-repository only in version 2.0, and the :override forbids searching for other versions of X further up the list.

:override has no effect for package names that aren’t present in the overriding repository.

The special repository reference :rest stands for “all the other repositories” and can be useful to avoid lenghty lists of repository names:

\-- Force my-repository to be the first repository considered
active-repositories: :rest, my-repository

The special repository reference “none” disables all repositories, effectively putting cabal in “offline” mode:

active-repositories: none

`reject-unconstrained-dependencies``:` _all, none_[¶][49]

`--reject-unconstrained-dependencies``=[all|none]`[¶][50]

Default value

none

Since

2.6

By default, the dependency solver can include any package that it’s aware of in a build plan. If you wish to restrict the build plan to a closed set of packages (e.g., from a freeze file), use this flag.

When set to all, all non-local packages that aren’t goals must be explicitly constrained. When set to none, the solver will consider all packages.

## 8.5. Package configuration options[¶][51]

Package options affect the building of specific packages. There are three ways a package option can be specified:

-   They can be specified at the top-level, in which case they apply only to **local package**, or
    
-   They can be specified inside a `package` stanza, in which case they apply to the build of the package, whether or not it is local or external.
    
-   They can be specified inside an `package *` stanza, in which case they apply to all packages, local ones from the project and also external dependencies.
    

For example, the following options specify that [`optimization`][52] should be turned off for all local packages, and that `bytestring` (possibly an external dependency) should be built with `-fno-state-hack`:

optimization: False

package bytestring
 ghc-options: -fno-state-hack

`ghc-options` is not specifically described in this documentation, but is one of many fields for configuring programs. They take the form `progname-options` and `progname-location`, and can only be set inside package stanzas. (TODO: They are not supported at top-level, see [#3579][53].)

At the moment, there is no way to specify an option to apply to all external packages or all inplace packages. Additionally, it is only possible to specify these options on the command line for all local packages (there is no per-package command line interface.)

Some flags were added by more recent versions of the Cabal library. This means that they are NOT supported by packages which use Custom setup scripts that require a version of the Cabal library older than when the feature was added.

`flags``:` _list of +flagname or -flagname (space separated)_[¶][54]

`--flags``="+foo -bar"``,` `-ffoo` `,` `-f-bar` [¶][55]

Force all flags specified as `+flagname` to be true, and all flags specified as `-flagname` to be false. For example, to enable the flag `foo` and disable `bar`, set:

Exactly one of + or - is required before each flag.

Flags are _per-package_, so it doesn’t make much sense to specify flags at the top-level, unless you happen to know that _all_ of your local packages support the same named flags. If a flag is not supported by a package, it is ignored.

See also the solver configuration field [`constraints`][56].

The command line variant of this flag is `--flags`. There is also a shortened form `-ffoo -f-bar`.

A common mistake is to say `cabal v2-build -fhans`, where `hans` is a flag for a transitive dependency that is not in the local package; in this case, the flag will be silently ignored. If `haskell-tor` is the package you want this flag to apply to, try `--constraint="haskell-tor +hans"` instead.

`with-compiler``:` _executable_[¶][57]

`--with-compiler``=executable`[¶][58]

Specify the path to a particular compiler to be used. If not an absolute path, it will be resolved according to the `PATH` environment. The type of the compiler (GHC, GHCJS, etc) must be consistent with the setting of the [`compiler`][59] field.

The most common use of this option is to specify a different version of your compiler to be used; e.g., if you have `ghc-7.8` in your path, you can specify `with-compiler: ghc-7.8` to use it.

This flag also sets the default value of [`with-hc-pkg`][60], using the heuristic that it is named `ghc-pkg-7.8` (if your executable name is suffixed with a version number), or is the executable named `ghc-pkg` in the same directory as the `ghc` directory. If this heuristic does not work, set [`with-hc-pkg`][61] explicitly.

For inplace packages, `cabal v2-build` maintains a separate build directory for each version of GHC, so you can maintain multiple build trees for different versions of GHC without clobbering each other.

It’s not possible to set [`with-compiler`][62] on a per-package basis.

The command line variant of this flag is `--with-compiler=ghc-7.8`; there is also a short version `-w ghc-7.8`.

`with-hc-pkg``:` _executable_[¶][63]

`--with-hc-pkg``=executable`[¶][64]

Specify the path to the package tool, e.g., `ghc-pkg`. This package tool must be compatible with the compiler specified by [`with-compiler`][65] (generally speaking, it should be precisely the tool that was distributed with the compiler). If this option is omitted, the default value is determined from [`with-compiler`][66].

The command line variant of this flag is `--with-hc-pkg=ghc-pkg-7.8`.

`optimization``:` _nat_[¶][67]

`--enable-optimization` [¶][68]

`--disable-optimization` [¶][69]

Default value

`1`

Build with optimization. This is appropriate for production use, taking more time to build faster libraries and programs.

The optional _nat_ value is the optimisation level. Some compilers support multiple optimisation levels. The range is 0 to 2. Level 0 disables optimization, level 1 is the default. Level 2 is higher optimisation if the compiler supports it. Level 2 is likely to lead to longer compile times and bigger generated code. If you are not planning to run code, turning off optimization will lead to better build times and less code to be rebuilt when a module changes.

When optimizations are enabled, Cabal passes `-O2` to the C compiler.

We also accept `True` (equivalent to 1) and `False` (equivalent to 0).

Note that as of GHC 8.0, GHC does not recompile when optimization levels change (see [GHC #10923][70]), so if you change the optimization level for a local package you may need to blow away your old build products in order to rebuild with the new optimization level.

The command line variant of this flag is `-O2` (with `-O1` equivalent to `-O`). There are also long-form variants `--enable-optimization` and `--disable-optimization`.

`configure-options``:` _args (space separated)_[¶][71]

`--configure-option``=arg`[¶][72]

A list of extra arguments to pass to the external `./configure` script, if one is used. This is only useful for packages which have the `Configure` build type. See also the section on [system-dependent parameters][73].

The command line variant of this flag is `--configure-option=arg`, which can be specified multiple times to pass multiple options.

`compiler``:` _ghc, ghcjs, jhc, lhc, uhc or haskell-suite_[¶][74]

`--compiler``=compiler`[¶][75]

Default value

`ghc`

Specify the compiler toolchain to be used. This is independent of `with-compiler`, because the choice of toolchain affects Cabal’s build logic.

The command line variant of this flag is `--compiler=ghc`.

It’s not possible to set [`compiler`][76] on a per-package basis.

`tests``:` _boolean_[¶][77]

`--enable-tests` [¶][78]

`--disable-tests` [¶][79]

Default value

`False`

Force test suites to be enabled. For most users this should not be needed, as we always attempt to solve for test suite dependencies, even when this value is `False`; furthermore, test suites are automatically enabled if they are requested as a built target.

The command line variant of this flag is `--enable-tests` and `--disable-tests`.

`benchmarks``:` _boolean_[¶][80]

`--enable-benchmarks` [¶][81]

`--disable-benchmarks` [¶][82]

Default value

`False`

Force benchmarks to be enabled. For most users this should not be needed, as we always attempt to solve for benchmark dependencies, even when this value is `False`; furthermore, benchmarks are automatically enabled if they are requested as a built target.

The command line variant of this flag is `--enable-benchmarks` and `--disable-benchmarks`.

Since

Cabal 1.18

A list of directories to search for extra required programs. Most users should not need this, as programs like `happy` and `alex` will automatically be installed and added to the path. This can be useful if a `Custom` setup script relies on an exotic extra program.

The command line variant of this flag is `--extra-prog-path=PATH`, which can be specified multiple times.

`run-tests``:` _boolean_[¶][83]

`--run-tests` [¶][84]

Default value

`False`

Run the package test suite upon installation. This is useful for saying “When this package is installed, check that the test suite passes, terminating the rest of the build if it is broken.”

Warning

One deficiency: the [`run-tests`][85] setting of a package is NOT recorded as part of the hash, so if you install something without [`run-tests`][86] and then turn on `run-tests`, we won’t subsequently test the package. If this is causing you problems, give us a shout.

The command line variant of this flag is `--run-tests`.

### 8.5.1. Object code options[¶][87]

`debug-info``:` _integer_[¶][88]

`--enable-debug-info``=⟨n⟩`[¶][89]

`--disable-debug-info` [¶][90]

Since

Cabal 1.22

Default value

False

If the compiler (e.g., GHC 7.10 and later) supports outputing OS native debug info (e.g., DWARF), setting `debug-info: True` will instruct it to do so. See the GHC wiki page on [DWARF][91] for more information about this feature.

(This field also accepts numeric syntax, but until GHC 8.2 this didn’t do anything.)

The command line variant of this flag is `--enable-debug-info` and `--disable-debug-info`.

`split-sections``:` _boolean_[¶][92]

`--enable-split-sections` [¶][93]

`--disable-split-sections` [¶][94]

Since

Cabal 2.2

Default value

False

Use the GHC `-split-sections` feature when building the library. This reduces the final size of the executables that use the library by allowing them to link with only the bits that they use rather than the entire library. The downside is that building the library takes longer and uses a bit more memory.

This feature is supported by GHC 8.0 and later.

The command line variant of this flag is `--enable-split-sections` and `--disable-split-sections`.

`split-objs``:` _boolean_[¶][95]

`--enable-split-objs` [¶][96]

`--disable-split-objs` [¶][97]

Default value

False

Use the GHC `-split-objs` feature when building the library. This reduces the final size of the executables that use the library by allowing them to link with only the bits that they use rather than the entire library. The downside is that building the library takes longer and uses considerably more memory.

It is generally recommend that you use `split-sections` instead of `split-objs` where possible.

The command line variant of this flag is `--enable-split-objs` and `--disable-split-objs`.

`executable-stripping``:` _boolean_[¶][98]

`--enable-executable-stripping` [¶][99]

`--disable-executable-stripping` [¶][100]

Default value

True

When installing binary executable programs, run the `strip` program on the binary. This can considerably reduce the size of the executable binary file. It does this by removing debugging information and symbols.

Not all Haskell implementations generate native binaries. For such implementations this option has no effect.

If `debug-info` is set explicitly then `executable-stripping` is set to `False` as otherwise all the debug symbols will be stripped.

The command line variant of this flag is `--enable-executable-stripping` and `--disable-executable-stripping`.

`library-stripping``:` _boolean_[¶][101]

`--enable-library-stripping` [¶][102]

`--disable-library-stripping` [¶][103]

Since

Cabal 1.20

When installing binary libraries, run the `strip` program on the binary, saving space on the file system. See also `executable-stripping`.

If `debug-info` is set explicitly then `library-stripping` is set to `False` as otherwise all the debug symbols will be stripped.

The command line variant of this flag is `--enable-library-stripping` and `--disable-library-stripping`.

### 8.5.2. Executable options[¶][104]

`program-prefix``:` _prefix_[¶][105]

`--program-prefix``=prefix`[¶][106]

\[STRIKEOUT:Prepend _prefix_ to installed program names.\] (Currently implemented in a silly and not useful way. If you need this to work give us a shout.)

_prefix_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

The command line variant of this flag is `--program-prefix=foo-`.

`program-suffix``:` _suffix_[¶][107]

`--program-suffix``=suffix`[¶][108]

\[STRIKEOUT:Append _suffix_ to installed program names.\] (Currently implemented in a silly and not useful way. If you need this to work give us a shout.)

The most obvious use for this is to append the program’s version number to make it possible to install several versions of a program at once: `program-suffix: $version`.

_suffix_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

The command line variant of this flag is `--program-suffix='$version'`.

### 8.5.3. Dynamic linking options[¶][109]

`shared``:` _boolean_[¶][110]

`--enable-shared` [¶][111]

`--disable-shared` [¶][112]

Default value

False

Build shared library. This implies a separate compiler run to generate position independent code as required on most platforms.

The command line variant of this flag is `--enable-shared` and `--disable-shared`.

`executable-dynamic``:` _boolean_[¶][113]

`--enable-executable-dynamic` [¶][114]

`--disable-executable-dynamic` [¶][115]

Default value

False

Link executables dynamically. The executable’s library dependencies should be built as shared objects. This implies `shared: True` unless `shared: False` is explicitly specified.

The command line variant of this flag is `--enable-executable-dynamic` and `--disable-executable-dynamic`.

`library-for-ghci``:` _boolean_[¶][116]

`--enable-library-for-ghci` [¶][117]

`--disable-library-for-ghci` [¶][118]

Default value

True

Build libraries suitable for use with GHCi. This involves an extra linking step after the build.

Not all platforms support GHCi and indeed on some platforms, trying to build GHCi libs fails. In such cases, consider setting `library-for-ghci: False`.

The command line variant of this flag is `--enable-library-for-ghci` and `--disable-library-for-ghci`.

`relocatable``:` [¶][119]

`--relocatable` [¶][120]

Since

Cabal 1.22

Default value

False

\[STRIKEOUT:Build a package which is relocatable.\] (TODO: It is not clear what this actually does, or if it works at all.)

The command line variant of this flag is `--relocatable`.

### 8.5.4. Static linking options[¶][121]

`static``:` _boolean_[¶][122]

`--enable-static` [¶][123]

`--disable-static` [¶][124]

Default value

False

Roll this and all dependent libraries into a combined `.a` archive. This uses GHCs `-staticlib` flag, which is available for iOS and with GHC 8.4 and later for other platforms as well.

`executable-static``:` _boolean_[¶][125]

`--enable-executable-static` [¶][126]

`--disable-executable-static` [¶][127]

Default value

False

Build fully static executables. This links all dependent libraries into executables statically, including libc. This passes `-static` and `-optl=-static` to GHC.

### 8.5.5. Foreign function interface options[¶][128]

An extra directory to search for C header files. You can use this flag multiple times to get a list of directories.

You might need to use this flag if you have standard system header files in a non-standard location that is not mentioned in the package’s `.cabal` file. Using this option has the same affect as appending the directory _dir_ to the [`include-dirs`][129] field in each library and executable in the package’s `.cabal` file. The advantage of course is that you do not have to modify the package at all. These extra directories will be used while building the package and for libraries it is also saved in the package registration information and used when compiling modules that use the library.

The command line variant of this flag is `--extra-include-dirs=DIR`, which can be specified multiple times.

An extra directory to search for system libraries files.

The command line variant of this flag is `--extra-lib-dirs=DIR`, which can be specified multiple times.

An extra directory to search for frameworks (OS X only).

You might need to use this flag if you have standard system libraries in a non-standard location that is not mentioned in the package’s `.cabal` file. Using this option has the same affect as appending the directory _dir_ to the [`extra-lib-dirs`][130] field in each library and executable in the package’s `.cabal` file. The advantage of course is that you do not have to modify the package at all. These extra directories will be used while building the package and for libraries it is also saved in the package registration information and used when compiling modules that use the library.

The command line variant of this flag is `--extra-framework-dirs=DIR`, which can be specified multiple times.

### 8.5.6. Profiling options[¶][131]

`profiling``:` _boolean_[¶][132]

`--enable-profiling` [¶][133]

`--disable-profiling` [¶][134]

Since

Cabal 1.22

Default value

False

Build libraries and executables with profiling enabled (for compilers that support profiling as a separate mode). It is only necessary to specify [`profiling`][135] for the specific package you want to profile; `cabal v2-build` will ensure that all of its transitive dependencies are built with profiling enabled.

To enable profiling for only libraries or executables, see [`library-profiling`][136] and [`executable-profiling`][137].

For useful profiling, it can be important to control precisely what cost centers are allocated; see [`profiling-detail`][138].

The command line variant of this flag is `--enable-profiling` and `--disable-profiling`.

`profiling-detail``:` _level_[¶][139]

`--profiling-detail``=level`[¶][140]

Since

Cabal 1.24

Some compilers that support profiling, notably GHC, can allocate costs to different parts of the program and there are different levels of granularity or detail with which this can be done. In particular for GHC this concept is called “cost centers”, and GHC can automatically add cost centers, and can do so in different ways.

This flag covers both libraries and executables, but can be overridden by the `library-profiling-detail` field.

Currently this setting is ignored for compilers other than GHC. The levels that cabal currently supports are:

default

For GHC this uses `exported-functions` for libraries and `toplevel-functions` for executables.

none

No costs will be assigned to any code within this component.

exported-functions

Costs will be assigned at the granularity of all top level functions exported from each module. In GHC, this is for non-inline functions. Corresponds to `-fprof-auto-exported`.

toplevel-functions

Costs will be assigned at the granularity of all top level functions in each module, whether they are exported from the module or not. In GHC specifically, this is for non-inline functions. Corresponds to `-fprof-auto-top`.

all-functions

Costs will be assigned at the granularity of all functions in each module, whether top level or local. In GHC specifically, this is for non-inline toplevel or where-bound functions or values. Corresponds to `-fprof-auto`.

The command line variant of this flag is `--profiling-detail=none`.

`library-profiling-detail``:` _level_[¶][141]

`--library-profiling-detail``=level`[¶][142]

Since

Cabal 1.24

Like [`profiling-detail`][143], but applied only to libraries

The command line variant of this flag is `--library-profiling-detail=none`.

`library-vanilla``:` _boolean_[¶][144]

`--enable-library-vanilla` [¶][145]

`--disable-library-vanilla` [¶][146]

Default value

True

Build ordinary libraries (as opposed to profiling libraries). Mostly, you can set this to False to avoid building ordinary libraries when you are profiling.

The command line variant of this flag is `--enable-library-vanilla` and `--disable-library-vanilla`.

`library-profiling``:` _boolean_[¶][147]

`--enable-library-profiling` [¶][148]

`--disable-library-profiling` [¶][149]

Since

Cabal 1.22

Default value

False

Build libraries with profiling enabled. You probably want to use [`profiling`][150] instead.

The command line variant of this flag is `--enable-library-profiling` and `--disable-library-profiling`.

`executable-profiling``:` _boolean_[¶][151]

`--enable-executable-profiling` [¶][152]

`--disable-executable-profiling` [¶][153]

Since

Cabal 1.22

Default value

False

Build executables with profiling enabled. You probably want to use [`profiling`][154] instead.

The command line variant of this flag is `--enable-executable-profiling` and `--disable-executable-profiling`.

### 8.5.7. Coverage options[¶][155]

`coverage``:` _boolean_[¶][156]

`--enable-coverage` [¶][157]

`--disable-coverage` [¶][158]

Since

Cabal 1.22

Default value

False

Build libraries and executables (including test suites) with Haskell Program Coverage enabled. Running the test suites will automatically generate coverage reports with HPC.

The command line variant of this flag is `--enable-coverage` and `--disable-coverage`.

`library-coverage``:` _boolean_[¶][159]

`--enable-library-coverage` [¶][160]

`--disable-library-coverage` [¶][161]

Deprecated

Since

Cabal 1.22

Default value

False

Deprecated, use [`coverage`][162].

The command line variant of this flag is `--enable-library-coverage` and `--disable-library-coverage`.

### 8.5.8. Haddock options[¶][163]

`documentation``:` _boolean_[¶][164]

`--enable-documentation` [¶][165]

`--disable-documentation` [¶][166]

Default value

False

Enables building of Haddock documentation

The command line variant of this flag is `--enable-documentation` and `--disable-documentation`.

documentation: true does not imply [`haddock-benchmarks`][167], [`haddock-executables`][168], [`haddock-internal`][169] or [`haddock-tests`][170]. These need to be enabled separately if desired.

`doc-index-file``:` _templated path_[¶][171]

`--doc-index-file``=TEMPLATE`[¶][172]

A central index of Haddock API documentation (template cannot use `$pkgid`), which should be updated as documentation is built.

The command line variant of this flag is `--doc-index-file=TEMPLATE`

The following commands are equivalent to ones that would be passed when running `setup haddock`. (TODO: Where does the documentation get put.)

`haddock-hoogle``:` _boolean_[¶][173]

Default value

False

Generate a text file which can be converted by [Hoogle][174] into a database for searching. This is equivalent to running `haddock` with the `--hoogle` flag.

The command line variant of this flag is `--hoogle` (for the `haddock` command).

`haddock-html``:` _boolean_[¶][175]

Default value

True

Build HTML documentation.

The command line variant of this flag is `--html` (for the `haddock` command).

`haddock-html-location``:` _templated path_[¶][176]

`--html-location``=TEMPLATE`[¶][177]

Specify a template for the location of HTML documentation for prerequisite packages. The substitutions are applied to the template to obtain a location for each package, which will be used by hyperlinks in the generated documentation. For example, the following command generates links pointing at \[Hackage\] pages:

html-location: http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html

The command line variant of this flag is `--html-location` (for the `haddock` subcommand).

\--html-location='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

Here the argument is quoted to prevent substitution by the shell. If this option is omitted, the location for each package is obtained using the package tool (e.g. `ghc-pkg`).

`haddock-executables``:` _boolean_[¶][178]

Default value

False

Run haddock on all executable programs.

The command line variant of this flag is `--executables` (for the `haddock` subcommand).

`haddock-tests``:` _boolean_[¶][179]

Default value

False

Run haddock on all test suites.

The command line variant of this flag is `--tests` (for the `haddock` subcommand).

`haddock-benchmarks``:` _boolean_[¶][180]

Default value

False

Run haddock on all benchmarks.

The command line variant of this flag is `--benchmarks` (for the `haddock` subcommand).

`haddock-all``:` _boolean_[¶][181]

Default value

False

Run haddock on all components.

The command line variant of this flag is `--all` (for the `haddock` subcommand).

`haddock-internal``:` _boolean_[¶][182]

Default value

False

Build haddock documentation which includes unexposed modules and symbols.

The command line variant of this flag is `--internal` (for the `haddock` subcommand).

`haddock-css``:` _path_[¶][183]

The CSS file that should be used to style the generated documentation (overriding haddock’s default.)

The command line variant of this flag is `--css` (for the `haddock` subcommand).

`haddock-hyperlink-source``:` _boolean_[¶][184]

Default value

False

Generated hyperlinked source code using [HsColour][185], and have Haddock documentation link to it.

The command line variant of this flag is `--hyperlink-source` (for the `haddock` subcommand).

`haddock-hscolour-css``:` _path_[¶][186]

The CSS file that should be used to style the generated hyperlinked source code (from [HsColour][187]).

The command line variant of this flag is `--hscolour-css` (for the `haddock` subcommand).

`haddock-contents-location``:` _URL_[¶][188]

A baked-in URL to be used as the location for the contents page.

The command line variant of this flag is `--contents-location` (for the `haddock` subcommand).

`haddock-keep-temp-files``:` _boolean_[¶][189]

Keep temporary files.

The command line variant of this flag is `--keep-temp-files` (for the `haddock` subcommand).

## 8.6. Advanced global configuration options[¶][190]

`write-ghc-environment-files``:` _always, never, or ghc8.4.4+_[¶][191]

`--write-ghc-environment-files``=policy`[¶][192]

Default value

`never`

Whether a [GHC package environment file][193] should be created after a successful build.

Since Cabal 3.0, defaults to `never`. Before that, defaulted to creating them only when compiling with GHC 8.4.4 and older (GHC 8.4.4 [is the first version][194] that supports the `-package-env -` option that allows ignoring the package environment files).

`http-transport``:` _curl, wget, powershell, or plain-http_[¶][195]

`--http-transport``=transport`[¶][196]

Default value

`curl`

Set a transport to be used when making http(s) requests.

The command line variant of this field is `--http-transport=curl`.

`ignore-expiry``:` _boolean_[¶][197]

`--ignore-expiry` [¶][198]

Default value

False

If `True`, we will ignore expiry dates on metadata from Hackage.

In general, you should not set this to `True` as it will leave you vulnerable to stale cache attacks. However, it may be temporarily useful if the main Hackage server is down, and we need to rely on mirrors which have not been updated for longer than the expiry period on the timestamp.

The command line variant of this field is `--ignore-expiry`.

`remote-repo-cache``:` _directory_[¶][199]

`--remote-repo-cache``=DIR`[¶][200]

Default value

`~/.cabal/packages`

\[STRIKEOUT:The location where packages downloaded from remote repositories will be cached.\] Not implemented yet.

The command line variant of this flag is `--remote-repo-cache=DIR`.

`logs-dir``:` _directory_[¶][201]

`--logs-dir``=DIR`[¶][202]

Default value

`~/.cabal/logs`

\[STRIKEOUT:The location where build logs for packages are stored.\] Not implemented yet.

The command line variant of this flag is `--logs-dir=DIR`.

`build-summary``:` _template filepath_[¶][203]

`--build-summary``=TEMPLATE`[¶][204]

Default value

`~/.cabal/logs/build.log`

\[STRIKEOUT:The file to save build summaries. Valid variables which can be used in the path are `$pkgid`, `$compiler`, `$os` and `$arch`.\] Not implemented yet.

The command line variant of this flag is `--build-summary=TEMPLATE`.

`world-file``:` _path_[¶][205]

`--world-file``=FILE`[¶][206]

Deprecated

\[STRIKEOUT:The location of the world file.\] Deprecated.

The command line variant of this flag is `--world-file=FILE`.

Undocumented fields: `root-cmd`, `symlink-bindir`, `build-log`, `remote-build-reporting`, `report-planned-failure`, `one-shot`, `offline`.

### 8.6.1. Advanced solver options[¶][207]

Most users generally won’t need these.

`solver``:` _modular_[¶][208]

`--solver``=modular`[¶][209]

This field is reserved to allow the specification of alternative dependency solvers. At the moment, the only accepted option is `modular`.

The command line variant of this field is `--solver=modular`.

`max-backjumps``:` _nat_[¶][210]

`--max-backjumps``=N`[¶][211]

Default value

4000

Maximum number of backjumps (backtracking multiple steps) allowed while solving. Set -1 to allow unlimited backtracking, and 0 to disable backtracking completely.

The command line variant of this field is `--max-backjumps=4000`.

`reorder-goals``:` _boolean_[¶][212]

`--reorder-goals` [¶][213]

`--no-reorder-goals` [¶][214]

Default value

False

When enabled, the solver will reorder goals according to certain heuristics. Slows things down on average, but may make backtracking faster for some packages. It’s unlikely to help for small projects, but for big install plans it may help you find a plan when otherwise this is not possible. See [#1780][215] for more commentary.

The command line variant of this field is `--(no-)reorder-goals`.

`count-conflicts``:` _boolean_[¶][216]

`--count-conflicts` [¶][217]

`--no-count-conflicts` [¶][218]

Default value

True

Try to speed up solving by preferring goals that are involved in a lot of conflicts.

The command line variant of this field is `--(no-)count-conflicts`.

`fine-grained-conflicts``:` _boolean_[¶][219]

`--fine-grained-conflicts` [¶][220]

`--no-fine-grained-conflicts` [¶][221]

Default value

True

When enabled, the solver will skip a version of a package if it does not resolve any of the conflicts encountered in the last version of that package. For example, if `foo-1.2` depended on `bar`, and the solver couldn’t find consistent versions for `bar`’s dependencies, then the solver would skip `foo-1.1` if it also depended on `bar`.

The command line variant of this field is `--(no-)fine-grained-conflicts`.

`minimize-conflict-set``:` _boolean_[¶][222]

`--minimize-conflict-set` [¶][223]

`--no-minimize-conflict-set` [¶][224]

Default value

False

When there is no solution, try to improve the solver error message by finding a minimal conflict set. This option may increase run time significantly, so it is off by default.

The command line variant of this field is `--(no-)minimize-conflict-set`.

`strong-flags``:` _boolean_[¶][225]

`--strong-flags` [¶][226]

`--no-strong-flags` [¶][227]

Default value

False

Do not defer flag choices. (TODO: Better documentation.)

The command line variant of this field is `--(no-)strong-flags`.

`allow-boot-library-installs``:` _boolean_[¶][228]

`--allow-boot-library-installs` [¶][229]

`--no-allow-boot-library-installs` [¶][230]

Default value

False

By default, the dependency solver doesn’t allow `base`, `ghc-prim`, `integer-simple`, `integer-gmp`, and `template-haskell` to be installed or upgraded. This flag removes the restriction.

The command line variant of this field is `--(no-)allow-boot-library-installs`.

`cabal-lib-version``:` _version_[¶][231]

`--cabal-lib-version``=version`[¶][232]

This field selects the version of the Cabal library which should be used to build packages. This option is intended primarily for internal development use (e.g., forcing a package to build with a newer version of Cabal, to test a new version of Cabal.) (TODO: Specify its semantics more clearly.)

The command line variant of this field is `--cabal-lib-version=1.24.0.1`.

[1]: https://cabal.readthedocs.io/en/latest/cabal-project.html#specifying-the-local-packages "Permalink to this headline"
[2]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-packages "Permalink to this definition"
[3]: https://github.com/haskell/cabal/issues/3585
[4]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-optional-packages "Permalink to this definition"
[5]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-packages "cabal.project packages field "
[6]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-packages "cabal.project packages field "
[7]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-optional-packages "cabal.project optional-packages field "
[8]: https://tools.ietf.org/html/rfc5234
[9]: https://cabal.readthedocs.io/en/latest/cabal-project.html#specifying-packages-from-remote-version-control-locations "Permalink to this headline"
[10]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-type "Permalink to this definition"
[11]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-location "Permalink to this definition"
[12]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-type "Permalink to this definition"
[13]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-subdir "Permalink to this definition"
[14]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-post-checkout-command "Permalink to this definition"
[15]: https://cabal.readthedocs.io/en/latest/cabal-project.html#global-configuration-options "Permalink to this headline"
[16]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-verbose "Permalink to this definition"
[17]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---verbose "Permalink to this definition"
[18]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-jobs "Permalink to this definition"
[19]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---jobs "Permalink to this definition"
[20]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-keep-going "Permalink to this definition"
[21]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---keep-going "Permalink to this definition"
[22]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cmdoption-builddir "Permalink to this definition"
[23]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cmdoption-0 "Permalink to this definition"
[24]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cmdoption-store-dir "Permalink to this definition"
[25]: https://cabal.readthedocs.io/en/latest/cabal-project.html#phase-control "Permalink to this headline"
[26]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cmdoption-dry-run "Permalink to this definition"
[27]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cmdoption-only-configure "Permalink to this definition"
[28]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cmdoption-only-download "Permalink to this definition"
[29]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cmdoption-only-dependencies "Permalink to this definition"
[30]: https://cabal.readthedocs.io/en/latest/cabal-project.html#solver-configuration-options "Permalink to this headline"
[31]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-constraints "Permalink to this definition"
[32]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---constraint "Permalink to this definition"
[33]: https://cabal.readthedocs.io/en/latest/installing-packages.html#cmdoption-setup-configure-constraint
[34]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-preferences "Permalink to this definition"
[35]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---preference "Permalink to this definition"
[36]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-constraints "cabal.project constraints field "
[37]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-preferences "cabal.project preferences field "
[38]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-allow-newer "Permalink to this definition"
[39]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---allow-newer "Permalink to this definition"
[40]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[41]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[42]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-allow-newer "cabal.project allow-newer field "
[43]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-constraints "cabal.project constraints field "
[44]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-allow-older "Permalink to this definition"
[45]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---allow-older "Permalink to this definition"
[46]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-allow-newer "cabal.project allow-newer field "
[47]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-index-state "Permalink to this definition"
[48]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-active-repositories "Permalink to this definition"
[49]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-reject-unconstrained-dependencies "Permalink to this definition"
[50]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---reject-unconstrained-dependencies "Permalink to this definition"
[51]: https://cabal.readthedocs.io/en/latest/cabal-project.html#package-configuration-options "Permalink to this headline"
[52]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-optimization "cabal.project optimization field "
[53]: https://github.com/haskell/cabal/issues/3579
[54]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-flags "Permalink to this definition"
[55]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---flags "Permalink to this definition"
[56]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-constraints "cabal.project constraints field "
[57]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-with-compiler "Permalink to this definition"
[58]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---with-compiler "Permalink to this definition"
[59]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-compiler "cabal.project compiler field "
[60]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-with-hc-pkg "cabal.project with-hc-pkg field "
[61]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-with-hc-pkg "cabal.project with-hc-pkg field "
[62]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-with-compiler "cabal.project with-compiler field "
[63]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-with-hc-pkg "Permalink to this definition"
[64]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---with-hc-pkg "Permalink to this definition"
[65]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-with-compiler "cabal.project with-compiler field "
[66]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-with-compiler "cabal.project with-compiler field "
[67]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-optimization "Permalink to this definition"
[68]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-optimization "Permalink to this definition"
[69]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-optimization "Permalink to this definition"
[70]: https://gitlab.haskell.org/ghc/ghc/-/issues/10923
[71]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-configure-options "Permalink to this definition"
[72]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---configure-option "Permalink to this definition"
[73]: https://cabal.readthedocs.io/en/latest/developing-packages.html#system-dependent-parameters
[74]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-compiler "Permalink to this definition"
[75]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---compiler "Permalink to this definition"
[76]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-compiler "cabal.project compiler field "
[77]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-tests "Permalink to this definition"
[78]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-tests "Permalink to this definition"
[79]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-tests "Permalink to this definition"
[80]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-benchmarks "Permalink to this definition"
[81]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-benchmarks "Permalink to this definition"
[82]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-benchmarks "Permalink to this definition"
[83]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-run-tests "Permalink to this definition"
[84]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---run-tests "Permalink to this definition"
[85]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-run-tests "cabal.project run-tests field "
[86]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-run-tests "cabal.project run-tests field "
[87]: https://cabal.readthedocs.io/en/latest/cabal-project.html#object-code-options "Permalink to this headline"
[88]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-debug-info "Permalink to this definition"
[89]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-debug-info "Permalink to this definition"
[90]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-debug-info "Permalink to this definition"
[91]: https://gitlab.haskell.org/ghc/ghc/-/wikis/DWARF
[92]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-split-sections "Permalink to this definition"
[93]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-split-sections "Permalink to this definition"
[94]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-split-sections "Permalink to this definition"
[95]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-split-objs "Permalink to this definition"
[96]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-split-objs "Permalink to this definition"
[97]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-split-objs "Permalink to this definition"
[98]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-executable-stripping "Permalink to this definition"
[99]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-executable-stripping "Permalink to this definition"
[100]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-executable-stripping "Permalink to this definition"
[101]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-library-stripping "Permalink to this definition"
[102]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-library-stripping "Permalink to this definition"
[103]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-library-stripping "Permalink to this definition"
[104]: https://cabal.readthedocs.io/en/latest/cabal-project.html#executable-options "Permalink to this headline"
[105]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-program-prefix "Permalink to this definition"
[106]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---program-prefix "Permalink to this definition"
[107]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-program-suffix "Permalink to this definition"
[108]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---program-suffix "Permalink to this definition"
[109]: https://cabal.readthedocs.io/en/latest/cabal-project.html#dynamic-linking-options "Permalink to this headline"
[110]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-shared "Permalink to this definition"
[111]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-shared "Permalink to this definition"
[112]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-shared "Permalink to this definition"
[113]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-executable-dynamic "Permalink to this definition"
[114]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-executable-dynamic "Permalink to this definition"
[115]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-executable-dynamic "Permalink to this definition"
[116]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-library-for-ghci "Permalink to this definition"
[117]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-library-for-ghci "Permalink to this definition"
[118]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-library-for-ghci "Permalink to this definition"
[119]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-relocatable "Permalink to this definition"
[120]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---relocatable "Permalink to this definition"
[121]: https://cabal.readthedocs.io/en/latest/cabal-project.html#static-linking-options "Permalink to this headline"
[122]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-static "Permalink to this definition"
[123]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-static "Permalink to this definition"
[124]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-static "Permalink to this definition"
[125]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-executable-static "Permalink to this definition"
[126]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-executable-static "Permalink to this definition"
[127]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-executable-static "Permalink to this definition"
[128]: https://cabal.readthedocs.io/en/latest/cabal-project.html#foreign-function-interface-options "Permalink to this headline"
[129]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-include-dirs "package.cabal include-dirs field"
[130]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-extra-lib-dirs "cabal.project extra-lib-dirs field "
[131]: https://cabal.readthedocs.io/en/latest/cabal-project.html#profiling-options "Permalink to this headline"
[132]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-profiling "Permalink to this definition"
[133]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-profiling "Permalink to this definition"
[134]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-profiling "Permalink to this definition"
[135]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-profiling "cabal.project profiling field (since version: 1.22)"
[136]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-library-profiling "cabal.project library-profiling field (since version: 1.22)"
[137]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-executable-profiling "cabal.project executable-profiling field (since version: 1.22)"
[138]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-profiling-detail "cabal.project profiling-detail field (since version: 1.24)"
[139]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-profiling-detail "Permalink to this definition"
[140]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---profiling-detail "Permalink to this definition"
[141]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-library-profiling-detail "Permalink to this definition"
[142]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---library-profiling-detail "Permalink to this definition"
[143]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-profiling-detail "cabal.project profiling-detail field (since version: 1.24)"
[144]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-library-vanilla "Permalink to this definition"
[145]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-library-vanilla "Permalink to this definition"
[146]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-library-vanilla "Permalink to this definition"
[147]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-library-profiling "Permalink to this definition"
[148]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-library-profiling "Permalink to this definition"
[149]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-library-profiling "Permalink to this definition"
[150]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-profiling "cabal.project profiling field (since version: 1.22)"
[151]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-executable-profiling "Permalink to this definition"
[152]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-executable-profiling "Permalink to this definition"
[153]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-executable-profiling "Permalink to this definition"
[154]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-profiling "cabal.project profiling field (since version: 1.22)"
[155]: https://cabal.readthedocs.io/en/latest/cabal-project.html#coverage-options "Permalink to this headline"
[156]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-coverage "Permalink to this definition"
[157]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-coverage "Permalink to this definition"
[158]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-coverage "Permalink to this definition"
[159]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-library-coverage "Permalink to this definition"
[160]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-library-coverage "Permalink to this definition"
[161]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-library-coverage "Permalink to this definition"
[162]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-coverage "cabal.project coverage field (since version: 1.22)"
[163]: https://cabal.readthedocs.io/en/latest/cabal-project.html#haddock-options "Permalink to this headline"
[164]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-documentation "Permalink to this definition"
[165]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---enable-documentation "Permalink to this definition"
[166]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---disable-documentation "Permalink to this definition"
[167]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-benchmarks "cabal.project haddock-benchmarks field "
[168]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-executables "cabal.project haddock-executables field "
[169]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-internal "cabal.project haddock-internal field "
[170]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-tests "cabal.project haddock-tests field "
[171]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-doc-index-file "Permalink to this definition"
[172]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---doc-index-file "Permalink to this definition"
[173]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-hoogle "Permalink to this definition"
[174]: http://www.haskell.org/hoogle/
[175]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-html "Permalink to this definition"
[176]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-html-location "Permalink to this definition"
[177]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---html-location "Permalink to this definition"
[178]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-executables "Permalink to this definition"
[179]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-tests "Permalink to this definition"
[180]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-benchmarks "Permalink to this definition"
[181]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-all "Permalink to this definition"
[182]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-internal "Permalink to this definition"
[183]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-css "Permalink to this definition"
[184]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-hyperlink-source "Permalink to this definition"
[185]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[186]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-hscolour-css "Permalink to this definition"
[187]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[188]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-contents-location "Permalink to this definition"
[189]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-haddock-keep-temp-files "Permalink to this definition"
[190]: https://cabal.readthedocs.io/en/latest/cabal-project.html#advanced-global-configuration-options "Permalink to this headline"
[191]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-write-ghc-environment-files "Permalink to this definition"
[192]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---write-ghc-environment-files "Permalink to this definition"
[193]: https://downloads.haskell.org/~ghc/master/users-guide/packages.html#package-environments
[194]: https://gitlab.haskell.org/ghc/ghc/-/issues/13753
[195]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-http-transport "Permalink to this definition"
[196]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---http-transport "Permalink to this definition"
[197]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-ignore-expiry "Permalink to this definition"
[198]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---ignore-expiry "Permalink to this definition"
[199]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-remote-repo-cache "Permalink to this definition"
[200]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---remote-repo-cache "Permalink to this definition"
[201]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-logs-dir "Permalink to this definition"
[202]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---logs-dir "Permalink to this definition"
[203]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-build-summary "Permalink to this definition"
[204]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---build-summary "Permalink to this definition"
[205]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-world-file "Permalink to this definition"
[206]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---world-file "Permalink to this definition"
[207]: https://cabal.readthedocs.io/en/latest/cabal-project.html#advanced-solver-options "Permalink to this headline"
[208]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-solver "Permalink to this definition"
[209]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---solver "Permalink to this definition"
[210]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-max-backjumps "Permalink to this definition"
[211]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---max-backjumps "Permalink to this definition"
[212]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-reorder-goals "Permalink to this definition"
[213]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---reorder-goals "Permalink to this definition"
[214]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---no-reorder-goals "Permalink to this definition"
[215]: https://github.com/haskell/cabal/issues/1780
[216]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-count-conflicts "Permalink to this definition"
[217]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---count-conflicts "Permalink to this definition"
[218]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---no-count-conflicts "Permalink to this definition"
[219]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-fine-grained-conflicts "Permalink to this definition"
[220]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---fine-grained-conflicts "Permalink to this definition"
[221]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---no-fine-grained-conflicts "Permalink to this definition"
[222]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-minimize-conflict-set "Permalink to this definition"
[223]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---minimize-conflict-set "Permalink to this definition"
[224]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---no-minimize-conflict-set "Permalink to this definition"
[225]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-strong-flags "Permalink to this definition"
[226]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---strong-flags "Permalink to this definition"
[227]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---no-strong-flags "Permalink to this definition"
[228]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-allow-boot-library-installs "Permalink to this definition"
[229]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---allow-boot-library-installs "Permalink to this definition"
[230]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---no-allow-boot-library-installs "Permalink to this definition"
[231]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-field-cabal-lib-version "Permalink to this definition"
[232]: https://cabal.readthedocs.io/en/latest/cabal-project.html#cfg-flag---cabal-lib-version "Permalink to this definition"

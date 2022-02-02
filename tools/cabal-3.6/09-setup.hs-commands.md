---
created: 2021-08-20T03:05:36 (UTC +02:00)
tags: []
source: https://cabal.readthedocs.io/en/latest/setup-commands.html
author: 
---

# 9. Setup.hs Commands — Cabal 3.6.0.0 User's Guide

> ## Excerpt
> The low-level Cabal interface is implemented using Setup.hs scripts.
You should prefer using higher level interface provided by
nix-style builds.

---
The low-level Cabal interface is implemented using `Setup.hs` scripts. You should prefer using higher level interface provided by nix-style builds.

$ runhaskell Setup.hs \[command\] \[option...\]

For the summary of the command syntax, run:

$ runhaskell Setup.hs --help

## 9.1. Building and installing a system package[¶][1]

$ runhaskell Setup.hs configure --ghc
$ runhaskell Setup.hs build
$ runhaskell Setup.hs install

The first line readies the system to build the tool using GHC; for example, it checks that GHC exists on the system. The second line performs the actual building, while the last both copies the build results to some permanent place and registers the package with GHC.

Note

Global installing of packages is not recommended. The [Nix-style Local Builds][2] is the preferred way of building and installing packages.

## 9.2. Creating a binary package[¶][3]

When creating binary packages (e.g. for Red Hat or Debian) one needs to create a tarball that can be sent to another system for unpacking in the root directory:

$ runhaskell Setup.hs configure --prefix\=/usr
$ runhaskell Setup.hs build
$ runhaskell Setup.hs copy --destdir\=/tmp/mypkg
$ tar -czf mypkg.tar.gz /tmp/mypkg/

If the package contains a library, you need two additional steps:

$ runhaskell Setup.hs register --gen-script
$ runhaskell Setup.hs unregister --gen-script

This creates shell scripts `register.sh` and `unregister.sh`, which must also be sent to the target system. After unpacking there, the package must be registered by running the `register.sh` script. The `unregister.sh` script would be used in the uninstall procedure of the package. Similar steps may be used for creating binary packages for Windows.

The following options are understood by all commands:

`--help``,` `-h` `or -?`[¶][4]

List the available options for the command.

`--verbose``=n or -v n`[¶][5]

Set the verbosity level (0-3). The normal level is 1; a missing _n_ defaults to 2.

There is also an extended version of this command which can be used to fine-tune the verbosity of output. It takes the form `[silent|normal|verbose|debug]`_flags_, where _flags_ is a list of `+` flags which toggle various aspects of output. At the moment, only `+callsite` and `+callstack` are supported, which respectively toggle call site and call stack printing (these are only supported if Cabal is built with a sufficiently recent GHC.)

The various commands and the additional options they support are described below. In the simple build infrastructure, any other options will be reported as errors.

## 9.3. runhaskell Setup.hs configure[¶][6]

Prepare to build the package. Typically, this step checks that the target platform is capable of building the package, and discovers platform-specific features that are needed during the build.

The user may also adjust the behaviour of later stages using the options listed in the following subsections. In the simple build infrastructure, the values supplied via these options are recorded in a private file read by later stages.

If a user-supplied `configure` script is run (see the section on [System-dependent parameters][7] or on [More complex packages][8]), it is passed the [`--with-hc-pkg`][9], [`--prefix`][10], [`--bindir`][11], [`--libdir`][12], [`--dynlibdir`][13], [`--datadir`][14], [`--libexecdir`][15] and [`--sysconfdir`][16] options. In addition the value of the [`--with-compiler`][17] option is passed in a [`--with-hc-pkg`][18] option and all options specified with [`--configure-option`][19] are passed on.

In Cabal 2.0, support for a single positional argument was added to `runhaskell Setup.hs configure` This makes Cabal configure the specific component to be configured. Specified names can be qualified with `lib:` or `exe:` in case just a name is ambiguous (as would be the case for a package named `p` which has a library and an executable named `p`.) This has the following effects:

-   Subsequent invocations of `cabal build`, `register`, etc. operate only on the configured component.
    
-   Cabal requires all “internal” dependencies (e.g., an executable depending on a library defined in the same package) must be found in the set of databases via [`--package-db`][20] (and related flags): these dependencies are assumed to be up-to-date. A dependency can be explicitly specified using [`--dependency`][21] simply by giving the name of the internal library; e.g., the dependency for an internal library named `foo` is given as `--dependency=pkg-internal=pkg-1.0-internal-abcd`.
    
-   Only the dependencies needed for the requested component are required. Similarly, when [`--exact-configuration`][22] is specified, it’s only necessary to specify [`--dependency`][23] for the component. (As mentioned previously, you _must_ specify internal dependencies as well.)
    
-   Internal `build-tool-depends` and `build-tools` dependencies are expected to be in the `PATH` upon subsequent invocations of `setup`.
    

Full details can be found in the [Componentized Cabal proposal][24].

### 9.3.1. Programs used for building[¶][25]

The following options govern the programs used to process the source files of a package:

`--ghc` `or -g``,` `--jhc``,` `--lhc``,` `--uhc`[¶][26]

Specify which Haskell implementation to use to build the package. At most one of these flags may be given. If none is given, the implementation under which the setup script was compiled or interpreted is used.

`--with-compiler``=path or -w *path*`[¶][27]

Specify the path to a particular compiler. If given, this must match the implementation selected above. The default is to search for the usual name of the selected implementation.

This flag also sets the default value of the [`--with-hc-pkg`][28] option to the package tool for this compiler. Check the output of `runhaskell Setup.hs configure -v` to ensure that it finds the right package tool (or use [`--with-hc-pkg`][29] explicitly).

`--with-hc-pkg``=path`[¶][30]

Specify the path to the package tool, e.g. `ghc-pkg`. The package tool must be compatible with the compiler specified by [`--with-compiler`][31]. If this option is omitted, the default value is determined from the compiler selected.

`--with-prog``=path`[¶][32]

Specify the path to the program _prog_. Any program known to Cabal can be used in place of _prog_. It can either be a fully path or the name of a program that can be found on the program search path. For example: `--with-ghc=ghc-6.6.1` or `--with-cpphs=/usr/local/bin/cpphs`. The full list of accepted programs is not enumerated in this user guide. Rather, run `cabal install --help` to view the list.

`--prog-options``=options`[¶][33]

Specify additional options to the program _prog_. Any program known to Cabal can be used in place of _prog_. For example: `--alex-options="--template=mytemplatedir/"`. The _options_ is split into program options based on spaces. Any options containing embedded spaced need to be quoted, for example `--foo-options='--bar="C:\Program File\Bar"'`. As an alternative that takes only one option at a time but avoids the need to quote, use [`--prog-option`][34] instead.

`--prog-option``=option`[¶][35]

Specify a single additional option to the program _prog_. For passing an option that contains embedded spaces, such as a file name with embedded spaces, using this rather than [`--prog-options`][36] means you do not need an additional level of quoting. Of course if you are using a command shell you may still need to quote, for example `--foo-options="--bar=C:\Program File\Bar"`.

All of the options passed with either [`--prog-options`][37] or [`--prog-option`][38] are passed in the order they were specified on the configure command line.

### 9.3.2. Installation paths[¶][39]

The following options govern the location of installed files from a package:

`--prefix``=dir`[¶][40]

The root of the installation. For example for a global install you might use `/usr/local` on a Unix system, or `C:\Program Files` on a Windows system. The other installation paths are usually subdirectories of _prefix_, but they don’t have to be.

In the simple build system, _dir_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--bindir``=dir`[¶][41]

Executables that the user might invoke are installed here.

In the simple build system, _dir_ may contain the following path variables: `$prefix`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--libdir``=dir`[¶][42]

Object-code libraries are installed here.

In the simple build system, _dir_ may contain the following path variables: `$prefix`, `$bindir`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--dynlibdir``=dir`[¶][43]

Dynamic libraries are installed here.

By default, this is set to $libdir/$abi, which is usually not equal to $libdir/$libsubdir.

In the simple build system, _dir_ may contain the following path variables: `$prefix`, `$bindir`, `$libdir`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--libexecdir``=dir`[¶][44]

Executables that are not expected to be invoked directly by the user are installed here.

In the simple build system, _dir_ may contain the following path variables: `$prefix`, `$bindir`, `$libdir`, `$libsubdir`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--datadir``=dir`[¶][45]

Architecture-independent data files are installed here.

In the simple build system, _dir_ may contain the following path variables: `$prefix`, `$bindir`, `$libdir`, `$libsubdir`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--sysconfdir``=dir`[¶][46]

Installation directory for the configuration files.

In the simple build system, _dir_ may contain the following path variables: `$prefix`, `$bindir`, `$libdir`, `$libsubdir`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

In addition the simple build system supports the following installation path options:

`--libsubdir``=dir`[¶][47]

A subdirectory of _libdir_ in which libraries are actually installed. For example, in the simple build system on Unix, the default _libdir_ is `/usr/local/lib`, and _libsubdir_ contains the compiler ABI and package identifier, e.g. `x86_64-linux-ghc-8.0.2/mypkg-0.1.0-IxQNmCA7qrSEQNkoHSF7A`, so libraries would be installed in `/usr/local/lib/x86_64-linux-ghc-8.0.2/mypkg-0.1.0-IxQNmCA7qrSEQNkoHSF7A/`.

_dir_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--libexecsubdir``=dir`[¶][48]

A subdirectory of _libexecdir_ in which private executables are installed. For example, in the simple build system on Unix, the default _libexecdir_ is `/usr/local/libexec`, and _libsubdir_ is `x86_64-linux-ghc-8.0.2/mypkg-0.1.0`, so private executables would be installed in `/usr/local/libexec/x86_64-linux-ghc-8.0.2/mypkg-0.1.0/`

_dir_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--datasubdir``=dir`[¶][49]

A subdirectory of _datadir_ in which data files are actually installed.

_dir_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--docdir``=dir`[¶][50]

Documentation files are installed relative to this directory.

_dir_ may contain the following path variables: `$prefix`, `$bindir`, `$libdir`, `$libsubdir`, `$datadir`, `$datasubdir`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--htmldir``=dir`[¶][51]

HTML documentation files are installed relative to this directory.

_dir_ may contain the following path variables: `$prefix`, `$bindir`, `$libdir`, `$libsubdir`, `$datadir`, `$datasubdir`, `$docdir`, `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--program-prefix``=prefix`[¶][52]

Prepend _prefix_ to installed program names.

_prefix_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

`--program-suffix``=suffix`[¶][53]

Append _suffix_ to installed program names. The most obvious use for this is to append the program’s version number to make it possible to install several versions of a program at once: `--program-suffix='$version'`.

_suffix_ may contain the following path variables: `$pkgid`, `$pkg`, `$version`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`

#### 9.3.2.1. Path variables in the simple build system[¶][54]

For the simple build system, there are a number of variables that can be used when specifying installation paths. The defaults are also specified in terms of these variables. A number of the variables are actually for other paths, like `$prefix`. This allows paths to be specified relative to each other rather than as absolute paths, which is important for building relocatable packages (see [prefix independence][55]).

$prefix

The path variable that stands for the root of the installation. For an installation to be relocatable, all other installation paths must be relative to the `$prefix` variable.

$bindir

The path variable that expands to the path given by the [`--bindir`][56] configure option (or the default).

$libdir

As above but for [`--libdir`][57]

$libsubdir

As above but for [`--libsubdir`][58]

$dynlibdir

As above but for [`--dynlibdir`][59]

$datadir

As above but for [`--datadir`][60]

$datasubdir

As above but for [`--datasubdir`][61]

$docdir

As above but for [`--docdir`][62]

$pkgid

The name and version of the package, e.g. `mypkg-0.2`

$pkg

The name of the package, e.g. `mypkg`

$version

The version of the package, e.g. `0.2`

$compiler

The compiler being used to build the package, e.g. `ghc-6.6.1`

$os

The operating system of the computer being used to build the package, e.g. `linux`, `windows`, `osx`, `freebsd` or `solaris`

$arch

The architecture of the computer being used to build the package, e.g. `i386`, `x86_64`, `ppc` or `sparc`

$abitag

An optional tag that a compiler can use for telling incompatible ABI’s on the same architecture apart. GHCJS encodes the underlying GHC version in the ABI tag.

$abi

A shortcut for getting a path that completely identifies the platform in terms of binary compatibility. Expands to the same value as `$arch-$os-compiler-$abitag` if the compiler uses an abi tag, `$arch-$os-$compiler` if it doesn’t.

#### 9.3.2.2. Paths in the simple build system[¶][63]

For the simple build system, the following defaults apply:

#### 9.3.2.3. Prefix-independence[¶][64]

On Windows it is possible to obtain the pathname of the running program. This means that we can construct an installable executable package that is independent of its absolute install location. The executable can find its auxiliary files by finding its own path and knowing the location of the other files relative to `$bindir`. Prefix-independence is particularly useful: it means the user can choose the install location (i.e. the value of `$prefix`) at install-time, rather than having to bake the path into the binary when it is built.

In order to achieve this, we require that for an executable on Windows, all of `$bindir`, `$libdir`, `$dynlibdir`, `$datadir` and `$libexecdir` begin with `$prefix`. If this is not the case then the compiled executable will have baked-in all absolute paths.

The application need do nothing special to achieve prefix-independence. If it finds any files using `getDataFileName` and the [other functions provided for the purpose][65], the files will be accessed relative to the location of the current executable.

A library cannot (currently) be prefix-independent, because it will be linked into an executable whose file system location bears no relation to the library package.

### 9.3.3. Controlling Flag Assignments[¶][66]

Flag assignments (see [Resolution of Conditions and Flags][67]) can be controlled with the following command line options.

`-f` `flagname or -f -flagname`[¶][68]

Force the specified flag to `true` or `false` (if preceded with a `-`). Later specifications for the same flags will override earlier, i.e., specifying `-fdebug -f-debug` is equivalent to `-f-debug`

`--flags``=flagspecs`[¶][69]

Same as `-f`, but allows specifying multiple flag assignments at once. The parameter is a space-separated list of flag names (to force a flag to `true`), optionally preceded by a `-` (to force a flag to `false`). For example, `--flags="debug -feature1 feature2"` is equivalent to `-fdebug -f-feature1 -ffeature2`.

### 9.3.4. Building Test Suites[¶][70]

`--enable-tests`[¶][71]

Build the test suites defined in the package description file during the `build` stage. Check for dependencies required by the test suites. If the package is configured with this option, it will be possible to run the test suites with the `test` command after the package is built.

`--disable-tests`[¶][72]

(default) Do not build any test suites during the `build` stage. Do not check for dependencies required only by the test suites. It will not be possible to invoke the `test` command without reconfiguring the package.

`--enable-coverage`[¶][73]

Build libraries and executables (including test suites) with Haskell Program Coverage enabled. Running the test suites will automatically generate coverage reports with HPC.

`--disable-coverage`[¶][74]

(default) Do not enable Haskell Program Coverage.

### 9.3.5. Miscellaneous options[¶][75]

`--user`[¶][76]

Does a per-user installation. This changes the [default installation prefix][77]. It also allow dependencies to be satisfied by the user’s package database, in addition to the global database. This also implies a default of `--user` for any subsequent `install` command, as packages registered in the global database should not depend on packages registered in a user’s database.

`--global`[¶][78]

(default) Does a global installation. In this case package dependencies must be satisfied by the global package database. All packages in the user’s package database will be ignored. Typically the final installation step will require administrative privileges.

`--package-db``=db`[¶][79]

Allows package dependencies to be satisfied from this additional package database _db_ in addition to the global package database. All packages in the user’s package database will be ignored. The interpretation of _db_ is implementation-specific. Typically it will be a file or directory. Not all implementations support arbitrary package databases.

This pushes an extra db onto the db stack. The [`--global`][80] and [`--user`][81] mode switches add the respective \[Global\] and \[Global, User\] dbs to the initial stack. There is a compiler-implementation constraint that the global db must appear first in the stack, and if the user one appears at all, it must appear immediately after the global db.

To reset the stack, use `--package-db=clear`.

`--ipid``=ipid`[¶][82]

Specifies the _installed package identifier_ of the package to be built; this identifier is passed on to GHC and serves as the basis for linker symbols and the `id` field in a `ghc-pkg` registration. When a package has multiple components, the actual component identifiers are derived off of this identifier. E.g., an internal library `foo` from package `p-0.1-abcd` will get the identifier `p-0.1-abcd-foo`.

`--cid``=cid`[¶][83]

Specifies the _component identifier_ of the component being built; this is only valid if you are configuring a single component.

`--default-user-config``=file`[¶][84]

Allows a “default” `cabal.config` freeze file to be passed in manually. This file will only be used if one does not exist in the project directory already. Typically, this can be set from the global cabal `config` file so as to provide a default set of partial constraints to be used by projects, providing a way for users to peg themselves to stable package collections.

`--enable-optimization``[=n] or -O [n]`[¶][85]

(default) Build with optimization flags (if available). This is appropriate for production use, taking more time to build faster libraries and programs.

The optional _n_ value is the optimisation level. Some compilers support multiple optimisation levels. The range is 0 to 2. Level 0 is equivalent to [`--disable-optimization`][86], level 1 is the default if no _n_ parameter is given. Level 2 is higher optimisation if the compiler supports it. Level 2 is likely to lead to longer compile times and bigger generated code.

When optimizations are enabled, Cabal passes `-O2` to the C compiler.

`--disable-optimization`[¶][87]

Build without optimization. This is suited for development: building will be quicker, but the resulting library or programs will be slower.

`--enable-profiling`[¶][88]

Build libraries and executables with profiling enabled (for compilers that support profiling as a separate mode). For this to work, all libraries used by this package must also have been built with profiling support. For libraries this involves building an additional instance of the library in addition to the normal non-profiling instance. For executables it changes the single executable to be built in profiling mode.

This flag covers both libraries and executables, but can be overridden by the [`--enable-library-profiling`][89] flag.

See also the [`--profiling-detail`][90] flag below.

`--disable-profiling`[¶][91]

(default) Do not enable profiling in generated libraries and executables.

`--enable-library-profiling` `or -p`[¶][92]

As with [`--enable-profiling`][93] above, but it applies only for libraries. So this generates an additional profiling instance of the library in addition to the normal non-profiling instance.

The [`--enable-profiling`][94] flag controls the profiling mode for both libraries and executables, but if different modes are desired for libraries versus executables then use [`--enable-library-profiling`][95] as well.

`--disable-library-profiling`[¶][96]

(default) Do not generate an additional profiling version of the library.

`--profiling-detail``[=level]`[¶][97]

Some compilers that support profiling, notably GHC, can allocate costs to different parts of the program and there are different levels of granularity or detail with which this can be done. In particular for GHC this concept is called “cost centers”, and GHC can automatically add cost centers, and can do so in different ways.

This flag covers both libraries and executables, but can be overridden by the [`--library-profiling-detail`][98] flag.

Currently this setting is ignored for compilers other than GHC. The levels that cabal currently supports are:

default

For GHC this uses `exported-functions` for libraries and `toplevel-functions` for executables.

none

No costs will be assigned to any code within this component.

exported-functions

Costs will be assigned at the granularity of all top level functions exported from each module. In GHC specifically, this is for non-inline functions.

toplevel-functions

Costs will be assigned at the granularity of all top level functions in each module, whether they are exported from the module or not. In GHC specifically, this is for non-inline functions.

all-functions

Costs will be assigned at the granularity of all functions in each module, whether top level or local. In GHC specifically, this is for non-inline toplevel or where-bound functions or values.

This flag is new in Cabal-1.24. Prior versions used the equivalent of `none` above.

`--library-profiling-detail``[=level]`[¶][99]

As with [`--profiling-detail`][100] above, but it applies only for libraries.

The level for both libraries and executables is set by the [`--profiling-detail`][101] flag, but if different levels are desired for libraries versus executables then use [`--library-profiling-detail`][102] as well.

`--enable-library-vanilla`[¶][103]

(default) Build ordinary libraries (as opposed to profiling libraries). This is independent of the [`--enable-library-profiling`][104] option. If you enable both, you get both.

`--disable-library-vanilla`[¶][105]

Do not build ordinary libraries. This is useful in conjunction with [`--enable-library-profiling`][106] to build only profiling libraries, rather than profiling and ordinary libraries.

`--enable-library-for-ghci`[¶][107]

(default) Build libraries suitable for use with GHCi.

`--disable-library-for-ghci`[¶][108]

Not all platforms support GHCi and indeed on some platforms, trying to build GHCi libs fails. In such cases this flag can be used as a workaround.

`--enable-split-objs`[¶][109]

Use the GHC `-split-objs` feature when building the library. This reduces the final size of the executables that use the library by allowing them to link with only the bits that they use rather than the entire library. The downside is that building the library takes longer and uses considerably more memory.

`--disable-split-objs`[¶][110]

(default) Do not use the GHC `-split-objs` feature. This makes building the library quicker but the final executables that use the library will be larger.

`--enable-executable-stripping`[¶][111]

(default) When installing binary executable programs, run the `strip` program on the binary. This can considerably reduce the size of the executable binary file. It does this by removing debugging information and symbols. While such extra information is useful for debugging C programs with traditional debuggers it is rarely helpful for debugging binaries produced by Haskell compilers.

Not all Haskell implementations generate native binaries. For such implementations this option has no effect.

`--disable-executable-stripping`[¶][112]

Do not strip binary executables during installation. You might want to use this option if you need to debug a program using gdb, for example if you want to debug the C parts of a program containing both Haskell and C code. Another reason is if your are building a package for a system which has a policy of managing the stripping itself (such as some Linux distributions).

`--enable-shared`[¶][113]

Build shared library. This implies a separate compiler run to generate position independent code as required on most platforms.

`--disable-shared`[¶][114]

(default) Do not build shared library.

`--enable-static`[¶][115]

Build a static library. This passes `-staticlib` to GHC (available for iOS, and with 8.4 more platforms). The result is an archive `.a` containing all dependent haskell libararies combined.

`--disable-static`[¶][116]

(default) Do not build a static library.

`--enable-executable-dynamic`[¶][117]

Link dependent Haskell libraries into executables dynamically. The executable’s library dependencies must have been built as shared objects. This implies [`--enable-shared`][118] unless [`--disable-shared`][119] is explicitly specified.

`--disable-executable-dynamic`[¶][120]

(default) Link dependent Haskell libraries into executables statically. Non-Haskell (C) libraries are still linked dynamically, including libc, so the result is still not a fully static executable unless [`--enable-executable-static`][121] is given.

`--enable-executable-static`[¶][122]

Build fully static executables. This links all dependent libraries into executables statically, including libc.

`--disable-executable-static`[¶][123]

(default) Do not build fully static executables.

`--configure-option``=str`[¶][124]

An extra option to an external `configure` script, if one is used (see the section on [System-dependent parameters][125]). There can be several of these options.

An extra directory to search for C header files. You can use this flag multiple times to get a list of directories.

You might need to use this flag if you have standard system header files in a non-standard location that is not mentioned in the package’s `.cabal` file. Using this option has the same effect as appending the directory _dir_ to the `include-dirs` field in each library and executable in the package’s `.cabal` file. The advantage of course is that you do not have to modify the package at all. These extra directories will be used while building the package and for libraries it is also saved in the package registration information and used when compiling modules that use the library.

An extra directory to search for system libraries files. You can use this flag multiple times to get a list of directories.

An extra directory to search for frameworks (OS X only). You can use this flag multiple times to get a list of directories.

You might need to use this flag if you have standard system libraries in a non-standard location that is not mentioned in the package’s `.cabal` file. Using this option has the same affect as appending the directory _dir_ to the `extra-lib-dirs` field in each library and executable in the package’s `.cabal` file. The advantage of course is that you do not have to modify the package at all. These extra directories will be used while building the package and for libraries it is also saved in the package registration information and used when compiling modules that use the library.

`--dependency``[=pkgname=ipid]`[¶][126]

Specify that a particular dependency should used for a particular package name. In particular, it declares that any reference to _pkgname_ in a [`build-depends`][127] should be resolved to _ipid_.

`--exact-configuration`[¶][128]

This changes Cabal to require every dependency be explicitly specified using [`--dependency`][129], rather than use Cabal’s (very simple) dependency solver. This is useful for programmatic use of Cabal’s API, where you want to error if you didn’t specify enough [`--dependency`][130] flags.

`--allow-newer``[=pkgs]``,` `--allow-older``[=pkgs]`[¶][131]

Selectively relax upper or lower bounds in dependencies without editing the package description respectively.

The following description focuses on upper bounds and the [`--allow-newer`][132] flag, but applies analogously to [`--allow-older`][133] and lower bounds. [`--allow-newer`][134] and [`--allow-older`][135] can be used at the same time.

If you want to install a package A that depends on B >= 1.0 && < 2.0, but you have the version 2.0 of B installed, you can compile A against B 2.0 by using `cabal install --allow-newer=B A`. This works for the whole package index: if A also depends on C that in turn depends on B < 2.0, C’s dependency on B will be also relaxed.

Example:

$ cd foo
$ cabal configure
Resolving dependencies...
cabal: Could not resolve dependencies:
\[...\]
$ cabal configure --allow-newer
Resolving dependencies...
Configuring foo...

Additional examples:

\# Relax upper bounds in all dependencies.
$ cabal install --allow-newer foo

\# Relax upper bounds only in dependencies on bar, baz and quux.
$ cabal install --allow-newer\=bar,baz,quux foo

\# Relax the upper bound on bar and force bar\==2.1.
$ cabal install --allow-newer\=bar --constraint\="bar==2.1" foo

It’s also possible to limit the scope of [`--allow-newer`][136] to single packages with the `--allow-newer=scope:dep` syntax. This means that the dependency on `dep` will be relaxed only for the package `scope`.

Example:

\# Relax upper bound in foo's dependency on base; also relax upper bound in
\# every package's dependency on lens.
$ cabal install --allow-newer\=foo:base,lens

\# Relax upper bounds in foo's dependency on base and bar's dependency
\# on time; also relax the upper bound in the dependency on lens specified by
\# any package.
$ cabal install --allow-newer\=foo:base,lens --allow-newer\=bar:time

Finally, one can enable [`--allow-newer`][137] permanently by setting `allow-newer: True` in the `~/.cabal/config` file. Enabling ‘allow-newer’ selectively is also supported in the config file (`allow-newer: foo, bar, baz:base`).

`--constraint``=constraint`[¶][138]

Restrict solutions involving a package to given version bounds, flag settings, and other properties. For example, to consider only install plans that use version 2.1 of `bar` or do not use `bar` at all, write:

$ cabal install --constraint\="bar == 2.1"

Version bounds have the same syntax as [`build-depends`][139]. As a special case, the following prevents `bar` from being used at all:

\# Note: this is just syntax sugar for '> 1 && < 1', and is
\# supported by build-depends.
$ cabal install --constraint\="bar -none"

You can also specify flag assignments:

\# Require bar to be installed with the foo flag turned on and
\# the baz flag turned off.
$ cabal install --constraint\="bar +foo -baz"

To specify multiple constraints, you may pass the `constraint` option multiple times.

There are also some more specialized constraints, which most people don’t generally need:

\# Require that a version of bar be used that is already installed in
\# the global package database.
$ cabal install --constraint\="bar installed"

\# Require the local source copy of bar to be used.
\# (Note: By default, if we have a local package we will
\# automatically use it, so it will generally not be necessary to
\# specify this.)
$ cabal install --constraint\="bar source"

\# Require that bar have test suites and benchmarks enabled.
$ cabal install --constraint\="bar test" --constraint\="bar bench"

By default, constraints only apply to build dependencies ([`build-depends`][140]), build dependencies of build dependencies, and so on. Constraints normally do not apply to dependencies of the `Setup.hs` script of any package ([`custom-setup:setup-depends`][141]) nor do they apply to build tools ([`build-tool-depends`][142]) or the dependencies of build tools. To explicitly apply a constraint to a setup or build tool dependency, you can add a qualifier to the constraint as follows:

\# Example use of the 'any' qualifier. This constraint
\# applies to package bar anywhere in the dependency graph.
$ cabal install --constraint\="any.bar == 1.0"

\# Example uses of 'setup' qualifiers.

\# This constraint applies to package bar when it is a
\# dependency of any Setup.hs script.
$ cabal install --constraint\="setup.bar == 1.0"

\# This constraint applies to package bar when it is a
\# dependency of the Setup.hs script of package foo.
$ cabal install --constraint\="foo:setup.bar == 1.0"

`--preference``=preference`[¶][143]

Specify a soft constraint on versions of a package. The solver will attempt to satisfy these preferences on a “best-effort” basis.

`--disable-response-files`[¶][144]

Enable workaround for older versions of programs such as `ar` or `ld` that do not support response file arguments (i.e. `@file` arguments). You may want this flag only if you specify custom ar executable. For system `ar` or the one bundled with `ghc` on Windows the `cabal` should do the right thing and hence should normally not require this flag.

## 9.4. runhaskell Setup.hs build[¶][145]

Perform any preprocessing or compilation needed to make this package ready for installation.

This command takes the following options:

`--prog-options``=options``,` `--prog-option``=option`[¶][146]

These are mostly the same as the [options configure step][147]. Unlike the options specified at the configure step, any program options specified at the build step are not persistent but are used for that invocation only. The options specified at the build step are in addition not in replacement of any options specified at the configure step.

## 9.5. runhaskell Setup.hs haddock[¶][148]

Build the documentation for the package using [Haddock][149]. By default, only the documentation for the exposed modules is generated (but see the [`--executables`][150] and [`--internal`][151] flags below).

This command takes the following options:

`--hoogle`[¶][152]

Generate a file `dist/doc/html/`_pkgid_`.txt`, which can be converted by [Hoogle][153] into a database for searching. This is equivalent to running [Haddock][154] with the `--hoogle` flag.

`--html-location``=url`[¶][155]

Specify a template for the location of HTML documentation for prerequisite packages. The substitutions ([see listing][156]) are applied to the template to obtain a location for each package, which will be used by hyperlinks in the generated documentation. For example, the following command generates links pointing at [Hackage][157] pages:

$ runhaskell Setup.hs haddock \\
--html-location\='http://hackage.haskell.org/packages/archive/$pkg/latest/doc/html'

Here the argument is quoted to prevent substitution by the shell. If this option is omitted, the location for each package is obtained using the package tool (e.g. `ghc-pkg`).

`--executables`[¶][158]

Also run [Haddock][159] for the modules of all the executable programs. By default [Haddock][160] is run only on the exported modules.

`--internal`[¶][161]

Run [Haddock][162] for the all modules, including unexposed ones, and make [Haddock][163] generate documentation for unexported symbols as well.

`--css``=path`[¶][164]

The argument _path_ denotes a CSS file, which is passed to [Haddock][165] and used to set the style of the generated documentation. This is only needed to override the default style that [Haddock][166] uses.

`--hyperlink-source`[¶][167]

Generate [Haddock][168] documentation integrated with [HsColour][169] . First, [HsColour][170] is run to generate colourised code. Then [Haddock][171] is run to generate HTML documentation. Each entity shown in the documentation is linked to its definition in the colourised code.

`--hscolour-css``=path`[¶][172]

The argument _path_ denotes a CSS file, which is passed to [HsColour][173] as in

$ runhaskell Setup.hs hscolour --css\=\*path\*

## 9.6. runhaskell Setup.hs hscolour[¶][174]

Produce colourised code in HTML format using [HsColour][175]. Colourised code for exported modules is put in `dist/doc/html/`_pkgid_`/src`.

This command takes the following options:

`--executables`[¶][176]

Also run [HsColour][177] on the sources of all executable programs. Colourised code is put in `dist/doc/html/`_pkgid_/_executable_`/src`.

`--css``=path`[¶][178]

Use the given CSS file for the generated HTML files. The CSS file defines the colours used to colourise code. Note that this copies the given CSS file to the directory with the generated HTML files (renamed to `hscolour.css`) rather than linking to it.

## 9.7. runhaskell Setup.hs install[¶][179]

Copy the files into the install locations and (for library packages) register the package with the compiler, i.e. make the modules it contains available to programs.

The [install locations][180] are determined by options to [runhaskell Setup.hs configure][181].

This command takes the following options:

`--global`[¶][182]

Register this package in the system-wide database. (This is the default, unless the [`runhaskell Setup.hs configure --user`][183] option was supplied to the `configure` command.)

`--user`[¶][184]

Register this package in the user’s local package database. (This is the default if the [`runhaskell Setup.hs configure --user`][185] option was supplied to the `configure` command.)

## 9.8. runhaskell Setup.hs copy[¶][186]

Copy the files without registering them. This command is mainly of use to those creating binary packages.

This command takes the following option:

`--destdir``=path`[¶][187]

Specify the directory under which to place installed files. If this is not given, then the root directory is assumed.

## 9.9. runhaskell Setup.hs register[¶][188]

Register this package with the compiler, i.e. make the modules it contains available to programs. This only makes sense for library packages. Note that the `install` command incorporates this action. The main use of this separate command is in the post-installation step for a binary package.

This command takes the following options:

`--global`[¶][189]

Register this package in the system-wide database. (This is the default.)

`--user`[¶][190]

Register this package in the user’s local package database.

`--gen-script`[¶][191]

Instead of registering the package, generate a script containing commands to perform the registration. On Unix, this file is called `register.sh`, on Windows, `register.bat`. This script might be included in a binary bundle, to be run after the bundle is unpacked on the target system.

`--gen-pkg-config``[=path]`[¶][192]

Instead of registering the package, generate a package registration file (or directory, in some circumstances). This only applies to compilers that support package registration files which at the moment is only GHC. The file should be used with the compiler’s mechanism for registering packages. This option is mainly intended for packaging systems. If possible use the [`--gen-script`][193] option instead since it is more portable across Haskell implementations. The _path_ is optional and can be used to specify a particular output file to generate. Otherwise, by default the file is the package name and version with a `.conf` extension.

This option outputs a directory if the package requires multiple registrations: this can occur if internal/convenience libraries are used. These configuration file names are sorted so that they can be registered in order.

`--inplace`[¶][194]

Registers the package for use directly from the build tree, without needing to install it. This can be useful for testing: there’s no need to install the package after modifying it, just recompile and test.

This flag does not create a build-tree-local package database. It still registers the package in one of the user or global databases.

However, there are some caveats. It only works with GHC (currently). It only works if your package doesn’t depend on having any supplemental files installed — plain Haskell libraries should be fine.

## 9.10. runhaskell Setup.hs unregister[¶][195]

Deregister this package with the compiler.

This command takes the following options:

`--global`[¶][196]

Deregister this package in the system-wide database. (This is the default.)

`--user`[¶][197]

Deregister this package in the user’s local package database.

`--gen-script`[¶][198]

Instead of deregistering the package, generate a script containing commands to perform the deregistration. On Unix, this file is called `unregister.sh`, on Windows, `unregister.bat`. This script might be included in a binary bundle, to be run on the target system.

## 9.11. runhaskell Setup.hs clean[¶][199]

Remove any local files created during the `configure`, `build`, `haddock`, `register` or `unregister` steps, and also any files and directories listed in the [`extra-tmp-files`][200] field.

This command takes the following options:

`--save-configure``,` `-s`[¶][201]

Keeps the configuration information so it is not necessary to run the configure step again before building.

## 9.12. runhaskell Setup.hs test[¶][202]

Run the test suites specified in the package description file. Aside from the following flags, Cabal accepts the name of one or more test suites on the command line after `test`. When supplied, Cabal will run only the named test suites, otherwise, Cabal will run all test suites in the package.

`--builddir``=dir`[¶][203]

The directory where Cabal puts generated build files (default: `dist`). Test logs will be located in the `test` subdirectory.

`--human-log``=path`[¶][204]

The template used to name human-readable test logs; the path is relative to `dist/test`. By default, logs are named according to the template `$pkgid-$test-suite.log`, so that each test suite will be logged to its own human-readable log file. Template variables allowed are: `$pkgid`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag`, `$test-suite`, and `$result`.

`--machine-log``=path`[¶][205]

The path to the machine-readable log, relative to `dist/test`. The default template is `$pkgid.log`. Template variables allowed are: `$pkgid`, `$compiler`, `$os`, `$arch`, `$abi`, `$abitag` and `$result`.

`--show-details``=filter`[¶][206]

Determines if the results of individual test cases are shown on the terminal. May be `always` (always show), `never` (never show), `failures` (show only failed results), or `streaming` (show all results in real time).

`--test-options``=options`[¶][207]

Give extra options to the test executables.

`--test-option``=option`[¶][208]

Give an extra option to the test executables. There is no need to quote options containing spaces because a single option is assumed, so options will not be split on spaces.

`--test-wrapper``=path`[¶][209]

The wrapper script/application used to setup and tear down the test execution context. The text executable path and test arguments are passed as arguments to the wrapper and it is expected that the wrapper will return the test’s return code, as well as a copy of stdout/stderr.

## 9.13. runhaskell Setup.hs bench[¶][210]

Run the benchmarks specified in the package description file. Aside from the following flags, Cabal accepts the name of one or more benchmarks on the command line after `bench`. When supplied, Cabal will run only the named benchmarks, otherwise, Cabal will run all benchmarks in the package.

`--benchmark-options``=options`[¶][211]

Give extra options to the benchmark executables.

`--benchmark-option``=option`[¶][212]

Give an extra option to the benchmark executables. There is no need to quote options containing spaces because a single option is assumed, so options will not be split on spaces.

## 9.14. runhaskell Setup.hs sdist[¶][213]

Create a system- and compiler-independent source distribution in a file _package_\-_version_`.tar.gz` in the `dist` subdirectory, for distribution to package builders. When unpacked, the commands listed in this section will be available.

The files placed in this distribution are the package description file, the setup script, the sources of the modules named in the package description file, and files named in the `license-file`, `main-is`, `c-sources`, `asm-sources`, `cmm-sources`, `js-sources`, `data-files`, `extra-source-files` and `extra-doc-files` fields.

This command takes the following option:

`--snapshot`[¶][214]

Append today’s date (in “YYYYMMDD” format) to the version number for the generated source package. The original package is unaffected.

[1]: https://cabal.readthedocs.io/en/latest/setup-commands.html#building-and-installing-a-system-package "Permalink to this headline"
[2]: https://cabal.readthedocs.io/en/latest/nix-local-build-overview.html#nix-style-builds
[3]: https://cabal.readthedocs.io/en/latest/setup-commands.html#creating-a-binary-package "Permalink to this headline"
[4]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-setup-help "Permalink to this definition"
[5]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-setup-verbose "Permalink to this definition"
[6]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-configure "Permalink to this headline"
[7]: https://cabal.readthedocs.io/en/latest/cabal-package.html#system-dependent-parameters
[8]: https://cabal.readthedocs.io/en/latest/cabal-package.html#more-complex-packages
[9]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-hc-pkg
[10]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prefix
[11]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-bindir
[12]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libdir
[13]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dynlibdir
[14]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-datadir
[15]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libexecdir
[16]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-sysconfdir
[17]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-compiler
[18]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-hc-pkg
[19]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-configure-option
[20]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-package-db
[21]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dependency
[22]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-exact-configuration
[23]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dependency
[24]: https://github.com/ezyang/ghc-proposals/blob/master/proposals/0000-componentized-cabal.rst
[25]: https://cabal.readthedocs.io/en/latest/setup-commands.html#programs-used-for-building "Permalink to this headline"
[26]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-ghc "Permalink to this definition"
[27]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-compiler "Permalink to this definition"
[28]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-hc-pkg
[29]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-hc-pkg
[30]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-hc-pkg "Permalink to this definition"
[31]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-compiler
[32]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-with-prog "Permalink to this definition"
[33]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prog-options "Permalink to this definition"
[34]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prog-option
[35]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prog-option "Permalink to this definition"
[36]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prog-options
[37]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prog-options
[38]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prog-option
[39]: https://cabal.readthedocs.io/en/latest/setup-commands.html#installation-paths "Permalink to this headline"
[40]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-prefix "Permalink to this definition"
[41]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-bindir "Permalink to this definition"
[42]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libdir "Permalink to this definition"
[43]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dynlibdir "Permalink to this definition"
[44]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libexecdir "Permalink to this definition"
[45]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-datadir "Permalink to this definition"
[46]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-sysconfdir "Permalink to this definition"
[47]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libsubdir "Permalink to this definition"
[48]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libexecsubdir "Permalink to this definition"
[49]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-datasubdir "Permalink to this definition"
[50]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-docdir "Permalink to this definition"
[51]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-htmldir "Permalink to this definition"
[52]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-program-prefix "Permalink to this definition"
[53]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-program-suffix "Permalink to this definition"
[54]: https://cabal.readthedocs.io/en/latest/setup-commands.html#path-variables-in-the-simple-build-system "Permalink to this headline"
[55]: https://cabal.readthedocs.io/en/latest/setup-commands.html#prefix-independence
[56]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-bindir
[57]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libdir
[58]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-libsubdir
[59]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dynlibdir
[60]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-datadir
[61]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-datasubdir
[62]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-docdir
[63]: https://cabal.readthedocs.io/en/latest/setup-commands.html#paths-in-the-simple-build-system "Permalink to this headline"
[64]: https://cabal.readthedocs.io/en/latest/setup-commands.html#prefix-independence "Permalink to this headline"
[65]: https://cabal.readthedocs.io/en/latest/cabal-package.html#accessing-data-files
[66]: https://cabal.readthedocs.io/en/latest/setup-commands.html#controlling-flag-assignments "Permalink to this headline"
[67]: https://cabal.readthedocs.io/en/latest/cabal-package.html#resolution-of-conditions-and-flags
[68]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-f "Permalink to this definition"
[69]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-flags "Permalink to this definition"
[70]: https://cabal.readthedocs.io/en/latest/setup-commands.html#building-test-suites "Permalink to this headline"
[71]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-tests "Permalink to this definition"
[72]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-tests "Permalink to this definition"
[73]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-coverage "Permalink to this definition"
[74]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-coverage "Permalink to this definition"
[75]: https://cabal.readthedocs.io/en/latest/setup-commands.html#miscellaneous-options "Permalink to this headline"
[76]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-user "Permalink to this definition"
[77]: https://cabal.readthedocs.io/en/latest/setup-commands.html#paths-in-the-simple-build-system
[78]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-global "Permalink to this definition"
[79]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-package-db "Permalink to this definition"
[80]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-global
[81]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-user
[82]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-ipid "Permalink to this definition"
[83]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-cid "Permalink to this definition"
[84]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-default-user-config "Permalink to this definition"
[85]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-optimization "Permalink to this definition"
[86]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-optimization
[87]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-optimization "Permalink to this definition"
[88]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-profiling "Permalink to this definition"
[89]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-library-profiling
[90]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-profiling-detail
[91]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-profiling "Permalink to this definition"
[92]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-library-profiling "Permalink to this definition"
[93]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-profiling
[94]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-profiling
[95]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-library-profiling
[96]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-library-profiling "Permalink to this definition"
[97]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-profiling-detail "Permalink to this definition"
[98]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-library-profiling-detail
[99]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-library-profiling-detail "Permalink to this definition"
[100]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-profiling-detail
[101]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-profiling-detail
[102]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-library-profiling-detail
[103]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-library-vanilla "Permalink to this definition"
[104]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-library-profiling
[105]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-library-vanilla "Permalink to this definition"
[106]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-library-profiling
[107]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-library-for-ghci "Permalink to this definition"
[108]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-library-for-ghci "Permalink to this definition"
[109]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-split-objs "Permalink to this definition"
[110]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-split-objs "Permalink to this definition"
[111]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-executable-stripping "Permalink to this definition"
[112]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-executable-stripping "Permalink to this definition"
[113]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-shared "Permalink to this definition"
[114]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-shared "Permalink to this definition"
[115]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-static "Permalink to this definition"
[116]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-static "Permalink to this definition"
[117]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-executable-dynamic "Permalink to this definition"
[118]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-shared
[119]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-shared
[120]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-executable-dynamic "Permalink to this definition"
[121]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-executable-static
[122]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-enable-executable-static "Permalink to this definition"
[123]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-executable-static "Permalink to this definition"
[124]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-configure-option "Permalink to this definition"
[125]: https://cabal.readthedocs.io/en/latest/cabal-package.html#system-dependent-parameters
[126]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dependency "Permalink to this definition"
[127]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[128]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-exact-configuration "Permalink to this definition"
[129]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dependency
[130]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-dependency
[131]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-allow-newer "Permalink to this definition"
[132]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-allow-newer
[133]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-allow-newer
[134]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-allow-newer
[135]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-allow-newer
[136]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-allow-newer
[137]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-allow-newer
[138]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-constraint "Permalink to this definition"
[139]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[140]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-depends "package.cabal build-depends field"
[141]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-custom-setup-setup-depends "package.cabal custom-setup section setup-depends: field(since version: 1.24)"
[142]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-build-tool-depends "package.cabal build-tool-depends field(since version: 2.0)"
[143]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-preference "Permalink to this definition"
[144]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-disable-response-files "Permalink to this definition"
[145]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-build "Permalink to this headline"
[146]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-build-prog-options "Permalink to this definition"
[147]: https://cabal.readthedocs.io/en/latest/setup-commands.html#setup-configure
[148]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-haddock "Permalink to this headline"
[149]: http://www.haskell.org/haddock/
[150]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-executables
[151]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-internal
[152]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-hoogle "Permalink to this definition"
[153]: http://www.haskell.org/hoogle/
[154]: http://www.haskell.org/haddock/
[155]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-html-location "Permalink to this definition"
[156]: https://cabal.readthedocs.io/en/latest/setup-commands.html#paths-in-the-simple-build-system
[157]: http://hackage.haskell.org/
[158]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-executables "Permalink to this definition"
[159]: http://www.haskell.org/haddock/
[160]: http://www.haskell.org/haddock/
[161]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-internal "Permalink to this definition"
[162]: http://www.haskell.org/haddock/
[163]: http://www.haskell.org/haddock/
[164]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-css "Permalink to this definition"
[165]: http://www.haskell.org/haddock/
[166]: http://www.haskell.org/haddock/
[167]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-hyperlink-source "Permalink to this definition"
[168]: http://www.haskell.org/haddock/
[169]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[170]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[171]: http://www.haskell.org/haddock/
[172]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-haddock-hscolour-css "Permalink to this definition"
[173]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[174]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-hscolour "Permalink to this headline"
[175]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[176]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-hscolour-executables "Permalink to this definition"
[177]: http://www.cs.york.ac.uk/fp/darcs/hscolour/
[178]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-hscolour-css "Permalink to this definition"
[179]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-install "Permalink to this headline"
[180]: https://cabal.readthedocs.io/en/latest/setup-commands.html#installation-paths
[181]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-configure
[182]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-install-global "Permalink to this definition"
[183]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-user
[184]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-install-user "Permalink to this definition"
[185]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-configure-user
[186]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-copy "Permalink to this headline"
[187]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-copy-destdir "Permalink to this definition"
[188]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-register "Permalink to this headline"
[189]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-register-global "Permalink to this definition"
[190]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-register-user "Permalink to this definition"
[191]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-register-gen-script "Permalink to this definition"
[192]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-register-gen-pkg-config "Permalink to this definition"
[193]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-register-gen-script
[194]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-register-inplace "Permalink to this definition"
[195]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-unregister "Permalink to this headline"
[196]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-unregister-global "Permalink to this definition"
[197]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-unregister-user "Permalink to this definition"
[198]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-unregister-gen-script "Permalink to this definition"
[199]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-clean "Permalink to this headline"
[200]: https://cabal.readthedocs.io/en/latest/cabal-package.html#pkg-field-extra-tmp-files "package.cabal extra-tmp-files field"
[201]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-clean-save-configure "Permalink to this definition"
[202]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-test "Permalink to this headline"
[203]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-builddir "Permalink to this definition"
[204]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-human-log "Permalink to this definition"
[205]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-machine-log "Permalink to this definition"
[206]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-show-details "Permalink to this definition"
[207]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-test-options "Permalink to this definition"
[208]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-test-option "Permalink to this definition"
[209]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-test-wrapper "Permalink to this definition"
[210]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-bench "Permalink to this headline"
[211]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-benchmark-options "Permalink to this definition"
[212]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-test-benchmark-option "Permalink to this definition"
[213]: https://cabal.readthedocs.io/en/latest/setup-commands.html#runhaskell-setup-hs-sdist "Permalink to this headline"
[214]: https://cabal.readthedocs.io/en/latest/setup-commands.html#cmdoption-runhaskell-Setup.hs-sdist-snapshot "Permalink to this definition"

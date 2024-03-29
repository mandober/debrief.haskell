# cabal-install

The command-line interface for Cabal and Hackage

See the [Cabal web site][1] for more information.

The `cabal-install` package provides a command line tool named `cabal`. It uses the [Cabal][2] library and provides a user interface to the Cabal/[Hackage][3] build automation and package management system. It can build and install both local and remote packages, including dependencies.

The `cabal-install` package requires a number of other packages, most of which come with a standard GHC installation. It requires the [network][4] package, which is sometimes packaged separately by Linux distributions; for example, on Debian or Ubuntu, it is located in the "libghc6-network-dev" package.

`cabal` requires a few other Haskell packages that are not always installed. The exact list is specified in the [.cabal][5] file or in the [bootstrap.sh][6] file. All these packages are available from [Hackage][7].

Note that on some Unix systems you may need to install an additional zlib development package using your system package manager; for example, on Debian or Ubuntu, it is located in the "zlib1g-dev" package; on Fedora, it is located in the "zlib-devel" package. It is required because the Haskell zlib package uses the system zlib C library and header files.

The `cabal-install` package is now part of the [Haskell Platform][8], so you do not usually need to install it separately. However, if you are starting from a minimal GHC installation, you need to install `cabal-install` manually. Since it is an ordinary Cabal package, `cabal-install` can be built the standard way; to facilitate this, the process has been partially automated. It is described below.

## Quick start on Unix-like systems

As a convenience for users on Unix-like systems, there is a [bootstrap.sh][9] script that will download and install each of `cabal-install`'s dependencies in turn.

```
$ ./bootstrap.sh
```

It will download and install the dependencies. The script will install the library packages (vanilla, profiling and shared) into `$HOME/.cabal/` and the `cabal` program into `$HOME/.cabal/bin/`. If you don't want to install profiling and shared versions of the libraries, use

```
$ EXTRA_CONFIGURE_OPTS="" ./bootstrap.sh
```

You then have the choice either to place `$HOME/.cabal/bin` on your `$PATH` or move the `cabal` program to somewhere on your `$PATH`. Next, you can get the latest list of packages by running:

```
$ cabal update
```

This will also create a default configuration file, if it does not already exist, at `$HOME/.cabal/config`.

By default, `cabal` will install programs to `$HOME/.cabal/bin`. If you do not want to add this directory to your `$PATH`, you can change the setting in the config file; for example, you could use the following:

```
installdir: $HOME/bin
```

## Quick start on Windows systems

For Windows users, a precompiled program ([cabal.exe][10]) is provided. Download and put it somewhere on your `%PATH%` (for example, `C:\Program Files\Haskell\bin`.)

Next, you can get the latest list of packages by running:

```
$ cabal update
```

This will also create a default configuration file (if it does not already exist) at `C:\Documents and Settings\%USERNAME%\Application Data\cabal\config`.

There are two sets of commands: commands for working with a local project build tree and those for working with packages distributed from [Hackage][11].

For the list of the full set of commands and flags for each command, run:

```
$ cabal help
```

## Commands for developers for local build trees

The commands for local project build trees are almost the same as the `runghc Setup` command-line interface you may already be familiar with. In particular, it has the following commands:

-   `cabal configure`
-   `cabal build`
-   `cabal haddock`
-   `cabal clean`
-   `cabal sdist`

The `install` command is somewhat different; it is an all-in-one operation. If you run `cabal install` in your build tree, it will configure, build, and install. It takes all the flags that `configure` takes such as `--global` and `--prefix`.

In addition, `cabal` will download and install any dependencies that are not already installed. It can also rebuild packages to ensure a consistent set of dependencies.

## Commands for released Hackage packages

```
$ cabal update
```

This command gets the latest list of packages from the [Hackage][12] server. On occasion, this command must be run manually--for instance, if you want to install a newly released package.

```
$ cabal install xmonad
```

This command installs one or more named packages, and all their dependencies, from Hackage. By default, it installs the latest available version; however, you may specify exact versions or version ranges. For example, `cabal install alex-2.2` or `cabal install parsec < 3`.

```
$ cabal list xml
```

This does a search of the installed and available packages. It does a case-insensitive substring match on the package name.

[1]: http://www.haskell.org/cabal/
[2]: chrome-extension://hackage.haskell.org/package/Cabal/README.md
[3]: http://hackage.haskell.org/
[4]: http://hackage.haskell.org/package/network
[5]: chrome-extension://hackage.haskell.org/package/cabal-install-3.4.0.0/cabal-install.cabal
[6]: chrome-extension://hackage.haskell.org/package/cabal-install-3.4.0.0/bootstrap.sh
[7]: http://hackage.haskell.org/
[8]: http://www.haskell.org/platform/
[9]: chrome-extension://hackage.haskell.org/package/cabal-install-3.4.0.0/bootstrap.sh
[10]: http://www.haskell.org/cabal/release/cabal-install-latest/
[11]: http://hackage.haskell.org/
[12]: http://hackage.haskell.org/

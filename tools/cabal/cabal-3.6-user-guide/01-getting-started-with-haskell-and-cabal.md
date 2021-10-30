# 1. Getting Started with Haskell and Cabal

## 1.2. Creating a new application

Let's start by creating a simple Haskell application from scratch where we'll learn about a Haskell package's directory structure, how to run the executable, and how to add external dependencies.

### 1.2.1. Initializing the application

Start by creating a `hs92` directory to hold the project files, these instructions work in unix shells and PowerShell (if you're on Windows).

First create the new directory matching the name you intended for the project. Here, we'll use "hs92" for the project name. Change into that newly created folder so we can **initialize the project from within the project's dir**.

```bash
mkdir hs92
cd hs92
cabal init --cabal-version=3.4 --license=MIT -p hs92
```

Note: `cabal-version` refers to the [version of the .cabal file format specification][3], that can be different from the versions of the cabal library and tool in use. It is common to use a slightly older cabal-version, to strike a compromise between feature availability and backward compatibility.

The `cabal init ...` will generate the following files:
- CHANGELOG.md
- Main.hs
- hs92.cabal
- Setup.hs

`Main.hs` is where your package's code lives. By default `cabal init` creates an executable with the same name as the package (`hs92` in this case); you can instruct `cabal init` to generate just a library (with `--lib`) or both a library and executable with (`--libandexe`); for the full set of options see `cabal init --help`.

`hs92.cabal` is Cabal's metadata file which describes your package and its dependencies. We'll be updating this file in a little bit when we add an external dependency to our package.

To fire up the VSCode editor with the current project:

```bash
code .
# if it doesn't work, try with the entire nix-coded win-path to code.exe
# C:\Users\ivan\AppData\Local\Programs\vscode
/mnt/c/Users/ivan/AppData/Local/Programs/vscode/code.exe .
```



### 1.2.2. Running the application [4]

As mentioned above, `cabal init` with no arguments generates a package with a single executable that prints `"Hello, Haskell!"` to the terminal. To run the executable enter the following command:

`cabal run hs92`

You should see the following output in the terminal:

$ cabal run hs92
...
Hello, Haskell!

Notice that we didn't need to run a build command before `cabal run`, this is because `cabal run` first determines if the code needs to be re-built before running the executable. If you just want to build a target you can do so with `cabal build`:

`cabal build hs92`

### 1.2.3. Adding dependencies [5]

Next we'll add an external dependency to our application. [Hackage][6] is the Haskell community's central package archive of open source software.

In our application, we'll use a package called [haskell-say][7] to print text to the terminal with some embellishment.

Tip

If you installed `cabal` a while ago but haven't used it recently you may need to update the package index, you can do this by running `cabal update`.

In our `hs92.cabal` file we'll update the `build-depends` attribute of the `executable hs92` section to include `haskell-say`:

executable hs92
 main-is: Main.hs
 build-depends:
        base \>=4.11 && <4.12,
        haskell-say ^>=1.0.0.0

Note

`^>=1.0.0.0` means use version 1.0.0.0 of the library or any more recent minor release with the same major version.

Next we'll update `Main.hs` to use the `HaskellSay` library:

module Main where

import HaskellSay (haskellSay)

main :: IO ()
main \=
  haskellSay "Hello, Haskell! You're using a function from another package!"

`import HaskellSay (haskellSay)` brings the `haskellSay` function from the module named `HaskellSay` into scope. The `HaskellSay` module is defined in the `haskell-say` packages that we added a dependency on above.

Now you can build and re-run your code to see the new output:

$ cabal run
Hello, Haskell! You're using a function from anothe package!


[1]: https://cabal.readthedocs.io/en/latest/getting-started.html#creating-a-new-application "Permalink to this headline"
[2]: https://cabal.readthedocs.io/en/latest/getting-started.html#initializing-the-application "Permalink to this headline"
[3]: https://cabal.readthedocs.io/en/latest/file-format-changelog.html
[4]: https://cabal.readthedocs.io/en/latest/getting-started.html#running-the-application "Permalink to this headline"
[5]: https://cabal.readthedocs.io/en/latest/getting-started.html#adding-dependencies "Permalink to this headline"
[6]: https://hackage.haskell.org/
[7]: https://hackage.haskell.org/package/haskell-say

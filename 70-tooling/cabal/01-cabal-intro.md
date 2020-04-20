# Cabal intro

Haskell's CABAL, i.e. *Common Architecture for Building Applications and Libraries*, is the standard package system for Haskell software. It helps people configure, build, install and distribute Haskell software.

Cabal deals with:
- project manager
- package manager
- resolving dependencies (packages and their version)
- testing
- documentation
- building

Package and dependency refer to the program you've built along with the interlinked elements of that program, which can include the code you write, the modules you import, tests you have associated with the project, and documentation of the project.

Cabal exists to help organize all this and make sure all dependencies are properly in scope. It also builds the project, that is, it turns all this into an executable standalone program.

## cabal command
There is a command line tool called `cabal` for working with *Cabal packages*. It helps with installing existing packages and also helps people developing their own packages. It can be used to work with local packages or to install packages from online package archives, including automatically installing dependencies. By default it is configured to use *Hackage* which is Haskell’s central package archive that contains thousands of libraries and applications in the Cabal package format.

## Project structure

A project layout that isoften used:
- `cabal init` to scaffold the project (configs, license, etc.)
- `git init` for version control
- `/src` directory for the code you author
- `/src/Main.hs` as the main Haskell file, the entry point into the app
- `/test` directory for tests

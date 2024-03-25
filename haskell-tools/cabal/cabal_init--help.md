Create a new .cabal package file.

Usage: cabal init [FLAGS]

Create a .cabal, Setup.hs, and optionally a LICENSE file.

Calling init with no arguments creates an executable, guessing as many options
as possible. The interactive mode can be invoked by the -i/--interactive flag,
which will try to guess as much as possible and prompt you for the rest. You
can change init to always be interactive by setting the interactive flag in
your configuration file. Command-line arguments are provided for scripting
purposes.

Flags for init:
 -h --help                           Show this help text
 -i --interactive                    Enable interactive mode.
 -n --non-interactive                Disable interactive mode.
 -q --quiet                          Do not generate log messages to stdout.
    --no-comments                    Do not generate explanatory comments in
                                     the .cabal file.
 -m --minimal                        Generate a minimal .cabal file, that is,
                                     do not include extra empty fields. Also
                                     implies --no-comments.
    --overwrite                      Overwrite any existing .cabal, LICENSE, or
                                     Setup.hs files without warning.
    --package-dir=DIRECTORY          Root directory of the package (default =
                                     current directory).
 -p --package-name=PACKAGE           Name of the Cabal package to create.
    --version=VERSION                Initial version of the package.
    --cabal-version=CABALSPECVERSION Version of the Cabal specification.
 -l --license=LICENSE                Project license.
 -a --author=NAME                    Name of the project's author.
 -e --email=EMAIL                    Email address of the maintainer.
 -u --homepage=URL                   Project homepage and/or repository.
 -s --synopsis=TEXT                  Short project synopsis.
 -c --category=CATEGORY              Project category.
 -x --extra-source-file=FILE         Extra source file to be distributed with
                                     tarball.
    --lib                            Build a library.
    --exe                            Build an executable.
    --libandexe                      Build a library and an executable.
    --tests                          Generate a test suite for the library.
    --test-dir=DIR                   Directory containing tests.
    --simple                         Create a simple project with sensible
                                     defaults.
    --main-is=FILE                   Specify the main module.
    --language=LANGUAGE              Specify the default language.
 -o --expose-module=MODULE           Export a module from the package.
    --extension=EXTENSION            Use a LANGUAGE extension (in the
                                     other-extensions field).
 -d --dependency=PACKAGE             Package dependency.
    --application-dir=DIR            Directory containing package application
                                     executable.
    --source-dir=DIR                 Directory containing package library
                                     source.
    --build-tool=TOOL                Required external build tool.
 -w --with-compiler=PATH             give the path to a particular compiler
 -v --verbose[=n]                    Control verbosity (n is 0--3, default
                                     verbosity level is 1)

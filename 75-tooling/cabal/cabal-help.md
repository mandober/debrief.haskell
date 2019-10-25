# cabal

Command line interface to the Haskell Cabal infrastructure.

See http://www.haskell.org/cabal/ for more information.


Usage: `cabal [GLOBAL FLAGS] [COMMAND [FLAGS]]`

[global]
update           Updates list of known packages.
install          Install packages.

help             Help about commands.
info             Display detailed information about a particular package.
list             List packages matching a search string.
fetch            Downloads packages for later installation.
user-config      Display and update the user's global cabal configuration.

[package]
get              Download/Extract a package's source code (repository).
init             Create a new .cabal package file (interactively).

configure        Prepare to build the package.
build            Compile all/specific components.
clean            Clean up after a build.

run              Builds and runs an executable.
repl             Open an interpreter session for the given component.
test             Run all/specific tests in the test suite.
bench            Run all/specific benchmarks.

check            Check the package for common mistakes.
sdist            Generate a source distribution file (.tar.gz).
upload           Uploads source packages or documentation to Hackage.
report           Upload build reports to a remote server.

freeze           Freeze dependencies.
gen-bounds       Generate dependency bounds.
outdated         Check for outdated dependencies
doctest          Run doctest tests.
haddock          Generate Haddock HTML documentation.
hscolour         Generate HsColour colourised code, in HTML format.
copy             Copy the files of all/specific components to install locations.
register         Register this package with the compiler.
reconfigure      Reconfigure the package if necessary.

[sandbox]
sandbox          Create/modify/delete a sandbox.
exec             Give a command access to the sandbox package repository.
repl             Open interpreter with access to sandbox packages.

[new-style projects (beta)]
new-build        Compile targets within the project.
new-configure    Add extra project configuration
new-repl         Open an interactive session for the given component.
new-run          Run an executable.
new-test         Run test-suites
new-bench        Run benchmarks
new-freeze       Freeze dependencies.
new-haddock      Build Haddock documentation

For more information about a command use:
`cabal COMMAND --help` or 
`cabal help COMMAND`

To install Cabal packages from hackage use:
`cabal install foo [--dry-run]`

Occasionally you need to update the list of available packages:
`cabal update`


Global flags:
 -h --help                         Show this help text
 -V --version                      Print version information
    --numeric-version              Print just the version number

    --config-file=FILE             Set an alternate location for the config file
    --sandbox-config-file=FILE     Set an alternate location for the sandbox config file (default: `./cabal.sandbox.config`)
    --default-user-config=FILE     Set a location for a `cabal.config` file for projects without their own `cabal.config` freeze file.

    --require-sandbox              Enable requiring the presence of a sandbox for sandbox-aware commands
    --no-require-sandbox           Disable requiring the presence of a sandbox for sandbox-aware commands
    --ignore-sandbox               Ignore any existing sandbox
    --ignore-expiry                Ignore expiry dates on signed metadata (use only in exceptional circumstances)
    --http-transport=HttpTransport Set a transport for http(s) requests.  Accepts 'curl', 'wget', 'powershell', and 'plain-http'. (default: 'curl')
    --enable-nix                   Enable Nix integration: run commands through nix-shell if a 'shell.nix' file exists
    --disable-nix                  Disable Nix integration: run commands through nix-shell if a 'shell.nix' file exists


You can edit the cabal configuration file to set defaults:
`C:\Users\ivan\AppData\Roaming\cabal\config`

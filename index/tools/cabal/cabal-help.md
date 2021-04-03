# cabal

Command line interface to the Haskell Cabal infrastructure.
http://www.haskell.org/cabal/

## Usage

`cabal [GLOBAL FLAGS] [COMMAND [FLAGS]]`


## Commands

[global]
update            Updates list of known packages.
install           Install packages.
help              Help about commands.
info              Display detailed information about a particular package.
list              List packages matching a search string.
fetch             Downloads packages for later installation.
user-config       Display and update the user's global cabal configuration.

[package]
get               Download/Extract a package's source code (repository).
init              Create a new .cabal package file.

configure         Add extra project configuration
build             Compile targets within the project.
clean             Clean the package store and remove temporary files.

run               Run an executable.
repl              Open an interactive session for the given component.
test              Run test-suites
bench             Run benchmarks

check             Check the package for common mistakes.
sdist             Generate a source distribution file (.tar.gz).
upload            Uploads source packages or documentation to Hackage.
report            Upload build reports to a remote server.

freeze            Freeze dependencies.
gen-bounds        Generate dependency bounds.
outdated          Check for outdated dependencies
haddock           Build Haddock documentation
hscolour          Generate HsColour colourised code, in HTML format.
exec              Give a command access to the store.


[new-style-projects] **beta**
new-build         Compile targets within the project.
new-configure     Add extra project configuration
new-repl          Open an interactive session for the given component.
new-run           Run an executable.
new-test          Run test-suites
new-bench         Run benchmarks
new-freeze        Freeze dependencies.
new-haddock       Build Haddock documentation
new-exec          Give a command access to the store.
new-update        Updates list of known packages.
new-install       Install packages.
new-clean         Clean the package store and remove temporary files.
new-sdist         Generate a source distribution file (.tar.gz).


[new-style_projects] **forwards compatible aliases**
v2-build          Compile targets within the project.
v2-configure      Add extra project configuration
v2-repl           Open an interactive session for the given component.
v2-run            Run an executable.
v2-test           Run test-suites
v2-bench          Run benchmarks
v2-freeze         Freeze dependencies.
v2-haddock        Build Haddock documentation
v2-exec           Give a command access to the store.
v2-update         Updates list of known packages.
v2-install        Install packages.
v2-clean          Clean the package store and remove temporary files.
v2-sdist          Generate a source distribution file (.tar.gz).

[legacy-command-aliases]
v1-build          Compile all/specific components.
v1-configure      Prepare to build the package.
v1-repl           Open an interpreter session for the given component.
v1-run            Builds and runs an executable.
v1-test           Run all/specific tests in the test suite.
v1-bench          Run all/specific benchmarks.
v1-freeze         Freeze dependencies.
v1-haddock        Generate Haddock HTML documentation.
v1-exec           Give a command access to the sandbox package repository.
v1-update         Updates list of known packages.
v1-install        Install packages.
v1-clean          Clean up after a build.
v1-sdist          Generate a source distribution file (.tar.gz).
v1-doctest        Run doctest tests.
v1-copy           Copy files of all/specific components to install locations.
v1-register       Register this package with the compiler.
v1-reconfigure    Reconfigure the package if necessary.
v1-sandbox        Create/modify/delete a sandbox.


## More info about a command

For more info about a command use:
`cabal COMMAND --help`
or
`cabal help COMMAND`

To install Cabal packages from hackage use:
`cabal install ${PKG} [--dry-run]`

Occasionally you need to update the list of available packages:
`cabal update`



## Global flags

You can edit the cabal configuration file to set defaults:
*/home/ivan/.cabal/config*

`--config-file=FILE`
Set alt location for the config file

`--default-user-config=FILE`
Set location for *cabal.config* file for projects
without their own *cabal.config* freeze file.

`--sandbox-config-file=FILE`
alt location for sandbox config file. default: *./cabal.sandbox.config*
`--require-sandbox`
Enable requiring the presence of a sandbox for sandbox-aware commands
`--no-require-sandbox`
Disable requiring the presence of a sandbox for sandbox-aware commands
`--ignore-sandbox`
Ignore any existing sandbox

`--enable-nix`
run commands through nix-shell if a 'shell.nix' file exists
`--disable-nix`
run commands through nix-shell if a 'shell.nix' file exists

`--ignore-expiry`
Ignore expiry dates on signed metadata
(use only in exceptional circumstances)

`--http-transport=HttpTransport`
Set a transport for http(s) requests.
Accepts 'curl', 'wget', 'powershell', and 'plain-http'
default: 'curl'


-h --help              Show this help text
-V --version           Print version information
   --numeric-version   Print just the version number

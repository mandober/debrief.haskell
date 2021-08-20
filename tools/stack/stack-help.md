## Stack

stack - The Haskell Tool Stack
stack's documentation is available at https://docs.haskellstack.org/

## Usage

stack [--help] [--version] [--numeric-version] [--hpack-numeric-version]
      [--docker*] [--nix*] ([--verbosity VERBOSITY] | [-v|--verbose] |
      [--silent]) [--[no-]time-in-log] [--stack-root STACK-ROOT]
      [--work-dir WORK-DIR] [--[no-]system-ghc] [--[no-]install-ghc]
      [--arch ARCH] [--ghc-variant VARIANT] [--ghc-build BUILD]
      [-j|--jobs JOBS] [--extra-include-dirs DIR] [--extra-lib-dirs DIR]
      [--with-gcc PATH-TO-GCC] [--with-hpack HPACK]
      [--[no-]skip-ghc-check] [--[no-]skip-msys] [--local-bin-path DIR]
      [--[no-]modify-code-page] [--[no-]allow-different-user]
      [--[no-]dump-logs] [--resolver RESOLVER] [--compiler COMPILER]
      [--[no-]terminal] [--color WHEN] [--terminal-width INT]
      [--stack-yaml STACK-YAML] COMMAND|FILE


## Available options

--help                   Show this help text
--version                Show version
--numeric-version        Show only version number
--hpack-numeric-version  Show only hpack's version number
--docker*                Run 'stack --docker-help' for details
--nix*                   Run 'stack --nix-help' for details
--verbosity VERBOSITY    Verbosity: silent, error, warn, info, debug
--verbose, -v            Enable verbose mode: verbosity level "debug"
--silent                 Enable silent mode: verbosity level "silent"

  --[no-]time-in-log       Enable/disable inclusion of timings in logs, for the
                           purposes of using diff with logs
  --stack-root STACK-ROOT  Absolute path to the global stack root directory
                           (Overrides any STACK_ROOT environment variable)
  --work-dir WORK-DIR      Relative path of work directory (Overrides any
                           STACK_WORK environment variable, default is
                           '.stack-work')
  --[no-]system-ghc        Enable/disable using the system installed GHC (on the
                           PATH) if it is available and its version matches.
                           Disabled by default.
  --[no-]install-ghc       Enable/disable downloading and installing GHC if
                           necessary (can be done manually with stack setup)
  --arch ARCH              System architecture, e.g. i386, x86_64
  --ghc-variant VARIANT    Specialized GHC variant, e.g. integersimple
                           (incompatible with --system-ghc)
  --ghc-build BUILD        Specialized GHC build, e.g. 'gmp4' or 'standard'
                           (usually auto-detected)
  -j,--jobs JOBS           Number of concurrent jobs to run
  --extra-include-dirs DIR Extra directories to check for C header files
  --extra-lib-dirs DIR     Extra directories to check for libraries
  --with-gcc PATH-TO-GCC   Use gcc found at PATH-TO-GCC
  --with-hpack HPACK       Use HPACK executable (overrides bundled Hpack)
  --[no-]skip-ghc-check    Enable/disable skipping the GHC version and
                           architecture check
  --[no-]skip-msys         Enable/disable skipping the local MSYS installation
                           (Windows only)
  --local-bin-path DIR     Install binaries to DIR
  --[no-]modify-code-page  Enable/disable setting the codepage to support UTF-8
                           (Windows only)
  --[no-]allow-different-user
                           Enable/disable permission for users other than the
                           owner of the stack root directory to use a stack
                           installation (POSIX only)
  --[no-]dump-logs         Enable/disable dump the build output logs for local
                           packages to the console
  --resolver RESOLVER      Override resolver in project file
  --compiler COMPILER      Use the specified compiler
  --[no-]terminal          Enable/disable overriding terminal detection in the
                           case of running in a false terminal
  --color WHEN             Specify when to use color in output; WHEN is
                           'always', 'never', or 'auto'
  --terminal-width INT     Specify the width of the terminal, used for
                           pretty-print messages
  --stack-yaml STACK-YAML  Override project stack.yaml file (overrides any
                           STACK_YAML environment variable)


## Available commands

new
  Create a new project from a template.
  Run `stack templates` to see available templates.
  You can also specify a local file or a remote URL as a template.

templates
  List the templates available for `stack new`, drawn from:
  https://github.com/commercialhaskell/stack-templates
  Also accepts a template from local file or a remote URL

init
  Create stack project config from cabal or hpack package specifications

build
  Build the package(s) in this directory/configuration

hoogle
  Run hoogle, the Haskell API search engine
  Use `stack exec` syntax to pass Hoogle arguments, 
  e.g. `stack hoogle -- --count=20`

solver   Add missing extra-deps to stack project config
setup    Get the appropriate GHC for your project
path     Print out handy path information
ls       List command (supports snapshots and dependencies)

upgrade  Upgrade to the latest stack
upload   Upload a package to Hackage
unpack   Unpack one or more packages locally
update   Update the package index
clean    Clean the local packages

sdist    Create source distribution tarballs
dot      Visualize your project's dependency graph using Graphviz dot

ghci     Run ghci in the context of package(s) (experimental)
repl     alias for `ghci`

exec               Execute a command
ghc                Run ghc
runghc             Run runghc
runhaskell         Run runghc (alias for 'runghc')
script             Run a Stack Script
list-dependencies  List the dependencies
query              Query general build information (experimental)
ide                IDE-specific commands
docker             Subcommands specific to Docker use
config             Subcommands specific to modifying stack.yaml files
image              Subcommands specific to imaging
hpc                Subcommands specific to Haskell Program Coverage


### Shortcuts

install   Shortcut for `build --copy-bins`
test      Shortcut for `build --test`
bench     Shortcut for `build --bench`
haddock   Shortcut for `build --haddock`

eval      Evaluate some haskell code inline, `stack exec ghc -- -e CODE`

### Deprecated

uninstall
  This command performs no actions, and is present for documentation only

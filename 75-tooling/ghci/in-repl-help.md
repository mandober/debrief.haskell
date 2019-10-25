# Help system inside the REPL

`ghci> :?`

## Commands available from the prompt

### General
* `it`          Value of the last evaluated expr
* :help, :?     Display this list of commands
* :quit         Exit GHCi

### Shell
* :cd <dir>          cd to <dir>
* :edit <file>       Edit file
* :edit              Edit last module
* :!<cmd>            Run the shell command <cmd>
* :etags [<file>]    Create tags <file> for Emacs. Default: "TAGS"
* :ctags[!] [<file>] Create tags file <file> for Vi. Default: "tags".
  To use regex instead of line number `!:`

### Commands
* :                  Repeat last command
* <stmnt>            Evaluate/run <stmnt>
* :{\n ... \n:}\n    Multiline command
* :cmd <expr>        Run the commands returned by `<expr>::IO String`
* :undef <cmd>       Undefine user-defined command :<cmd>
* :def <cmd> <expr>  Define command :<cmd>. 
  Latter cmds have precedence, `::<cmd>` is always a builtin command.

### Modules
* :load[!] [*]<mod...>   Load modules and their deps; defer type errors `!:`
* :reload[!]             Reload the current module set; defer type errors `!:`
* :edit                  Edit last module
* :add [*]<mod ...>      Add modules to the current target set
* :issafe [<mod>]           Display safe haskell information of module
* :module [+/-] [*]<mod...> Set the context for expression evaluation
* :browse[!] [[*]<mod>]     Display names defined by module
        more details `!:`;  all top-level names `*:`

### Info
* :type <expr>          Show the type of <expr>
* :info[!] [<name...>]  Show info about names; don't filter instances `!:`
* :kind[!] <type>       Show the kind of <type>; print normalised type `!:`

### Run
* :main [<args> ...]         Run the main function with the given args
* :run function [<args...>]  Run function with given args
* :script <file>             Run script <file>

### Misc
* :complete <dom> [<rng>] <s>  List completions for partial input string



## Commands for changing settings

* :set <option> ...           set options
* :seti <option> ...          set options for interactive evaluation only
* :set args <arg> ...         set the arguments returned by `System.getArgs`
* :set prog <progname>        set the value returned by `System.getProgName`
* :set prompt <prompt>        set the prompt used in GHCi
* :set prompt2 <prompt>       set the continuation prompt used in GHCi
* :set editor <cmd>           set the command used for `:edit`
* :set stop [<n>] <cmd>       set the command to run when a breakpoint is hit
* :unset <option> ...         unset options


## Options for :set and :unset

* `+t`  enable printing inferred type after evaluation
* `+m`  enable multiline commands
* `+s`  enable printing of timing/memory stats after each evaluation
* `+r`     Revert top-level expressions after each evaluation
* `+c`     Collect type/location info after loading modules
* -FLAGS   Most GHCi command line flags can also be set here:
  - `:set -XFlexibleInstances`
  - `:set -XNoImplicitPrelude`  Do not load the Prelude
  - `:set -fglasgow-exts`       Different type sigs (forall)
  - `:set -v2`
  - For GHCi-specific flags see: User Guide › Flag reference › Interactive-mode


> :t quot
quot :: Integral a => a -> a -> a
> :set -fglasgow-exts
> :t quot
quot :: forall {a}. Integral a => a -> a -> a



## Commands for displaying information

* :show bindings  Show bindings made in the current repl session
* :show breaks    Show the active breakpoints
* :show context   Show the breakpoint context
* :show imports   Show the current imports
* :show linker    Show current linker state
* :show modules   Show the currently loaded modules
* :show packages  Show the currently active package flags
* :show paths     Show the currently active search paths
* :show language  Show the currently active language flags
* :showi language Show language flags for interactive evaluation
* :show <setting> Show value of <setting>,one of [args,prog,prompt,editor,stop]




## Commands for debugging

* :abandon                    at a breakpoint, abandon current computation
* :back [<n>]                 go back in the history N steps (after :trace)
* :break [<mod>] <l> [<col>]  set a breakpoint at the specified location
* :break <name>               set a breakpoint on the specified function
* :continue                   resume after a breakpoint
* :delete <number>            delete the specified breakpoint
* :delete *                   delete all breakpoints
* :force <expr>               print <expr>, forcing unevaluated parts
* :forward [<n>]              go forward in the history N step s(after :back)
* :history [<n>]              after :trace, show the execution history
* :list                       show the source code around current breakpoint
* :list <identifier>          show the source code for <identifier>
* :list [<module>] <line>     show the source code around line number <line>
* :print [<name> ...]         show a value without forcing its computation
* :sprint [<name> ...]        simplified version of :print
* :step                       single-step after stopping at a breakpoint
* :step <expr>                single-step into <expr>
* :steplocal                  single-step within the current top-level binding
* :stepmodule                 single-step restricted to the current module
* :trace                      trace after stopping at a breakpoint
* :trace <expr>               evaluate <expr> with tracing on (see :history)

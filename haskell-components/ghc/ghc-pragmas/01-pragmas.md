# Pragmas

GHC supports several pragmas, or instructions to the compiler placed in the source code. In most cases, pragmas don't affect the meaning of the program, but they might affect the efficiency of the generated code.

Pragmas all take the form `{-# word ... #-}` where `⟨word⟩` indicates the type of pragma, and is followed optionally by information specific to  that type of pragma.

- `⟨word⟩` is case insensitive
- unrecognised `⟨word⟩`s are ignored
- the layout rule applies in pragmas, so the closing tag (`#-}`) should start in the column to the right of the opening tag (`{-#`).

Certain pragmas are *file-header pragmas*:
- A file-header pragma must precede the `module` keyword in a file.
- any number of file-header pragmas may be placed in a file
- file-header pragmas may be preceded/followed by comments
- file-header pragmas are READ-ONCE-ONLY
- file-header pragmas are read before pre-processing the file (with CPP)

The file-header pragmas are:
- `{-# LANGUAGE    #-}`
- `{-# OPTIONS_GHC #-}`
- `{-# INCLUDE     #-}`


LANGUAGE or OPTIONS_GHC pragmas may be set
- as flags on the cmdline
- as file-header pragmas in a module
- in `ghci.conf`
- in `project.cabal` file

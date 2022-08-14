# Haskell language server

- https://github.com/haskell/haskell-language-server
- https://haskell-language-server.readthedocs.io/en/latest/

The **haskell-language-server (HLS)** project is an implementation of a language server for the *Language Server Protocol (LSP)*. A language server talks to a client (typically an IDE), which can ask the server to perform various operations, such as reporting errors or providing code completions.

## Language Server Protocol

The advantage of this system is that clients and servers can interoperate more easily so long as they all speak the *LSP protocol*. In the case of HLS, that means that it can be used with many different editors, since editor support for the LSP protocol is now widespread.

## Servers and clients

HLS is responsible for actually understanding your project and answering the questions that the client asks of it (what completion items could go here, are there any errors in the project, and so on). HLS provides many (but not all) of the features that the LSP protocol supports.

However, HLS only provides the server part of the setup. In order to actually use it you also need a client (editor). The client is responsible for managing your interaction with the server: launching it, dispatching commands to it, and displaying or implementing responses.

Common clients include
- VSCode (the reference implementation for a LSP client)
- Emacs, with the lsp-mode + lsp-haskell, or eglot packages
- vim or neovim, with the builtin LSP support or coc.vim
- kate editor

## LSP terminology

### Code action
A code action is a specific action triggered by a user on a particular region of code. For example, filling in the type signature for a function.

### Code lens
A pre-rendered edit or action shown in the body of the document itself, usually triggered with a click. For example, filling the type signature with a click.

### Completion item
An item that can be inserted into the text, including its metadata.

### Diagnostic
Any information about the project that is shown in the editor, including errors, warnings, and hints from tools such as linters.

### Semantic highlighting
Special service performed by the server to semantically highlight the syntax. Language elements are highlighted depending on their semantic, not syntactic, role. This includes highlighting the same identifier with a consistent color.

### Method
A LSP method is a function in the LSP protocol that the client can invoke to perform some action (e.g. ask for completions at a point).

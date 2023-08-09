ErlLS
=====

[![erlls](https://img.shields.io/crates/v/erlls.svg)](https://crates.io/crates/erlls)
[![vscode version](https://img.shields.io/vscode-marketplace/v/sile.erlls.svg?label=vscode)](https://marketplace.visualstudio.com/items?itemName=sile.erlls)
[![Actions Status](https://github.com/sile/erlls/workflows/CI/badge.svg)](https://github.com/sile/erlls/actions)
![License](https://img.shields.io/crates/l/erlls)

Erlang language server.

Supported LSP features
----------------------

- [ ] [textDocument/definition](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_definition)
  - [x] Module
  - [x] Function
  - [x] Type
  - [x] Record
  - [x] Macro
  - [ ] Variable
  - [x] Include file
- [x] [textDocument/formatting](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_formatting)
  - [x] Formatting documents by using [efmt](https://bithub.com/sile/efmt)
- [ ] [textDocument/completion](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion)
  - [x] Fully qualified function name
  - [x] Fully qualified type name
  - [ ] Record name
  - [ ] Record field name
- [ ] [textDocument/publishDiagnostics](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics) (when received `textDocument/{didOpen,didSave}`)
  - [x] Parse errors by [efmt](https://github.com/sile/efmt)
  - [ ] Lint checks
- [ ] [textDocument/rename](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_rename)
- [x] [textDocument/semanticTokens/full](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens)
- [x] [textDocument/semanticTokens/range](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_semanticTokens)

Editor integrations
-------------------

ErlLS can be used with any [LSP](https://microsoft.github.io/language-server-protocol/) clients. 
Here are a few examples.

### Visual Studio Code

Please install [erlls extension](https://marketplace.visualstudio.com/items?itemName=sile.erlls).

There is no need to install the `erlls` binary using the `$ cargo install` command as the extension already includes the WebAssembly build.

### Emacs ([lsp-mode](https://github.com/emacs-lsp/lsp-mode))

1. Install `erlls` command.

```console
$ cargo install erlls
```

2. Add the following code to your `.emacs` file.

```emacs
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(erlang-mode . "erlang")))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "erlls")
                  :activation-fn (lsp-activate-on "erlang")
                  :priority -1
                  :server-id 'erlls))
```

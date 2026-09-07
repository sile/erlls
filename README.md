ErlLS
=====

[![erlls](https://img.shields.io/crates/v/erlls.svg)](https://crates.io/crates/erlls)
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
  - [x] Formatting documents by using [efmt](https://github.com/sile/efmt)
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
- [ ] [textDocument/documentHighlight](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentHighlight)
  - [ ] Variable
- [x] [textDocument/hover](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_hover)
  - NOTE: Only the `-doc` and `-moduledoc` attributes are supported (i.e., doc comments are not supported)

Editor integrations
-------------------

ErlLS can be used with any [LSP](https://microsoft.github.io/language-server-protocol/) clients.
Here are a few examples.

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

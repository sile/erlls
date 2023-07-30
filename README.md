ErlLS
=====

[![erlls](https://img.shields.io/crates/v/erlls.svg)](https://crates.io/crates/erlls)
[![vscode version](https://img.shields.io/vscode-marketplace/v/sile.erlls.svg?label=vscode)](https://marketplace.visualstudio.com/items?itemName=sile.erlls)
[![Documentation](https://docs.rs/erlls/badge.svg)](https://docs.rs/erlls)
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
  - [x] Formatting by using [efmt](https://bithub.com/sile/efmt)
- [ ] [textDocument/publishDiagnostics](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_publishDiagnostics)



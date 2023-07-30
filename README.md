ErlLS
=====

[![erlls](https://img.shields.io/crates/v/erlls.svg)](https://crates.io/crates/erlls)
[![vscode version](https://img.shields.io/vscode-marketplace/v/sile.erlls.svg?label=vscode)](https://marketplace.visualstudio.com/items?itemName=sile.erlls)
[![Documentation](https://docs.rs/erlls/badge.svg)](https://docs.rs/erlls)
[![Actions Status](https://github.com/sile/erlls/workflows/CI/badge.svg)](https://github.com/sile/erlls/actions)
![License](https://img.shields.io/crates/l/erlls)

- https://microsoft.github.io/language-server-protocol/

Features List
-------------

- General features:
  - [x] Error tolerant parser
  - [x] `-include()`
- LSP features:
  - [ ] textDocument/definition
    - [x] Module
    - [x] Function
    - [x] Type
    - [x] Record
    - [x] Macro
    - [ ] Variable
    - [x] Include file
  - [x] textDocument/formatting
    - [x] Formatting by using `efmt`
  - [ ] textDocument/publishDiagnostics
    - [x] Formatting error

# Change Log

All notable changes to the "erlls" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [Unreleased]

### Fixed

- Change the type of DidCloseTextDocumentParams.text_document from TextDocumentItem to TextDocumentIdentifier

## [0.0.15] - 2025-02-15

### Added

- Add support for OTP 28.0-rc1

## [0.0.19] - 2024-02-23

### Added

- Add support for OTP 27.0-rc1 (triple-quoted strings and sigil string literals)

## [0.0.12] - 2023-08-11

### Fixed

- Add "_checkouts/" to lib directry list

## [0.0.11] - 2023-08-09

### Added

- Add push diagnostics by `efmt`

## [0.0.10] - 2023-08-07

### Fixed

- Fix a bug that the first finding of a module that outside the workspace will fail.

## [0.0.7] - 2023-08-06

### Changed

- Tweak semantic highlighting rules

## [0.0.5] - 2023-08-06

### Added

- Add semantic highlighting support
- Make it possible to go to the definition when the cursor is on the `/Arity` part

## [0.0.4] - 2023-08-02

### Added

- Add a cache when resolving module URIs

## [0.0.3] - 2023-08-02

### Added

- Add basic completion support for fully qualified function and type names

## [0.0.1] - 2023-07-30

- Initial release

# Change Log

All notable changes to the "erlls" extension will be documented in this file.

Check [Keep a Changelog](http://keepachangelog.com/) for recommendations on how to structure this file.

## [Unreleased]

## [0.0.18] - 2023-10-22

### Added

- Recognize *.app.src as Erlang files

## [0.0.17] - 2023-09-30

### Fixed

- Don't apply `"action": { "indent": "outdent" }` if the trailing `.` is commented out

## [0.0.16] - 2023-08-20

### Fixed

- Include erlls.wasm in the extension package

## [0.0.15] - 2023-08-20

### Fixed

- Update erlls.wasm as v0.0.14 included an old binary

## [0.0.14] - 2023-08-20

### Added

- Support VSCode Web
- Add `erlls.erlLibs` setting

### Changed

- Remove `erlls.enableCompletion` setting

## [0.0.13] - 2023-08-15

### Fixed

- Fix a bug that tag jumps to dependencies are failed if there is no `_checkouts/` directory

## [0.0.12] - 2023-08-11

### Fixed

- Add "_checkouts/" to lib directry list

## [0.0.11] - 2023-08-09

### Added

- Add push diagnostics by `efmt`

## [0.0.10] - 2023-08-07

### Fixed

- Fix a bug that the first finding of a module that outside the workspace will fail.

## [0.0.9] - 2023-08-06

### Added

- Add minimal indentation rules

## [0.0.8] - 2023-08-06

### Added

- Add `language-configuration.json` file to make the editor recognize comments and brackets

## [0.0.7] - 2023-08-06

### Changed

- Tweak semantic highlighting rules

### Added

- Recognize .hrl files as Erlang files

## [0.0.6] - 2023-08-06

### Fixed

- Make the erlls.wasm up to date

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
- Add `erlls.enableCompletion` setting

## [0.0.2] - 2023-07-30

### Fixed

- Make "vscode-languageclient" and "vscode-languageserver" to "dependencies" (package.json)
- Update erlls.wasm to the latest version

## [0.0.1] - 2023-07-30

- Initial release

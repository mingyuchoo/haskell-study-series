# Haskell LSP Extension

A Visual Studio Code extension that provides Language Server Protocol (LSP) support for Haskell development.

## Features

- **Syntax Highlighting**: Enhanced syntax highlighting for Haskell files
- **Error Diagnostics**: Real-time syntax and type error reporting
- **Code Completion**: Intelligent code completion with type signatures
- **Hover Information**: Type information and documentation on hover
- **Go to Definition**: Navigate to symbol definitions
- **Document Symbols**: Outline view of Haskell modules
- **Configuration Management**: Hot-reload configuration changes

## Requirements

- Visual Studio Code 1.74.0 or higher
- Haskell LSP Server executable (must be installed separately)

## Installation

1. Install the Haskell LSP server executable
2. Install this extension from the VSCode marketplace
3. Configure the server path in extension settings if needed

## Configuration

The extension can be configured through VSCode settings:

### `haskellLsp.serverPath`
- **Type**: `string`
- **Default**: `"haskell-lsp-server"`
- **Description**: Path to the Haskell LSP server executable

### `haskellLsp.logLevel`
- **Type**: `string`
- **Default**: `"info"`
- **Options**: `"debug"`, `"info"`, `"warning"`, `"error"`
- **Description**: Log level for the LSP server

### `haskellLsp.maxRestartCount`
- **Type**: `number`
- **Default**: `3`
- **Description**: Maximum number of times to restart the server on crash

### `haskellLsp.enableVerboseLogging`
- **Type**: `boolean`
- **Default**: `false`
- **Description**: Enable verbose logging output

## Commands

- **Haskell LSP: Restart Server** (`haskellLsp.restart`): Manually restart the LSP server

## Troubleshooting

### Server Not Found Error

If you see an error about the server executable not being found:

1. Ensure the Haskell LSP server is installed and available in your PATH
2. Configure the correct path in `haskellLsp.serverPath` setting
3. Use the "Browse for Executable" option in the error dialog

### Server Crashes

The extension automatically attempts to restart crashed servers up to the configured limit. If the server continues to crash:

1. Check the VSCode output panel for error logs
2. Verify your Haskell project configuration
3. Try restarting VSCode
4. Report issues with detailed logs

## Development

### Building the Extension

To build and test the extension:

```bash
cd vscode-extension
npm install
npm run compile
npm run lint
```

### Packaging

To create a distributable `.vsix` package:

```bash
npm run package
```

Or build everything (server + extension):

```bash
# From project root
make build-all
```

### Installation from Source

```bash
# Build and install locally
make install-extension
```

## License

This extension is licensed under the same terms as the main Haskell LSP project.
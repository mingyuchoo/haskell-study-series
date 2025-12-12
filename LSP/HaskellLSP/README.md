# Haskell LSP Extension

A comprehensive Language Server Protocol (LSP) implementation for Haskell, consisting of:

1. **Haskell LSP Server** - A language server written in Haskell using the `lsp` library
2. **VSCode Extension** - A TypeScript-based VSCode extension that manages the server lifecycle

## Features

### Language Server Features
- **Document Synchronization**: Real-time document tracking with incremental updates
- **Diagnostics**: Syntax and type error reporting
- **Code Completion**: Intelligent completion with type signatures
  - Function completion with type information
  - Module-qualified completion (e.g., `Data.List.`)
  - Context-aware suggestions
- **Hover Information**: Type information and documentation
  - Function type signatures
  - Type definitions and kinds
  - Operator fixity information
- **Go to Definition**: Navigate to symbol definitions
- **Document Symbols**: Outline view of declarations
- **Configuration Management**: Hot-reload configuration changes

### VSCode Extension Features
- **Server Lifecycle Management**: Automatic start/stop/restart
- **Crash Recovery**: Automatic restart with exponential backoff
- **Configuration Synchronization**: Real-time settings updates
- **Error Handling**: User-friendly error messages and recovery options

## Installation

### Quick Start

1. **Build everything**:
   ```bash
   make build-all
   ```

2. **Install the VSCode extension**:
   ```bash
   make install-extension
   ```

3. **Open a Haskell project in VSCode** and the extension will automatically start the LSP server.

### Manual Installation

#### Building the LSP Server

```bash
# Build the Haskell LSP server
stack build

# Run tests
stack test

# Install locally
stack install
```

#### Building the VSCode Extension

```bash
# Navigate to extension directory
cd vscode-extension

# Install dependencies
npm install

# Compile TypeScript
npm run compile

# Package the extension
npm run package

# Install in VSCode
code --install-extension *.vsix
```

## Usage

### Running the LSP Server

The LSP server can be run standalone for testing or integration with other editors:

```bash
# Run the server (communicates via stdin/stdout)
stack run

# Or run the installed executable
haskell-lsp-server

# With logging
haskell-lsp-server --log-level debug
```

### VSCode Extension

Once installed, the extension automatically:

1. **Activates** when you open a Haskell file (`.hs` or `.lhs`)
2. **Starts** the LSP server process
3. **Provides** language features like completion, hover, diagnostics
4. **Manages** server lifecycle (restart on crash, shutdown on deactivate)

### Configuration

Configure the extension through VSCode settings:

```json
{
  "haskellLsp.serverPath": "haskell-lsp-server",
  "haskellLsp.logLevel": "info",
  "haskellLsp.maxRestartCount": 3,
  "haskellLsp.enableVerboseLogging": false
}
```

#### Configuration Options

- **`haskellLsp.serverPath`**: Path to the LSP server executable
- **`haskellLsp.logLevel`**: Server log level (`debug`, `info`, `warning`, `error`)
- **`haskellLsp.maxRestartCount`**: Maximum automatic restart attempts
- **`haskellLsp.enableVerboseLogging`**: Enable detailed logging

## Development

### Project Structure

```
├── src/                    # Haskell LSP server source
│   ├── LSP/               # Core LSP functionality
│   ├── Handlers/          # Request/notification handlers
│   └── Analysis/          # Code analysis and parsing
├── app/                   # Server executable entry point
├── test/                  # Test suite
├── vscode-extension/      # VSCode extension
│   ├── src/              # TypeScript source
│   └── package.json      # Extension manifest
└── docker/               # Docker configuration
```

### Building and Testing

#### Haskell Server

```bash
# Fast build during development
stack build --fast

# Build with optimizations
make build

# Run tests
stack test

# Watch mode for tests
make watch-test

# Interactive development
make ghcid

# Format code
make format
```

#### VSCode Extension

```bash
cd vscode-extension

# Install dependencies
npm install

# Compile TypeScript
npm run compile

# Watch mode
npm run watch

# Run linter
npm run lint

# Package extension
npm run package
```

### Testing the Integration

1. **Build both components**:
   ```bash
   make build-all
   ```

2. **Install the extension**:
   ```bash
   make install-extension
   ```

3. **Open a Haskell project** in VSCode

4. **Test features**:
   - Type some Haskell code and check for syntax highlighting
   - Hover over functions to see type information
   - Use Ctrl+Space for code completion
   - Use F12 for go-to-definition

### Debugging

#### Server Debugging

```bash
# Run server with debug logging
stack run -- --log-level debug

# Check server output in VSCode
# View > Output > Select "Haskell LSP"
```

#### Extension Debugging

1. Open the extension source in VSCode
2. Press F5 to launch Extension Development Host
3. Open a Haskell project in the new window
4. Check Debug Console for extension logs

## Architecture

### LSP Server (Haskell)

- **LSP.Server**: Main server entry point and handler registration
- **LSP.Types**: Core data types and configuration
- **Handlers.***: Individual LSP request/notification handlers
- **Analysis.Parser**: Haskell code parsing and analysis
- **LSP.Diagnostics**: Error detection and reporting

### VSCode Extension (TypeScript)

- **extension.ts**: Main extension entry point
- **config.ts**: Configuration management
- Language client setup and server lifecycle management

### Communication Flow

```
VSCode Extension ←→ Language Client ←→ LSP Server
     (TypeScript)      (JSON-RPC)      (Haskell)
```

## Troubleshooting

### Common Issues

#### "Server executable not found"

1. Ensure the server is built: `stack build`
2. Check the server path in settings
3. Try using an absolute path
4. Verify executable permissions

#### Server crashes repeatedly

1. Check VSCode Output panel (View > Output > "Haskell LSP")
2. Enable verbose logging in settings
3. Verify your Haskell project is valid
4. Try restarting VSCode

#### No language features working

1. Ensure the extension is activated (check status bar)
2. Verify file is recognized as Haskell (`.hs` or `.lhs`)
3. Check for conflicting Haskell extensions
4. Restart the LSP server: Ctrl+Shift+P > "Haskell LSP: Restart Server"

### Getting Help

1. Check the [troubleshooting guide](vscode-extension/README.md#troubleshooting)
2. Enable verbose logging and check output panels
3. Report issues with detailed logs and reproduction steps

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests for new functionality
5. Ensure all tests pass: `make test`
6. Submit a pull request

### Development Guidelines

- Follow existing code style (use `make format` for Haskell)
- Add tests for new features
- Update documentation as needed
- Test both server and extension components

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Built using the [lsp](https://hackage.haskell.org/package/lsp) Haskell library
- VSCode extension based on [vscode-languageclient](https://www.npmjs.com/package/vscode-languageclient)
- Inspired by other LSP implementations in the Haskell ecosystem

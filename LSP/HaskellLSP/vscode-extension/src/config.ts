import * as vscode from 'vscode';

export interface ExtensionConfig {
    serverPath: string;
    logLevel: 'debug' | 'info' | 'warning' | 'error';
    maxRestartCount: number;
    enableVerboseLogging: boolean;
}

export function getExtensionConfig(): ExtensionConfig {
    const config = vscode.workspace.getConfiguration('haskellLsp');
    
    return {
        serverPath: config.get<string>('serverPath', 'haskell-lsp-server'),
        logLevel: config.get<'debug' | 'info' | 'warning' | 'error'>('logLevel', 'info'),
        maxRestartCount: config.get<number>('maxRestartCount', 3),
        enableVerboseLogging: config.get<boolean>('enableVerboseLogging', false)
    };
}

export function validateConfig(config: ExtensionConfig): string[] {
    const errors: string[] = [];
    
    if (!config.serverPath || config.serverPath.trim() === '') {
        errors.push('Server path cannot be empty');
    }
    
    if (!['debug', 'info', 'warning', 'error'].includes(config.logLevel)) {
        errors.push('Invalid log level. Must be one of: debug, info, warning, error');
    }
    
    if (config.maxRestartCount < 0 || config.maxRestartCount > 10) {
        errors.push('Max restart count must be between 0 and 10');
    }
    
    return errors;
}

export function getServerInitializationOptions(config: ExtensionConfig): object {
    return {
        logLevel: config.logLevel,
        enableVerboseLogging: config.enableVerboseLogging,
        extensionVersion: vscode.extensions.getExtension('haskell-lsp.haskell-lsp-extension')?.packageJSON?.version || '0.1.0'
    };
}
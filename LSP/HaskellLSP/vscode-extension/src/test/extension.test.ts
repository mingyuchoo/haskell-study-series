import * as assert from 'assert';
import * as vscode from 'vscode';
import { getExtensionConfig, validateConfig } from '../config';

suite('Extension Test Suite', () => {
    vscode.window.showInformationMessage('Start all tests.');

    test('Configuration validation', () => {
        const validConfig = {
            serverPath: 'haskell-lsp-server',
            logLevel: 'info' as const,
            maxRestartCount: 3,
            enableVerboseLogging: false
        };

        const errors = validateConfig(validConfig);
        assert.strictEqual(errors.length, 0, 'Valid configuration should have no errors');
    });

    test('Invalid configuration detection', () => {
        const invalidConfig = {
            serverPath: '',
            logLevel: 'invalid' as any,
            maxRestartCount: -1,
            enableVerboseLogging: false
        };

        const errors = validateConfig(invalidConfig);
        assert.ok(errors.length > 0, 'Invalid configuration should have errors');
    });

    test('Extension configuration loading', () => {
        const config = getExtensionConfig();
        assert.ok(config.serverPath, 'Server path should be defined');
        assert.ok(['debug', 'info', 'warning', 'error'].includes(config.logLevel), 'Log level should be valid');
        assert.ok(config.maxRestartCount >= 0, 'Max restart count should be non-negative');
    });
});
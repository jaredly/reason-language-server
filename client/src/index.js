/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
const vscode = require("vscode");
const {LanguageClient} = require("vscode-languageclient");
const path = require('path')
const fs = require('fs')
const DEV = true;

function activate(context) {
    // The server is implemented in reason
    let binaryLocation = DEV
    ? context.asAbsolutePath(path.join('..', 'lib', 'bs', 'native', 'bin.native'))
    : context.asAbsolutePath(path.join('node_modules', '@jaredly', 'reason-language-server', 'lib', 'bs', 'native', 'bin.native'));

    let serverOptions = {
        command: binaryLocation,
        args: [],
    };
    let clientOptions = {
        documentSelector: [{scheme: 'file', language: 'reason'}],
        synchronize: {
            // Synchronize the setting section 'reason_language_server' to the server
            configurationSection: 'reason_language_server',
            // Notify the server about file changes to '.clientrc files contain in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/bsconfig.json')
        }
    };
    let client = new LanguageClient(
        'reason-language-server',
        'Reason Language Server',
        serverOptions,
        clientOptions
    )
    // Push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(client.start());

    let lastStartTime = Date.now();
    const restart = () => {
        client.stop();
        client = new LanguageClient(
            'reason-language-server',
            'Reason Language Server',
            serverOptions,
            clientOptions
        );
        lastStartTime = Date.now()
        context.subscriptions.push(client.start());
        vscode.window.showInformationMessage('Reason language server restarted');
    }

    if (DEV) {
        vscode.window.showInformationMessage('DEBUG MODE: Will auto-restart the reason language server if it recompiles');
        const checkForBinaryChagne = setInterval(() => {
            try {
                const stat = fs.statSync(binaryLocation)
                const mtime = stat.mtime.getTime()
                if (mtime > lastStartTime) {
                    restart()
                }
            } catch (e) {
                console.warn('Failed to check binary mtime ' + e.message)
            }
        }, 500);
        context.subscriptions.push({dispose: () => clearInterval(checkForBinaryChange)})
    }

    vscode.commands.registerCommand('reason-language-server.restart', restart);
}
exports.activate = activate;
/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
const vscode = require("vscode");
const {LanguageClient} = require("vscode-languageclient");
const path = require('path')
const fs = require('fs')
// const DEV = false;

const getLocation = () => {
    let config = vscode.workspace.getConfiguration('reason_language_server')

    let binaryLocation = config.get('location')

    if (!binaryLocation) {
        binaryLocation = path.join(vscode.workspace.rootPath, 'node_modules', '@jaredly', 'reason-language-server', 'lib', 'bs', 'native', 'bin.native')
        // binaryLocation = path.join(vscode.workspace.rootPath, '..', 'lib', 'bs', 'native', 'bin.native')
    } else {
        if (!binaryLocation.startsWith('/')) {
            binaryLocation = path.join(vscode.workspace.rootPath, binaryLocation)
        }
    }
    return binaryLocation
}

const newClient = (clientOptions, location) => {
    return new LanguageClient(
        'reason-language-server',
        'Reason Language Server',
        serverOptions,
        clientOptions
    )
}

const shouldReload = () => vscode.workspace.getConfiguration('reason_language_server').get('reloadOnChange')

function activate(context) {
    // The server is implemented in reason

    let clientOptions = {
        documentSelector: [{scheme: 'file', language: 'reason'}],
        synchronize: {
            // Synchronize the setting section 'reason_language_server' to the server
            configurationSection: 'reason_language_server',
            // Notify the server about file changes to '.clientrc files contain in the workspace
            fileEvents: vscode.workspace.createFileSystemWatcher('**/bsconfig.json')
        }
    };

    let client = null
    let lastStartTime = null
    let interval = null

    const startChecking = (location) => {
        vscode.window.showInformationMessage('DEBUG MODE: Will auto-restart the reason language server if it recompiles');
        interval = setInterval(() => {
            try {
                const stat = fs.statSync(location)
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

    const restart = () => {
        if (client) {
            client.stop();
        }
        const location = getLocation()
        client = new LanguageClient(
            'reason-language-server',
            'Reason Language Server',
            {
                command: location,
                args: [],
            },
            clientOptions
        );
        lastStartTime = Date.now()
        context.subscriptions.push(client.start());
        vscode.window.showInformationMessage('Reason language server restarted');
        if (shouldReload()) {
            if (!interval) {
                startChecking(location)
            }
        } else if (interval) {
            vscode.window.showInformationMessage('DEBUG MODE OFF - no longer monitoring reason-language-server binary');
            clearInterval(interval)
            interval = null
        }
    }

    restart();

    vscode.commands.registerCommand('reason-language-server.restart', restart);
}
exports.activate = activate;

/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
const vscode = require("vscode");
const {LanguageClient} = require("vscode-languageclient");
const path = require('path')
const fs = require('fs')

const getLocation = (context) => {
    let binaryLocation = vscode.workspace.getConfiguration('reason_language_server').get('location')

    if (!binaryLocation) {
        // see if it's bundled with the extension
        // hmm actually I could bundle one for each platform & probably be fine
        // I guess it's 9mb binary. I wonder if I can cut that down?
        binaryLocation = path.join(vscode.workspace.rootPath, 'node_modules', '@jaredly', 'reason-language-server', 'lib', 'bs', 'native', 'bin.native')
        if (!fs.existsSync(binaryLocation)) {
            binaryLocation = context.asAbsolutePath('./bin.native')
            if (!fs.existsSync(binaryLocation)) {
                vscode.window.showErrorMessage('Reason Language Server not found! Please install the npm package @jaredly/reason-language-server to enable IDE functionality');
                return null
            }
        }
    } else {
        if (!binaryLocation.startsWith('/')) {
            binaryLocation = path.join(vscode.workspace.rootPath, binaryLocation)
        }
        if (!fs.existsSync(binaryLocation)) {
            vscode.window.showErrorMessage('Reason Language Server not found! You specified ' + binaryLocation);
            return null
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

    /**
     * client.sendRequest(...).then(response => {
     * })
     *
     * client.sendNotification()....
     * that could be a way to implement the code actions that I want to do.
     *
     * alsooo I think I want
     */

    const restart = () => {
        if (client) {
            client.stop();
        }
        const location = getLocation(context)
        if (!location) return
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

    vscode.workspace.onDidChangeConfiguration(evt => {
        if (evt.affectsConfiguration('reason_language_server.location')) {
            restart()
        }
    })

    restart();

    vscode.commands.registerCommand('reason-language-server.restart', restart);
}
exports.activate = activate;

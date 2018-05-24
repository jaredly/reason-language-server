/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
const vscode = require("vscode");
const {LanguageClient} = require("vscode-languageclient");
function activate(context) {
    // The server is implemented in reason
    let binaryLocation = '/Users/jared/clone/tools/language-server/lib/bs/native/bin.native';
    // Todo have it be a child npm module, and access it from there
    // context.asAbsolutePath(path.join('server', 'server.js'));
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
    let disposable = new LanguageClient(
        'reason-language-server',
        'Reason Language Server',
        serverOptions,
        clientOptions
    ).start();
    // Push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
exports.activate = activate;
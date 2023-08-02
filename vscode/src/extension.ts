import * as path from 'path';
import * as vscode from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration("erlls");
    const enableCompletion = config.get<boolean>("enableCompletion");
    const configJson = JSON.stringify({ enableCompletion });

    const serverModule = context.asAbsolutePath(
        path.join('out', 'server.js')
    );
    const wasmPath = context.asAbsolutePath(
        path.join('out', 'erlls.wasm')
    );

    const serverOptions: ServerOptions = {
        run: {
            module: serverModule,
            args: [wasmPath, configJson],
            transport: TransportKind.ipc
        },
        debug: {
            module: serverModule,
            args: [wasmPath, configJson],
            transport: TransportKind.ipc,
        }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'erlang' }]
    };

    client = new LanguageClient(
        'erlls',
        'Erlang Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate() {
    if (!client) {
        return undefined;
    }
    return client.stop();
}

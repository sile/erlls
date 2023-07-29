import * as path from 'path';
import { ExtensionContext } from 'vscode';

import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
    TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    const serverModule = context.asAbsolutePath(
        path.join('out', 'server.js')
    );

    const serverOptions: ServerOptions = {
        run: { module: serverModule, transport: TransportKind.ipc },
        debug: {
            module: serverModule,
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

import * as vscode from 'vscode';
import { LanguageClientOptions } from 'vscode-languageclient';
import { LanguageClient } from 'vscode-languageclient/browser';

export async function activate(context: vscode.ExtensionContext) {
    console.log('ErlLS is activated.');
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'erlang' }]
    };
    const client = await createWorkerLanguageClient(context, clientOptions);
    await client.start();
    console.log('ErlLS server is ready.');
}

async function createWorkerLanguageClient(context: vscode.ExtensionContext, clientOptions: LanguageClientOptions): Promise<LanguageClient> {
    // const uri = vscode.Uri.file('/etc/hosts');
    // console.log('uri: ' + uri);
    // const data = await vscode.workspace.fs.readFile(uri);
    // console.log('data: ' + data);

    const serverMain = vscode.Uri.joinPath(context.extensionUri, 'dist/web/server.js');
    const worker = new Worker(serverMain.toString(true));

    const channel = new MessageChannel();
    const wasmUri = vscode.Uri.joinPath(context.extensionUri, 'dist/web/erlls.wasm');
    const wasmBytes = await (await fetch(wasmUri.toString(true))).arrayBuffer();
    worker.postMessage(
        { 'type': 'initialize', wasmBytes, 'port': channel.port2 },
        [wasmBytes, channel.port2]
    );

    const port = channel.port1;
    port.onmessage = (_msg) => {
    };

    return new LanguageClient('erllsweb', 'ErlLS Web', clientOptions, worker);
}

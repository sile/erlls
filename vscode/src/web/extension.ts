import * as vscode from 'vscode';
import { LanguageClientOptions } from 'vscode-languageclient';
import { LanguageClient } from 'vscode-languageclient/browser';

let client: LanguageClient;

export async function activate(context: vscode.ExtensionContext) {
    console.log('ErlLS is activated.');
    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ language: 'erlang' }]
    };
    client = await createLanguageClient(context, clientOptions);
    await client.start();
    console.log('ErlLS server is ready.');
}

export function deactivate() {
    if (!client) {
        return undefined;
    }
    console.log('ErlLS is deactivated.');
    return client.stop();
}

async function createLanguageClient(context: vscode.ExtensionContext, clientOptions: LanguageClientOptions): Promise<LanguageClient> {
    const wasmUri = vscode.Uri.joinPath(context.extensionUri, 'dist/web/erlls.wasm');
    const wasmBytes = await vscode.workspace.fs.readFile(wasmUri);

    const serverScriptUri = vscode.Uri.joinPath(context.extensionUri, 'dist/web/server.js');
    const serverScript = new TextDecoder().decode(await vscode.workspace.fs.readFile(serverScriptUri));
    const workerUrl = URL.createObjectURL(new Blob([serverScript], { type: 'application/javascript' }));
    const worker = new Worker(workerUrl);

    const config = vscode.workspace.getConfiguration("erlls");
    const erlLibsString = config.get<string>("erlLibs", "");
    let erlLibs: string[] = [];
    if (erlLibsString !== "") {
        erlLibs = erlLibsString.split(":");
    }

    const channel = new MessageChannel();
    worker.postMessage(
        { type: 'initialize', wasmBytes, port: channel.port2, erlLibs },
        [wasmBytes.buffer, channel.port2]);

    const port = channel.port1;
    return new Promise((resolve, _reject) => {
        port.onmessage = (msg: PortMessage) => {
            switch (msg.data.type) {
                case 'initialized':
                    resolve(new LanguageClient('erlls', 'ErlLS', clientOptions, worker));
                    break
                default:
                    handlePortMessage(port, msg);
            }
        };
    });
}

type PortMessage = {
    data:
    { type: 'initialized' } |
    { type: 'fsExists.call', promiseId: number, uri: string } |
    { type: 'fsReadFile.call', promiseId: number, uri: string } |
    { type: 'fsReadSubDirs.call', promiseId: number, uri: string }
};

function handlePortMessage(port: MessagePort, msg: PortMessage) {
    switch (msg.data.type) {
        case 'fsExists.call':
            {
                const promiseId = msg.data.promiseId;
                const uri = vscode.Uri.parse(msg.data.uri);
                vscode.workspace.fs.stat(uri).then(
                    (_stat) => {
                        port.postMessage({ type: 'fsExists.reply', promiseId, result: true })
                    },
                    (_reason) => {
                        port.postMessage({ type: 'fsExists.reply', promiseId, result: false })
                    }
                );
            }
            break;
        case 'fsReadFile.call':
            {
                const promiseId = msg.data.promiseId;
                const uri = vscode.Uri.parse(msg.data.uri);
                vscode.workspace.fs.readFile(uri).then(
                    (content) => {
                        port.postMessage({ type: 'fsReadFile.reply', promiseId, content }, [content.buffer]);
                    },
                    () => port.postMessage({ type: 'fsReadFile.reply', promiseId, content: null }),
                );
            }
            break;
        case 'fsReadSubDirs.call':
            {
                const promiseId = msg.data.promiseId;
                const parentDirUri = vscode.Uri.parse(msg.data.uri);
                vscode.workspace.fs.readDirectory(parentDirUri).then(
                    (entries) => {
                        const dirs = [];
                        for (const [name, type] of entries) {
                            if (type === vscode.FileType.Directory || type === vscode.FileType.SymbolicLink) {
                                const dir = vscode.Uri.joinPath(parentDirUri, name).toString();
                                dirs.push(dir);
                            }
                        }
                        port.postMessage({ type: 'fsReadSubDirs.reply', promiseId, dirs });
                    },
                    () => port.postMessage({ type: 'fsReadSubDirs.reply', promiseId, dirs: [] }),
                );
            }
            break;
        default:
            console.warn('Unknown message: ' + JSON.stringify(msg.data));

    }
}

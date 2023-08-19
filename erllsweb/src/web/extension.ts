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

    const channel = new MessageChannel();
    const wasmUri = vscode.Uri.joinPath(context.extensionUri, 'dist/web/erlls.wasm');
    const wasmBytes = await (await fetch(wasmUri.toString(true),
        // TODO: remove(?)
        { mode: "cors" }
    )).arrayBuffer();
    console.log("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@");

    const serverMain = vscode.Uri.joinPath(context.extensionUri, 'dist/web/server.js');

    const serverCode = new TextDecoder().decode(await vscode.workspace.fs.readFile(serverMain));
    console.log(serverCode.slice(0, 100));
    const webWorkerScriptObjectUrl = URL.createObjectURL(
        new Blob([serverCode], { type: 'application/javascript' }),
    );
    const worker = new Worker(webWorkerScriptObjectUrl);

    // const worker = new Worker(serverMain.toString(true),
    //     // TODO: remove
    //     { credentials: 'omit' });
    console.log("@--------@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@");

    worker.postMessage(
        { 'type': 'initialize', wasmBytes, 'port': channel.port2 },
        [wasmBytes, channel.port2]
    );

    const port = channel.port1;
    return new Promise((resolve, _reject) => {
        port.onmessage = (msg: PortMessage) => {
            console.log('port.onmessage: ' + JSON.stringify(msg.data));
            switch (msg.data.type) {
                case 'initialized':
                    resolve(new LanguageClient('erllsweb', 'ErlLS Web', clientOptions, worker));
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
    { type: 'fsExists.call', promiseId: number, path: string } |
    { type: 'fsReadFile.call', promiseId: number, path: string } |
    { type: 'fsReadSubDirs.call', promiseId: number, path: string }
};

function handlePortMessage(port: MessagePort, msg: PortMessage) {
    console.log('handlePortMessage: ' + JSON.stringify(msg.data));
    switch (msg.data.type) {
        case 'fsExists.call':
            {
                const { promiseId, path } = msg.data;
                vscode.workspace.fs.stat(vscode.Uri.file(path)).then(
                    (stat) => {
                        console.log('path: ' + path);
                        console.log('stat: ' + JSON.stringify(stat));
                        port.postMessage({ type: 'fsExists.reply', promiseId, result: true })
                    },
                    (reason) => {
                        console.log('path: ' + path);
                        console.log('reason: ' + JSON.stringify(reason));
                        port.postMessage({ type: 'fsExists.reply', promiseId, result: false })
                    }
                );
            }
            break;
        case 'fsReadFile.call':
            {
                const { promiseId, path } = msg.data;
                vscode.workspace.fs.readFile(vscode.Uri.file(path)).then(
                    (content) => {
                        port.postMessage({ type: 'fsReadFile.reply', promiseId, content }, [content.buffer]);
                    },
                    () => port.postMessage({ type: 'fsReadFile.reply', promiseId, content: null }),
                );
            }
            break;
        case 'fsReadSubDirs.call':
            {
                const { promiseId, path } = msg.data;
                const parentDirUri = vscode.Uri.file(path);
                vscode.workspace.fs.readDirectory(parentDirUri).then(
                    (entries) => {
                        //const wasmUri = vscode.Uri.joinPath(context.extensionUri, 'dist/web/erlls.wasm');

                        // TODO
                        console.log('path: ' + path);
                        console.log('entries: ' + JSON.stringify(entries));
                        const dirs = [];
                        for (const [name, type] of entries) {
                            if (type === vscode.FileType.Directory || type === vscode.FileType.SymbolicLink) {
                                const dir = vscode.Uri.joinPath(parentDirUri, name).path;
                                dirs.push(dir);
                            }
                        }
                        console.log(dirs);
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

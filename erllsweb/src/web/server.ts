import { createConnection, BrowserMessageReader, BrowserMessageWriter } from 'vscode-languageserver/browser';
import { InitializeParams, InitializeResult, ServerCapabilities } from 'vscode-languageserver';

let wasmMemory: WebAssembly.Memory | undefined;
let wasmExports: WebAssembly.Exports | undefined;
let serverPtr: number = 0;
let poolPtr: number = 0;

type Message = { data: { type: 'initialize', wasmBytes: Uint8Array, port: MessagePort, erlLibs: string[] } };
self.onmessage = async (msg: Message) => {
    if (msg.data.type !== 'initialize') {
        throw new Error('Unexpected message: ' + JSON.stringify(msg.data));
    }

    const port = msg.data.port;
    const messageReader = new BrowserMessageReader(self);
    const messageWriter = new BrowserMessageWriter(self);
    const connection = createConnection(messageReader, messageWriter);

    type PortMessage = {
        data:
        { type: 'fsExists.reply', promiseId: number, result: boolean } |
        { type: 'fsReadFile.reply', promiseId: number, content: Uint8Array | null } |
        { type: 'fsReadSubDirs.reply', promiseId: number, dirs: [string] }
    };
    port.onmessage = (msg: PortMessage) => {
        connection.console.log("port.onmessage: " + JSON.stringify(msg.data));
        if (!wasmExports || !wasmMemory) {
            throw new Error("Unreachable");
        }
        switch (msg.data.type) {
            case 'fsExists.reply':
                (wasmExports.notifyFsExistsAsyncResult as CallableFunction)(
                    serverPtr, msg.data.promiseId, msg.data.result
                );
                connection.console.log("port.onmessage: fsExists.reply");
                break;
            case 'fsReadFile.reply':
                if (msg.data.content === null) {
                    (wasmExports.notifyFsReadFileAsyncResult as CallableFunction)(
                        serverPtr, msg.data.promiseId, 0
                    );
                } else {
                    const content = msg.data.content;
                    connection.console.log('contant(1): ' + content.slice(0, 100));
                    connection.console.log("---here---2: " + content.length);
                    connection.console.log(new TextDecoder().decode(content));
                    const wasmDataPtr = (wasmExports.allocateVec as CallableFunction)(content.length);
                    const wasmDataOffset = (wasmExports.vecOffset as CallableFunction)(wasmDataPtr);
                    new Uint8Array(wasmMemory.buffer, wasmDataOffset, content.length).set(content);
                    (wasmExports.notifyFsReadFileAsyncResult as CallableFunction)(
                        serverPtr, msg.data.promiseId, wasmDataPtr
                    );
                }
                break;
            case 'fsReadSubDirs.reply':
                {
                    const dirsJson = JSON.stringify(msg.data.dirs);
                    connection.console.log("dirsJson: " + dirsJson);
                    const data = new TextEncoder().encode(dirsJson);
                    const wasmDataPtr = (wasmExports.allocateVec as CallableFunction)(data.length);
                    const wasmDataOffset = (wasmExports.vecOffset as CallableFunction)(wasmDataPtr);
                    new Uint8Array(wasmMemory.buffer, wasmDataOffset, data.length).set(data);
                    (wasmExports.notifyFsReadSubDirsAsyncResult as CallableFunction)(
                        serverPtr, msg.data.promiseId, wasmDataPtr
                    );
                }
                break;
        }
    }

    function consoleLog(msgOffset: number, msgLen: number) {
        if (!wasmMemory) {
            return;
        }
        const msg = new TextDecoder('utf-8').decode(
            new Uint8Array(wasmMemory.buffer, msgOffset, msgLen));
        connection.console.log(msg);
    }

    function fsExistsAsync(promiseId: number, uriOffset: number, uriLen: number) {
        connection.console.log("fsExistsAsync");
        if (!wasmMemory || !wasmExports) {
            throw new Error("Unreachable");
        }
        const uri = new TextDecoder('utf-8').decode(new Uint8Array(wasmMemory.buffer, uriOffset, uriLen));
        port.postMessage({ type: 'fsExists.call', promiseId, uri });
        connection.console.log("fsExistsAsync: sent");
    }

    function fsReadFileAsync(promiseId: number, uriOffset: number, uriLen: number) {
        connection.console.log("fsReadFileAsync");
        if (!wasmMemory || !wasmExports) {
            throw new Error("Unreachable");
        }
        const uri = new TextDecoder('utf-8').decode(new Uint8Array(wasmMemory.buffer, uriOffset, uriLen));
        port.postMessage({ type: 'fsReadFile.call', promiseId, uri });
    }

    function fsReadSubDirsAsync(promiseId: number, uriOffset: number, uriLen: number) {
        connection.console.log("fsReadSubDirsAsync");
        if (!wasmMemory || !wasmExports) {
            throw new Error("Unreachable");
        }
        const uri = new TextDecoder('utf-8').decode(new Uint8Array(wasmMemory.buffer, uriOffset, uriLen));
        port.postMessage({ type: 'fsReadSubDirs.call', promiseId, uri });
    }

    // TODO
    const memory = new WebAssembly.Memory({ initial: 100, maximum: 2000 });

    const importOjbect = {
        js: { mem: memory },
        env: {
            consoleLog,
            fsExistsAsync,
            fsReadFileAsync,
            fsReadSubDirsAsync,
        }
    };
    const wasmInstance = (await WebAssembly.instantiate(msg.data.wasmBytes, importOjbect)).instance;
    wasmExports = wasmInstance.exports;
    wasmMemory = wasmExports.memory as WebAssembly.Memory;
    serverPtr = (wasmExports.newServer as CallableFunction)();
    poolPtr = (wasmExports.newLocalPool as CallableFunction)();

    const config = { erlLibs: msg.data.erlLibs };
    const configJsonBytes = new TextEncoder().encode(JSON.stringify(config));
    const wasmConfigPtr =
        (wasmExports.allocateVec as CallableFunction)(configJsonBytes.length);
    const wasmConfigOffset =
        (wasmExports.vecOffset as CallableFunction)(wasmConfigPtr);
    new Uint8Array(wasmMemory.buffer, wasmConfigOffset, configJsonBytes.length).set(configJsonBytes);
    (wasmExports.updateConfig as CallableFunction)(serverPtr, wasmConfigPtr);
    port.postMessage({ type: 'initialized' });

    async function handleIncomingMessage(
        message: object
    ): Promise<any[] | object | undefined> {
        if (!wasmExports || !wasmMemory) {
            throw new Error("unreachable");
        }
        connection.console.log("handleIncomingMessage: " + JSON.stringify(message));
        const messageJsonBytes = new TextEncoder().encode(JSON.stringify(message));

        const wasmMessagePtr =
            (wasmExports.allocateVec as CallableFunction)(messageJsonBytes.length);
        const wasmMessageOffset =
            (wasmExports.vecOffset as CallableFunction)(wasmMessagePtr);
        connection.console.log("handleIncomingMessage(0)");
        new Uint8Array(wasmMemory.buffer, wasmMessageOffset, messageJsonBytes.length).set(messageJsonBytes);
        (wasmExports.handleIncomingMessage as CallableFunction)(poolPtr, serverPtr, wasmMessagePtr);
        connection.console.log("handleIncomingMessage(1)");
        return new Promise((resolve, _reject) => waitOutgoingMessage(resolve));
    }

    async function waitOutgoingMessage(resolve: (result: any[] | object | undefined) => void) {
        connection.console.log("handleIncomingMessage(2)");
        if (!wasmExports || !wasmMemory) {
            throw new Error("Unreachable");
        }

        const ready = (wasmExports.tryRunOne as CallableFunction)(poolPtr);
        if (!ready) {
            setTimeout(() => waitOutgoingMessage(resolve), 100);// TODO
            return;
        }

        connection.console.log("waitOutgoingMessage: ready");
        let resultParams: any[] | object | undefined = undefined;
        while (true) {
            const wasmOutgoingMessagePtr =
                (wasmExports.takeOutgoingMessage as CallableFunction)(serverPtr);
            if (wasmOutgoingMessagePtr === 0) {
                break;
            }

            const wasmOutgoingMessageLen =
                (wasmExports.vecLen as CallableFunction)(wasmOutgoingMessagePtr);
            const wasmOutgoingMessageOffset =
                (wasmExports.vecOffset as CallableFunction)(wasmOutgoingMessagePtr);
            const outgoingMessageJson = new TextDecoder().decode(
                new Uint8Array(wasmMemory.buffer, wasmOutgoingMessageOffset, wasmOutgoingMessageLen));
            const outgoingMessage = JSON.parse(outgoingMessageJson);
            if (outgoingMessage.id !== undefined) {
                if (outgoingMessage.result) {
                    resultParams = outgoingMessage.result;
                } else if (outgoingMessage.error) {
                    resultParams = outgoingMessage.error;
                }
            } else {
                connection.sendNotification(outgoingMessage.method, outgoingMessage.params);
            }
        }

        connection.console.log("waitOutgoingMessage(2): " + JSON.stringify(resultParams));
        resolve(resultParams);
    }

    connection.onInitialize(async (params) => {
        const message = {
            jsonrpc: "2.0",
            id: 0, // dummy value (unused)
            method: "initialize",
            params
        };
        return await handleIncomingMessage(message) as InitializeResult;
    });

    connection.onRequest(async (method, params) => {
        const message = {
            jsonrpc: "2.0",
            id: 0, // dummy value (unused)
            method,
            params
        };
        return await handleIncomingMessage(message);
    });

    connection.onNotification(async (method, params) => {
        const message = {
            jsonrpc: "2.0",
            method,
            params
        };
        await handleIncomingMessage(message);
    });

    // connection.onDidOpenTextDocument(async (params) => {
    //     const message = {
    //         jsonrpc: "2.0",
    //         method: "textDocument/didOpen",
    //         params
    //     };
    //     await handleIncomingMessage(message) as InitializeResult;
    // });


    connection.console.log("ErlLS server started");
    connection.console.log("config: " + JSON.stringify(config));
    connection.listen();
}

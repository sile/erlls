import { Connection, createConnection, BrowserMessageReader, BrowserMessageWriter } from 'vscode-languageserver/browser';
import { InitializeResult } from 'vscode-languageserver';

let globalWasmMemory: WebAssembly.Memory = new WebAssembly.Memory({ initial: 1 }); // dummy value (unused)

type InitializeMessage =
    { data: { type: 'initialize', wasmBytes: Uint8Array, port: MessagePort, erlLibs: string[] } };

self.onmessage = async (msg: InitializeMessage) => {
    if (msg.data.type !== 'initialize') {
        throw new Error('Unexpected message: ' + JSON.stringify(msg.data));
    }

    const messageReader = new BrowserMessageReader(self);
    const messageWriter = new BrowserMessageWriter(self);
    const connection = createConnection(messageReader, messageWriter);

    const port = msg.data.port;
    const importOjbect = createImportObject(port, connection);
    const wasmInstance = (await WebAssembly.instantiate(msg.data.wasmBytes, importOjbect)).instance;
    const wasmExports = wasmInstance.exports;
    const wasmMemory = wasmExports.memory as WebAssembly.Memory;
    globalWasmMemory = wasmMemory;

    const serverPtr = (wasmExports.newServer as CallableFunction)();
    const poolPtr = (wasmExports.newLocalPool as CallableFunction)();
    port.onmessage = (m) => handleFsMessage(m, serverPtr, wasmMemory, wasmExports);

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
        const messageJsonBytes = new TextEncoder().encode(JSON.stringify(message));

        const wasmMessagePtr =
            (wasmExports.allocateVec as CallableFunction)(messageJsonBytes.length);
        const wasmMessageOffset =
            (wasmExports.vecOffset as CallableFunction)(wasmMessagePtr);
        new Uint8Array(wasmMemory.buffer, wasmMessageOffset, messageJsonBytes.length).set(messageJsonBytes);
        (wasmExports.handleIncomingMessage as CallableFunction)(poolPtr, serverPtr, wasmMessagePtr);
        return new Promise((resolve, _reject) => waitOutgoingMessage(resolve));
    }

    async function waitOutgoingMessage(resolve: (result: any[] | object | undefined) => void) {
        const ready = (wasmExports.tryRunOne as CallableFunction)(poolPtr);
        if (!ready) {
            setTimeout(() => waitOutgoingMessage(resolve), 1);
            return;
        }

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

    connection.console.log(`ErlLS server started: config=${JSON.stringify(config)}`);
    connection.listen();
}

type FsMessage = {
    data:
    { type: 'fsExists.reply', promiseId: number, result: boolean } |
    { type: 'fsReadFile.reply', promiseId: number, content: Uint8Array | null } |
    { type: 'fsReadSubDirs.reply', promiseId: number, dirs: [string] }
};

function handleFsMessage(msg: FsMessage, serverPtr: number, memory: WebAssembly.Memory, exports: WebAssembly.Exports) {
    switch (msg.data.type) {
        case 'fsExists.reply':
            (exports.notifyFsExistsAsyncResult as CallableFunction)(serverPtr, msg.data.promiseId, msg.data.result);
            break;
        case 'fsReadFile.reply':
            if (msg.data.content === null) {
                (exports.notifyFsReadFileAsyncResult as CallableFunction)(serverPtr, msg.data.promiseId, 0);
            } else {
                const content = msg.data.content;
                const wasmDataPtr = (exports.allocateVec as CallableFunction)(content.length);
                const wasmDataOffset = (exports.vecOffset as CallableFunction)(wasmDataPtr);
                new Uint8Array(memory.buffer, wasmDataOffset, content.length).set(content);
                (exports.notifyFsReadFileAsyncResult as CallableFunction)(serverPtr, msg.data.promiseId, wasmDataPtr);
            }
            break;
        case 'fsReadSubDirs.reply':
            {
                const dirsJson = JSON.stringify(msg.data.dirs);
                const data = new TextEncoder().encode(dirsJson);
                const wasmDataPtr = (exports.allocateVec as CallableFunction)(data.length);
                const wasmDataOffset = (exports.vecOffset as CallableFunction)(wasmDataPtr);
                new Uint8Array(memory.buffer, wasmDataOffset, data.length).set(data);
                (exports.notifyFsReadSubDirsAsyncResult as CallableFunction)(serverPtr, msg.data.promiseId, wasmDataPtr);
            }
            break;
    }
}

function createImportObject(port: MessagePort, connection: Connection) {
    function consoleLog(msgOffset: number, msgLen: number) {
        const msg = new TextDecoder('utf-8').decode(new Uint8Array(globalWasmMemory.buffer, msgOffset, msgLen));
        connection.console.log(msg);
    }

    function fsExistsAsync(promiseId: number, uriOffset: number, uriLen: number) {
        const uri = new TextDecoder('utf-8').decode(new Uint8Array(globalWasmMemory.buffer, uriOffset, uriLen));
        port.postMessage({ type: 'fsExists.call', promiseId, uri });
    }

    function fsReadFileAsync(promiseId: number, uriOffset: number, uriLen: number) {
        const uri = new TextDecoder('utf-8').decode(new Uint8Array(globalWasmMemory.buffer, uriOffset, uriLen));
        port.postMessage({ type: 'fsReadFile.call', promiseId, uri });
    }

    function fsReadSubDirsAsync(promiseId: number, uriOffset: number, uriLen: number) {
        const uri = new TextDecoder('utf-8').decode(new Uint8Array(globalWasmMemory.buffer, uriOffset, uriLen));
        port.postMessage({ type: 'fsReadSubDirs.call', promiseId, uri });
    }

    return {
        env: {
            consoleLog,
            fsExistsAsync,
            fsReadFileAsync,
            fsReadSubDirsAsync,
        }
    };
}

import { createConnection, BrowserMessageReader, BrowserMessageWriter } from 'vscode-languageserver/browser';
import { InitializeParams, InitializeResult, ServerCapabilities } from 'vscode-languageserver';

// TODO
const erlLibs = ["/usr/local/lib/erlang/lib", "_checkouts", "_build/default/lib"];

let wasmMemory: WebAssembly.Memory | undefined;
let wasmExports: WebAssembly.Exports | undefined;
let serverPtr: number = 0;
let poolPtr: number = 0;

type Message = { data: { type: 'initialize', wasmBytes: ArrayBuffer, port: MessagePort } };
self.onmessage = async (msg: Message) => {
    if (msg.data.type !== 'initialize') {
        throw new Error('Unexpected message: ' + JSON.stringify(msg.data));
    }

    const port = msg.data.port;
    const messageReader = new BrowserMessageReader(self);
    const messageWriter = new BrowserMessageWriter(self);
    const connection = createConnection(messageReader, messageWriter);

    function consoleLog(msgOffset: number, msgLen: number) {
        if (!wasmMemory) {
            return;
        }
        const msg = new TextDecoder('utf-8').decode(
            new Uint8Array(wasmMemory.buffer, msgOffset, msgLen));
        connection.console.log(msg);
    }

    function fsExistsAsync(promiseId: number, pathOffset: number, pathLen: number) {
        if (!wasmMemory || !wasmExports) {
            throw new Error("Unreachable");
        }

        // const path = new TextDecoder('utf-8').decode(
        //     new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
        // const result = fs.existsSync(path);
        // connection.console.log(`fsExistsAsync(${promiseId}, ${path}) = ${result}`);

        const result = false;
        (wasmExports.notifyFsExistsAsyncResult as CallableFunction)(serverPtr, promiseId, result);
    }

    function fsReadFileAsync(promiseId: number, pathOffset: number, pathLen: number) {
        if (!wasmMemory || !wasmExports) {
            throw new Error("Unreachable");
        }
        // const exports = wasmExports;
        // const memory = wasmMemory;
        const notifyResult = wasmExports.notifyFsReadFileAsyncResult as CallableFunction;
        // const path = new TextDecoder('utf-8').decode(
        //     new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
        // fs.readFile(
        //     path,
        //     (err, data) => {
        //         connection.console.log(`fsReadFileAsync(${promiseId}, ${path}) = ${err}`);
        //         if (err) {
        //             notifyResult(serverPtr, promiseId, 0);
        //         } else {
        //             const wasmDataPtr = (exports.allocateVec as CallableFunction)(data.length);
        //             const wasmDataOffset = (exports.vecOffset as CallableFunction)(wasmDataPtr);
        //             new Uint8Array(memory.buffer, wasmDataOffset, data.length).set(data);
        //             connection.console.log(`[notify] fsReadFileAsync(${promiseId}, ${path}) = ${data.length}`);
        //             notifyResult(serverPtr, promiseId, wasmDataPtr);
        //             connection.console.log("notified");
        //         }
        //     });
        notifyResult(serverPtr, promiseId, 0);
    }

    function fsReadSubDirsAsync(promiseId: number, pathOffset: number, pathLen: number) {
        if (!wasmMemory || !wasmExports) {
            throw new Error("Unreachable");
        }
        const exports = wasmExports;
        const memory = wasmMemory;
        const notifyResult = wasmExports.notifyFsReadSubDirsAsyncResult as CallableFunction;
        notifyResult(serverPtr, promiseId, 0);

        // // TODO: use async version
        // const parentDir = new TextDecoder('utf-8').decode(
        //     new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
        // let subDirsJson;
        // try {
        //     const subDirs = fs.readdirSync(parentDir, { withFileTypes: true })
        //         .filter(dirent => dirent.isDirectory())
        //         .map(dirent => path.join(parentDir, dirent.name));
        //     subDirsJson = JSON.stringify(subDirs);
        // } catch (e) {
        //     connection.console.log(`Failed to read subdirs of ${parentDir}: ${e}`);
        //     return 0;
        // }

        // const data = new TextEncoder().encode(subDirsJson);
        // const wasmDataPtr = (wasmExports.allocateVec as CallableFunction)(data.length);
        // const wasmDataOffset = (wasmExports.vecOffset as CallableFunction)(wasmDataPtr);
        // new Uint8Array(wasmMemory.buffer, wasmDataOffset, data.length).set(data);
        // (wasmExports.notifyFsReadSubDirsAsyncResult as CallableFunction)(serverPtr, promiseId, wasmDataPtr);
    }

    const importOjbect = {
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

    const config = { erlLibs };
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
        if (!wasmExports || !wasmMemory) {
            throw new Error("Unreachable");
        }

        const ready = (wasmExports.tryRunOne as CallableFunction)(poolPtr);
        if (!ready) {
            setTimeout(() => waitOutgoingMessage(resolve), 100);// TODO
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

    connection.listen();
}

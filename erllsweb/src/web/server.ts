import { createConnection, BrowserMessageReader, BrowserMessageWriter } from 'vscode-languageserver/browser';
import { InitializeParams, InitializeResult, ServerCapabilities } from 'vscode-languageserver';

// TODO
const _erlLibs = ["/usr/local/lib/erlang/lib", "_checkouts", "_build/default/lib"];

let wasmMemory: WebAssembly.Memory | undefined;
let wasmExports: WebAssembly.Exports | undefined;
let serverPtr: number = 0;
let poolPtr: number = 0;


type Message = { data: { type: 'initialize', wasmBytes: ArrayBuffer, port: MessagePort } };
self.onmessage = async (msg: Message) => {
    if (msg.data.type !== 'initialize') {
        throw new Error('Unexpected message: ' + JSON.stringify(msg.data));
    }

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
        // connection.console.log(`fsExistsAsync(${promiseId}, ${pathOffset}, ${pathLen})`);

        // if (!wasmMemory || !wasmExports) {
        //     return false;
        // }
        // const path = new TextDecoder('utf-8').decode(
        //     new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
        // const result = fs.existsSync(path);
        // connection.console.log(`fsExistsAsync(${promiseId}, ${path}) = ${result}`);
        // (wasmExports.notifyFsExistsAsyncResult as CallableFunction)(serverPtr, promiseId, result);
    }

    function fsReadFileAsync(promiseId: number, pathOffset: number, pathLen: number) {
        // connection.console.log(`fsReadFileAsync(${promiseId}, ${pathOffset}, ${pathLen})`);
        // if (!wasmMemory || !wasmExports) {
        //     throw new Error("Unreachable");
        // }
        // const exports = wasmExports;
        // const memory = wasmMemory;
        // const notifyResult = wasmExports.notifyFsReadFileAsyncResult as CallableFunction;
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
    }

    function fsReadSubDirsAsync(promiseId: number, pathOffset: number, pathLen: number) {
        // connection.console.log(`fsReadSubDirsAsync(${promiseId}, ${pathOffset}, ${pathLen})`);
        // if (!wasmMemory || !wasmExports) {
        //     throw new Error("Unreachable");
        // }
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

    connection.console.log('WASM initialized');

    connection.onInitialize(async (_params: InitializeParams): Promise<InitializeResult> => {
        connection.console.log('initialize: ' + JSON.stringify(_params));

        const capabilities: ServerCapabilities = {
            //colorProvider: {} // provide a color provider
        };
        return { capabilities };
    });

    connection.listen();
}

// const messageReader = new BrowserMessageReader(self);
// const messageWriter = new BrowserMessageWriter(self);
// const connection = createConnection(messageReader, messageWriter);

// connection.onInitialize(async (_params: InitializeParams): Promise<InitializeResult> => {
//     connection.console.log('initialize: ' + JSON.stringify(_params));

//     const capabilities: ServerCapabilities = {
//         //colorProvider: {} // provide a color provider
//     };
//     return { capabilities };
// });

// connection.listen();

// self.onmessage = async (msg: Message) => {
//     connection.console.log('Unexpected message: ' + JSON.stringify(msg.data));
// }

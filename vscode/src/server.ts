import * as fs from "fs";

import {
    createConnection,
    InitializeParams,
    InitializeResult,
} from 'vscode-languageserver/node';


const args = process.argv.slice(2);
const wasmPath = args[0];
const wasmBuffer = fs.readFileSync(wasmPath);
let wasmMemory: WebAssembly.Memory | undefined;
let wasmInstance: WebAssembly.Instance | undefined;
let serverPtr: number = 0;

const connection = createConnection();

async function initializeWasm() {
    const importOjbect = {
        env: {
            fsExists(pathOffset: number, pathLen: number): boolean {
                if (!wasmMemory) {
                    return false;
                }
                const path = new TextDecoder('utf-8').decode(
                    new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
                return fs.existsSync(path);
            },
            fsReadFile(pathOffset: number, pathLen: number): number {
                if (!wasmMemory || !wasmInstance) {
                    return 0;
                }
                const path = new TextDecoder('utf-8').decode(
                    new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
                const data = fs.readFileSync(path);

                const wasmDataPtr = (wasmInstance.exports.allocateVec as CallableFunction)(data.length);
                const wasmDataOffset = (wasmInstance.exports.vecOffset as CallableFunction)(wasmDataPtr);
                new Uint8Array(wasmMemory.buffer, wasmDataOffset, data.length).set(data);
                return wasmDataPtr;
            },
            fsReadSubDirs(pathOffset: number, pathLen: number): number {
                if (!wasmMemory || !wasmInstance) {
                    return 0;
                }
                const path = new TextDecoder('utf-8').decode(
                    new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
                const subDirs = fs.readdirSync(path, { withFileTypes: true })
                    .filter(dirent => dirent.isDirectory())
                    .map(dirent => dirent.name);
                const subDirsJson = JSON.stringify(subDirs);

                const data = new TextEncoder().encode(subDirsJson);
                const wasmDataPtr = (wasmInstance.exports.allocateVec as CallableFunction)(data.length);
                const wasmDataOffset = (wasmInstance.exports.vecOffset as CallableFunction)(wasmDataPtr);
                new Uint8Array(wasmMemory.buffer, wasmDataOffset, data.length).set(data);
                return wasmDataPtr;
            },
        }
    };
    wasmInstance = (await WebAssembly.instantiate(wasmBuffer, importOjbect)).instance;
    wasmMemory = wasmInstance.exports.memory as WebAssembly.Memory;

    serverPtr = (wasmInstance.exports.newServer as CallableFunction)();

    // TODO: updateConfig
}

async function handleIncomingMessage(
    method: string,
    params: any[] | object | undefined
): Promise<any[] | object | undefined> {
    if (!wasmInstance || !wasmMemory) {
        await initializeWasm();
    }
    if (!wasmInstance || !wasmMemory) {
        throw new Error("Failed to initialize wasm");
    }

    const message = {
        jsonrpc: "2.0",
        id: 0, // dummy value
        method,
        params
    };
    const messageJsonBytes = new TextEncoder().encode(JSON.stringify(message));

    const wasmMessagePtr =
        (wasmInstance.exports.allocateVec as CallableFunction)(messageJsonBytes.length);
    const wasmMessageOffset =
        (wasmInstance.exports.vecOffset as CallableFunction)(wasmMessagePtr);
    new Uint8Array(wasmMemory.buffer, wasmMessageOffset, messageJsonBytes.length).set(messageJsonBytes);
    (wasmInstance.exports.handleIncomingMessage as CallableFunction)(
        serverPtr, wasmMessagePtr, messageJsonBytes.length);

    let resultParams: any[] | object | undefined = undefined;
    while (true) {
        const wasmOutgoingMessagePtr =
            (wasmInstance.exports.takeOutgoingMessage as CallableFunction)(serverPtr);
        if (wasmOutgoingMessagePtr === 0) {
            break;
        }

        const wasmOutgoingMessageLen =
            (wasmInstance.exports.vecLen as CallableFunction)(wasmOutgoingMessagePtr);
        const wasmOutgoingMessageOffset =
            (wasmInstance.exports.vecOffset as CallableFunction)(wasmOutgoingMessagePtr);
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

    return resultParams;
}

connection.onRequest(async (method, params) => {
    connection.console.log("onRequest: " + method);
    const resultParams = await handleIncomingMessage(method, params);
    return resultParams;
});

connection.onNotification(async (method, params) => {
    connection.console.log("onNotification: " + method);
    await handleIncomingMessage(method, params);
});

connection.listen();

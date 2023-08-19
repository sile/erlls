import * as fs from "fs";
import * as child_process from "child_process";
import * as path from "path";

import {
    createConnection,
    InitializeResult,
} from 'vscode-languageserver/node';


const args = process.argv.slice(2);
const wasmPath = args[0];
const wasmBuffer = fs.readFileSync(wasmPath);
let wasmMemory: WebAssembly.Memory | undefined;
let wasmInstance: WebAssembly.Instance | undefined;
let wasmExports: WebAssembly.Exports | undefined;
let serverPtr: number = 0;
let poolPtr: number = 0;

const configJson = args[1];
const config = JSON.parse(configJson);

const connection = createConnection();

async function initializeWasm() {
    const importOjbect = {
        env: {
            consoleLog(msgOffset: number, msgLen: number) {
                if (!wasmMemory) {
                    return;
                }
                const msg = new TextDecoder('utf-8').decode(
                    new Uint8Array(wasmMemory.buffer, msgOffset, msgLen));
                connection.console.log(msg);
            },
            fsExistsAsync(promiseId: number, pathOffset: number, pathLen: number) {
                connection.console.log(`fsExistsAsync(${promiseId}, ${pathOffset}, ${pathLen})`);

                if (!wasmMemory || !wasmExports) {
                    return false;
                }
                const path = new TextDecoder('utf-8').decode(
                    new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
                const result = fs.existsSync(path);
                connection.console.log(`fsExistsAsync(${promiseId}, ${path}) = ${result}`);
                (wasmExports.notifyFsExistsAsyncResult as CallableFunction)(serverPtr, promiseId, result);

            },
            fsReadFileAsync(promiseId: number, pathOffset: number, pathLen: number) {
                connection.console.log(`fsReadFileAsync(${promiseId}, ${pathOffset}, ${pathLen})`);
                if (!wasmMemory || !wasmExports) {
                    return 0;
                }
                // TODO: use async version
                const path = new TextDecoder('utf-8').decode(
                    new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
                const data = fs.readFileSync(path);

                const wasmDataPtr = (wasmExports.allocateVec as CallableFunction)(data.length);
                const wasmDataOffset = (wasmExports.vecOffset as CallableFunction)(wasmDataPtr);
                new Uint8Array(wasmMemory.buffer, wasmDataOffset, data.length).set(data);
                (wasmExports.notifyFsReadFileAsyncResult as CallableFunction)(serverPtr, promiseId, wasmDataPtr);
            },
            fsReadSubDirsAsync(promiseId: number, pathOffset: number, pathLen: number) {
                connection.console.log(`fsReadSubDirsAsync(${promiseId}, ${pathOffset}, ${pathLen})`);
                if (!wasmMemory || !wasmExports) {
                    return 0;
                }
                // TODO: use async version
                const parentDir = new TextDecoder('utf-8').decode(
                    new Uint8Array(wasmMemory.buffer, pathOffset, pathLen));
                let subDirsJson;
                try {
                    const subDirs = fs.readdirSync(parentDir, { withFileTypes: true })
                        .filter(dirent => dirent.isDirectory())
                        .map(dirent => path.join(parentDir, dirent.name));
                    subDirsJson = JSON.stringify(subDirs);
                } catch (e) {
                    connection.console.log(`Failed to read subdirs of ${parentDir}: ${e}`);
                    return 0;
                }

                const data = new TextEncoder().encode(subDirsJson);
                const wasmDataPtr = (wasmExports.allocateVec as CallableFunction)(data.length);
                const wasmDataOffset = (wasmExports.vecOffset as CallableFunction)(wasmDataPtr);
                new Uint8Array(wasmMemory.buffer, wasmDataOffset, data.length).set(data);
                (wasmExports.notifyFsReadSubDirsAsyncResult as CallableFunction)(serverPtr, promiseId, wasmDataPtr);
            },
        }
    };
    wasmInstance = (await WebAssembly.instantiate(wasmBuffer, importOjbect)).instance;
    wasmExports = wasmInstance.exports;
    wasmMemory = wasmExports.memory as WebAssembly.Memory;

    serverPtr = (wasmExports.newServer as CallableFunction)();
    poolPtr = (wasmExports.newLocalPool as CallableFunction)();

    child_process.exec("erl -boot start_clean -noshell -eval 'io:format(code:lib_dir(kernel)).' -s init stop", (_error, stdout, _stderr) => {
        if (wasmExports === undefined || wasmMemory === undefined) {
            return;
        }
        if (!stdout) {
            return;
        }
        const libDir = path.dirname(stdout);
        if (!libDir) {
            return;
        }

        const config = { "erlLibs": [libDir, "_checkouts", "_build/default/lib"] };
        const configJsonBytes = new TextEncoder().encode(JSON.stringify(config));
        const wasmConfigPtr =
            (wasmExports.allocateVec as CallableFunction)(configJsonBytes.length);
        const wasmConfigOffset =
            (wasmExports.vecOffset as CallableFunction)(wasmConfigPtr);
        new Uint8Array(wasmMemory.buffer, wasmConfigOffset, configJsonBytes.length).set(configJsonBytes);
        (wasmExports.updateConfig as CallableFunction)(serverPtr, wasmConfigPtr);
    });
}

async function handleIncomingMessage(
    message: object
): Promise<any[] | object | undefined> {
    connection.console.log(`handleIncomingMessage()`);
    if (!wasmExports || !wasmMemory) {
        await initializeWasm();
    }
    if (!wasmExports || !wasmMemory) {
        throw new Error("Failed to initialize wasm");
    }
    const messageJsonBytes = new TextEncoder().encode(JSON.stringify(message));

    const wasmMessagePtr =
        (wasmExports.allocateVec as CallableFunction)(messageJsonBytes.length);
    const wasmMessageOffset =
        (wasmExports.vecOffset as CallableFunction)(wasmMessagePtr);
    new Uint8Array(wasmMemory.buffer, wasmMessageOffset, messageJsonBytes.length).set(messageJsonBytes);
    (wasmExports.handleIncomingMessage as CallableFunction)(poolPtr, serverPtr, wasmMessagePtr);
    (wasmExports.tryRunOne as CallableFunction)(poolPtr);

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

    return resultParams;
}

connection.onInitialize(async (params) => {
    const message = {
        jsonrpc: "2.0",
        id: 0, // dummy value
        method: "initialize",
        params
    };
    const resultParams = await handleIncomingMessage(message) as InitializeResult;

    if (!config.enableCompletion) {
        connection.console.info("Disabling completion provider");
        resultParams.capabilities.completionProvider = undefined;
    }

    return resultParams as InitializeResult;
});

connection.onRequest(async (method, params) => {
    const message = {
        jsonrpc: "2.0",
        id: 0, // dummy value
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

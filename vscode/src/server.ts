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
                new Uint8Array(wasmMemory.buffer, wasmDataPtr, data.length).set(data);
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
                new Uint8Array(wasmMemory.buffer, wasmDataPtr, data.length).set(data);
                return wasmDataPtr;
            },
        }
    };
    wasmInstance = (await WebAssembly.instantiate(wasmBuffer, importOjbect)).instance;
    wasmMemory = wasmInstance.exports.memory as WebAssembly.Memory;
}

connection.onInitialize(async (params: InitializeParams) => {
    await initializeWasm();
    connection.console.log("onInitialize: " + wasmPath);
    connection.console.log(JSON.stringify(params));

    // TODO: updateConfig

    // const new TextEncoder().encode(JSON.stringify(params));

    const result: InitializeResult = {
        capabilities: {}
    };

    // };
    // if (hasWorkspaceFolderCapability) {
    //     result.capabilities.workspace = {
    //         workspaceFolders: {
    //             supported: true
    //         }
    //     };
    // }
    return result;
});

connection.onInitialized(() => {
    connection.console.log("onInitialized");
});

connection.listen();

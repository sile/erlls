import * as fs from "fs";

import {
    createConnection,
    InitializeParams,
    InitializeResult,
} from 'vscode-languageserver/node';


const args = process.argv.slice(2);
const wasmPath = args[0];
const wasmBuffer = fs.readFileSync(wasmPath);

const connection = createConnection();

connection.onInitialize(async (params: InitializeParams) => {
    connection.console.log("onInitialize: " + wasmPath);
    connection.console.log(JSON.stringify(params));

    const importOjbect = {
        env: {
            fsExists(pathPtr: number, pathLen: number): boolean {
                // TODO
                return false;
            },
            fsReadFile(pathPtr: number, pathLen: number): number {
                return 0;
            },
            fsReadSubDirs(pathPtr: number, pathLen: number): number {
                return 0;
            },
        }
    };
    const wasmInstance = await WebAssembly.instantiate(wasmBuffer, importOjbect);

    // JSON.parse(JSON.stringify(params));

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

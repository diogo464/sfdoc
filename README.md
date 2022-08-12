sfdoc
====

CLI utility to parse Starfall's documentation and generate meta files for lua language server and snippet files for vscode.

## Basic usage

Parse the documentation out of the code into a json file.
```bash
sfdoc parse <path to starfall>/lua/starfall/libs_*
```
This will create a `docs.json` file need to generate the meta files and snippets.

Generate the meta files.
```bash
sfdoc generate meta
```
This will use the `docs.json` and create a directory named `docs` with the documentation inside.

Generate the snippets.
```bash
sfdoc generate snippets
```
This will create a file named `starfall.code-snippets`.

Open your starfall folder, `.../GarrysMod/garrysmod/data/starfall/` with vscode and change the settings to contain:
```json
    "Lua.workspace.library": [
        "<path to the generated docs/ directory>",
    ],
    "Lua.runtime.nonstandardSymbol": [
        "//",
        "/**/",
        "!",
        "||",
        "&&",
        "!=",
        "continue",
    ],
    "files.associations": {
        "*.txt": "lua",
    }
```
and move the `starfall.code-snippets` into the `.vscode` folder.

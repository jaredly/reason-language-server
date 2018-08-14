# Reason Language Server

This project implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/specification#initialize) for reason.

It is written in Reason, and compiled via bsb-native. The goal is for it to work equally well on Windows, MacOS, and Linux.

## Usage

### VSCode

(for now) download the `.vsix` file from the [latest release](https://github.com/jaredly/reason-language-server/releases), and in the vscode extensions panel, open the menu and click "install from .vsix". And that's it!
Works for macos, windows, linux.

### OniVim

Oni has support for reason baked in, and it only needs a little bit of configuration to integrate this langauge server.

1. Download the `your-platform.zip` file from the [latest release](https://github.com/jaredly/reason-language-server/releases), and unzip it. put the `reason-language-server.exe` file somewhere.
2. Open the oni config file (`cmd+,`), and add the following to the `export const configuration = {` block at the end:
```
    "language.reason.languageServer.command": "/abs/path/to/your/reason-language-server.exe",
    "language.reason.languageServer.arguments": [],
    "language.reason.languageServer.configuration": {},
```
And you're done!

### Sublime Text

1. Install the [sublime reason](https://github.com/reasonml-editor/sublime-reason) plugin for syntax highlighting, etc. (has to be done manually)
2. Install the [LSP Plugin](https://github.com/tomv564/LSP) via the sublime package manager
3. Download the `your-platform.zip` file from the [latest release](https://github.com/jaredly/reason-language-server/releases), and unzip it. put the `reason-language-server.exe` file somewhere.
4. cmd+shift+p and type "Preferences: LSP Settings" to bring up the settings file, and put in:
```
{
  "auto_show_diagnostics_panel": false,
  "clients": {
    "reason": {
      "enabled": true,
      "command": ["/absolute/path/to/reason-language-server.exe"],
      "scopes": ["source.reason"],
      "syntaxes": ["Packages/sublime-reason/Reason.tmLanguage"],
      "languageId": "reason"
    }
  }
}
```

### Vim

Install [the reason-vim plugin](https://github.com/reasonml-editor/vim-reason-plus), following the readme. The only change is the LSP configuration should be:
```
let g:LanguageClient_serverCommands = {
    \ 'reason': ['/absolute/path/to/reason-language-server.exe']
    \ }
```

### Emacs

_TODO_ people have gotten it to work with emacs, but I don't know the steps.

## What about [ocaml-language-server](https://github.com/freebroccolo/ocaml-language-server/)?

That project uses [merlin](https://github.com/ocaml/merlin) under the hood, which is a very powerful and well-developed tool for IDE features in OCaml/Reason.
I had a couple of reasons for starting a new one. The biggest is that I wanted something with minimal dependencies, so that windows support would be relatively easy, and so that I might be able to ship it with bucklescript at some future point. (it also makes targetting JS easier). I also wanted a server that was written entirely in Reason (not part typescript, part reason), and something that was written from the ground up with the Langauge Server Protocol in mind, instead of taking a different IDE-support-tool and mapping the LSP onto it.

## Contributing

- clone
- cd to the cloned dir
- run `npm install` from the main project dir
- install the client's dependencies `cd client && npm i && cd ..`
- start it up `npm start`
- open this project in vscode

## To test your changes in one of the example projects
- open the "Debug" pane in vscode. Select a debug target. Press "Run"
- it should start up a window titled `[Extension development host]`
- you'll have to `npm install && npm run build` in that project directory if you haven't already.
- to reload the server if something goes wrong, `cmd+shift+p "restart reason language server"`

You can then develop on the language server! When you change something, the server will automatically reload.


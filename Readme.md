# Reason Language Server

This project implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/specification#initialize) for reason.

It is written in Reason, and compiled via bsb-native. The goal is for it to work equally well on Windows, MacOS, and Linux.

## What about [ocaml-language-server](https://github.com/freebroccolo/ocaml-language-server/)?

That project uses [merlin](https://github.com/ocaml/merlin) under the hood, which is a very powerful and well-developed tool for IDE features in OCaml/Reason.
I had a couple of reasons for starting a new one. The biggest is that I wanted something with minimal dependencies, so that windows support would be relatively easy, and so that I might be able to ship it with bucklescript at some future point. (it also makes targetting JS easier). I also wanted a server that was written entirely in Reason (not part typescript, part reason), and something that was written from the ground up with the Langauge Server Protocol in mind, instead of taking a different IDE-support-tool and mapping the LSP onto it.

## Contributing

- clone
- cd to the cloned dir
- run `npm install` from the main project dir
- run the following command to install dependenceis for example projects `for dir in ./examples/*; do (cd "$dir" && npm install); done`
- install the client's dependencies `cd client && npm i && cd ..`
- start it up `npm start`
- open this project in vscode
- select "Debug > Start Without Debugging" from the vscode menu
- it should start up a window titled `[Extension development host]`

You can then develop on the language server! When you change something, the server will automatically reload.

## Roadmap

- Code completion
- Open tracking
- Signature help
- Rename
- Codelens
  - open tracking
  - toplevel type definitions
- Hover
  - type definitions
- Range formatting (no change)
- Document Symbol (maybe I want Extent in addition to name loc)




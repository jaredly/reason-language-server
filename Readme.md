# Reason Language Server

This project implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/specification#initialize) for reason.

It is written in Reason, and compiled via bsb-native. The goal is for it to work equally well on Windows, MacOS, and Linux.

## What about [ocaml-language-server](https://github.com/freebroccolo/ocaml-language-server/)?

That project uses [merlin](https://github.com/ocaml/merlin) under the hood, which is a very powerful and well-developed tool for IDE features in OCaml/Reason.
I had a couple of reasons for starting a new one. The biggest is that I wanted something with minimal dependencies, so that windows support would be relatively easy, and so that I might be able to ship it with bucklescript at some future point. (it also makes targetting JS easier). I also wanted a server that was written entirely in Reason (not part typescript, part reason), and something that was written from the ground up with the Langauge Server Protocol in mind, instead of taking a different IDE-support-tool and mapping the LSP onto it.

## Contributing

- open this project in vscode
- install dependencies and build the example project `cd example-project && npm i && npm run build && cd ..`
- install the client's dependencies `cd client && npm i && cd ..`
- install the server's dependencies & start it running `npm i && npm start`
- select "Debug > Start Without Debugging" from the vscode menu
- it should start up a window titled `[Extension development host]`
- you then need to open the 'example-project' in that development window, and clicking "open folder" doesn't seem to do it -- I've had to drag the folder from finder on to the window, and then it will work.

You can then develop on the language server! When you change something, the server will automatically reload.

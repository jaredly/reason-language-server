# Reason Language Server

This project implements the [Language Server Protocol](https://microsoft.github.io/language-server-protocol/specification#initialize) for reason.

It is written in Reason, and compiled via bsb-native. The goal is for it to work equally well on Windows, MacOS, and Linux.

# Contributing

- open this project in vscode
- install dependencies and build the example project `cd example-project && npm i && npm build && cd ..`
- install the client's dependencies `cd client && npm i && cd ..`
- install the server's dependencies & start it running `npm i && npm start`
- select "Debug > Start Without Debugging" from the vscode menu
- it should start up a window titled `[Extension development host]`
- you then need to open the 'example-project' in that development window, and clicking "open folder" doesn't seem to do it -- I've had to drag the folder from finder on to the window, and then it will work.

You can then develop on the language server! When you change something, the server will automatically reload.

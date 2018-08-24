
# (cp lib/bs/native/bin.native client; cd client; vsce package);

rm -rf _zip
mkdir -p _zip/reason-language-server
cd _zip

chmod +x ../editor-extensions/vscode/bin.native
chmod +x ../editor-extensions/vscode/bin.native.exe
chmod +x ../editor-extensions/vscode/bin.native.linux

cp ../editor-extensions/vscode/bin.native reason-language-server/reason-language-server.exe
chmod +x reason-language-server/reason-language-server.exe
zip -r ../editor-extensions/vscode/macos.zip reason-language-server

cp ../editor-extensions/vscode/bin.native.linux reason-language-server/reason-language-server.exe
chmod +x reason-language-server/reason-language-server.exe
zip -r ../editor-extensions/vscode/linux.zip reason-language-server

cp ../editor-extensions/vscode/bin.native.exe reason-language-server/reason-language-server.exe
chmod +x reason-language-server/reason-language-server.exe
zip -r ../editor-extensions/vscode/windows.zip reason-language-server

cd ..
rm -rf _zip
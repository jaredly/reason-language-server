
# (cp lib/bs/native/bin.native client; cd client; vsce package);

rm -rf _zip
mkdir -p _zip/reason-language-server
cd _zip

cp ../client/bin.native reason-language-server/reason-language-server.exe
chmod +x reason-language-server/reason-language-server.exe
zip -r ../client/macos.zip reason-language-server

cp ../client/bin.native.linux reason-language-server/reason-language-server.exe
chmod +x reason-language-server/reason-language-server.exe
zip -r ../client/linux.zip reason-language-server

cp ../client/bin.native.exe reason-language-server/reason-language-server.exe
chmod +x reason-language-server/reason-language-server.exe
zip -r ../client/windows.zip reason-language-server

cd ..
rm -rf _zip
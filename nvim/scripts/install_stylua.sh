#! /usr/bin/env zsh

DOWNLOAD_URL=$(curl -s https://api.github.com/repos/JohnnyMorganz/StyLua/releases/latest \
        | grep browser_download_url \
        | grep macos \
        | cut -d '"' -f 4)

curl -L $DOWNLOAD_URL > stylua.zip
unzip stylua.zip
mkdir -p ~/.local/bin
mv stylua ~/.local/bin/stylua
chmod +x ~/.local/bin/stylua
rm stylua.zip


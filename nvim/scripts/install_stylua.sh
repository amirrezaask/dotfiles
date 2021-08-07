#! /usr/bin/zsh

DOWNLOAD_URL=$(curl -s https://api.github.com/repos/JohnnyMorganz/StyLua/releases/latest \
        | grep browser_download_url \
        | grep linux \
        | cut -d '"' -f 4)

curl -L $DOWNLOAD_URL > stylua.zip
unzip stylua.zip
mv stylua $BIN_PATH/stylua
chmod +x $BIN_PATH/stylua
rm stylua.zip


#! /bin/bash
git clone https://github.com/sumneko/lua-language-server ~/.local/lua-language-server

cd ~/.local/lua-language-server
git submodule update --init --recursive
cd 3rd/luamake
compile/install.sh
cd ../..
./3rd/luamake/luamake rebuild

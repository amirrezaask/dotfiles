#!/usr/bin/env bash

if ! test -d "$HOME/.neovim"; then
    git clone https://github.com/neovim/neovim.git "$HOME/.neovim"
fi
pushd "$HOME/.neovim"
git pull
make CMAKE_BUILD_TYPE=Release
popd

#!/usr/bin/env bash
SCRIPT_PATH=`dirname $0`
DOTFILES_PATH=`dirname $SCRIPT_PATH`

RELEASE="master"

if ! test -d "$HOME/.neovim"; then
    git clone https://github.com/neovim/neovim.git "$HOME/.neovim"
fi

pushd "$HOME/.neovim"
git pull
git checkout "{$RELEASE}"
make CMAKE_BUILD_TYPE=Release
popd
mkdir -p $HOME/.config

ln -s $DOTFILES_PATH/editor/nvim $HOME/.config/nvim

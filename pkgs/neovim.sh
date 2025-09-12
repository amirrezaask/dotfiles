#!/usr/bin/env bash

set -e
NEOVIM_RELEASE=$1
NEOVIM_DIR="$HOME/.local/neovim"

if [ -z "$NEOVIM_RELEASE" ]; then
	NEOVIM_RELEASE="master"
fi

if [ ! -d $NEOVIM_DIR ]; then
	git clone --depth 1 https://github.com/neovim/neovim.git $NEOVIM_DIR
fi

cd $NEOVIM_DIR

git pull
git checkout "$NEOVIM_RELEASE"

sudo make distclean || true
sudo make clean || true
make CMAKE_BUILD_TYPE=Release
sudo make install

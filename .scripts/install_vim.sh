#!/bin/bash
VIM_FOUND=0
NVIM_FOUND=0

command -v vim &> /dev/null
if [ "$?" != '0' ]; then
    echo "Found vim installation"
    $VIM_FOUND=1
fi

command -v nvim &> /dev/null
if [ "$?" != '0' ]; then 
    echo "Found neovim installation"
    $NVIM_FOUND=1
fi

if [ "$NVIM_FOUND" == '0' ]; then
    echo "Installing neovim configuration"
    if [ -d "$HOME/.config/nvim" ]; then 
        echo "nvim config already exists, backing up"
        rm -rf ~/.config/nvim_bak
        mv ~/.config/nvim ~/.config/nvim_bak
    fi
    ln -s $(pwd)/nvim ~/.config/
fi

if [ "$VIM_FOUND" == '0' ]; then
    echo "Installing vim configuration"
    if [[ -L "$HOME/.vimrc" ]]; then 
        mv ~/.vimrc ~/.vimrc_bak
    fi
    ln -s $(pwd)/vimrc ~/.vimrc
fi






#! /bin/bash

# Install Homebrew
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

# Install Brew Cask
brew tap caskroom/cask

brew install --cask google-chrome
brew install --cask brave-browser
brew install --cask ghostty

git clone https://github.com/amirrezaask/dotfiles.git ~/.dotfiles
push ~/.dotfiles/
make install
popd

git clone https://github.com/neovim/neovim.git ~/.neovim
push ~/.neovim/
make CMAKE_BUILD_TYPE=RelWithDebInfo 
sudo make install
popd


#!/usr/bin/env bash

if ! command -v brew &> /dev/null; then
    /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
fi

brew install go
brew install openfortivpn
brew install --cask brave-browser
brew install --cask google-chrome


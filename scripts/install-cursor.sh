#!/usr/bin/env bash

# check if macos
if [[ "$OSTYPE" == "darwin"* ]]; then
    if test -d "$HOME/Library/Application Support/Cursor"; then
        rm -rf "$HOME/Library/Application Support/Cursor/User/keybindings.json"
        rm -rf "$HOME/Library/Application Support/Cursor/User/settings.json"
        ln -s "$(pwd)/editor/vscode/keybindings.json" "$HOME/Library/Application Support/Cursor/User/keybindings.json"
        ln -s "$(pwd)/editor/vscode/settings.json" "$HOME/Library/Application Support/Cursor/User/settings.json"
    fi
    if test -d "$HOME/Library/Application Support/Code"; then
        rm -rf "$HOME/Library/Application Support/Code/User/keybindings.json"
        rm -rf "$HOME/Library/Application Support/Code/User/settings.json"
        ln -s "$(pwd)/editor/vscode/keybindings.json" "$HOME/Library/Application Support/Code/User/keybindings.json"
        ln -s "$(pwd)/editor/vscode/settings.json" "$HOME/Library/Application Support/Code/User/settings.json"
    fi
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Not implemented yet"
fi

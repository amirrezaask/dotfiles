#!/usr/bin/env bash

# check if macos
if [[ "$OSTYPE" == "darwin"* ]]; then
    rm -rf "$HOME/Library/Application Support/Cursor/User/keybindings.json"
    rm -rf "$HOME/Library/Application Support/Cursor/User/settings.json"
    ln -s "$(pwd)/editor/vscode/keybindings.json" "$HOME/Library/Application Support/Cursor/User/keybindings.json"
    ln -s "$(pwd)/editor/vscode/settings.json" "$HOME/Library/Application Support/Cursor/User/settings.json"
elif [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "Not implemented yet"
fi

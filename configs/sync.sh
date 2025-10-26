#!/usr/bin/env bash
set -e

CONFIGS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
XDG_CONFIG="$HOME/.config"

echo "CONFIGS_DIR $CONFIGS_DIR"

rm -rf "$XDG_CONFIG/nvim" \
	"$XDG_CONFIG/ghostty" \
	"$XDG_CONFIG/fish" \
	"$XDG_CONFIG/sway" \
	"$XDG_CONFIG/hypr" \
	"$XDG_CONFIG/waybar" \
	"$XDG_CONFIG/swayosd" \
	"$XDG_CONFIG/swaylock" \
	"$XDG_CONFIG/wofi" \
	"$XDG_CONFIG/alacritty" \
	"$XDG_CONFIG/btop" \
	"$XDG_CONFIG/kitty" \
	"$XDG_CONFIG/starship.toml" \
	"$XDG_CONFIG/Code/User/settings.json" \
	"$XDG_CONFIG/Code/User/keybindings.json" \
	"$XDG_CONFIG/zed/settings.json" \
	"$XDG_CONFIG/zed/keymap.json" \
	"$XDG_CONFIG/bash" \
	"$HOME/.zshrc" \
	"$HOME/.gitconfig" \
	"$HOME/.bashrc" \
	"$HOME/.tmux.conf"

mkdir -p "$XDG_CONFIG"
mkdir -p "$XDG_CONFIG/Code/User"

ln -s "$CONFIGS_DIR/nvim" "$XDG_CONFIG/nvim"
ln -s "$CONFIGS_DIR/fish" "$XDG_CONFIG/fish"
ln -s "$CONFIGS_DIR/sway" "$XDG_CONFIG/sway"
ln -s "$CONFIGS_DIR/hypr" "$XDG_CONFIG/hypr"
ln -s "$CONFIGS_DIR/waybar" "$XDG_CONFIG/waybar"
ln -s "$CONFIGS_DIR/swayosd" "$XDG_CONFIG/swayosd"
ln -s "$CONFIGS_DIR/swaylock" "$XDG_CONFIG/swaylock"
ln -s "$CONFIGS_DIR/wofi" "$XDG_CONFIG/wofi"
ln -s "$CONFIGS_DIR/alacritty" "$XDG_CONFIG/alacritty"
ln -s "$CONFIGS_DIR/btop" "$XDG_CONFIG/btop"
ln -s "$CONFIGS_DIR/kitty" "$XDG_CONFIG/kitty"
ln -s "$CONFIGS_DIR/ghostty" "$XDG_CONFIG/ghostty"
ln -s "$CONFIGS_DIR/starship.toml" "$XDG_CONFIG/starship.toml"
ln -s "$CONFIGS_DIR/.gitconfig" "$HOME/.gitconfig"
ln -s "$CONFIGS_DIR/code/settings.json" "$XDG_CONFIG/Code/User/settings.json"
ln -s "$CONFIGS_DIR/code/keybindings.json" "$XDG_CONFIG/Code/User/keybindings.json"
ln -s "$CONFIGS_DIR/zed/settings.json" "$XDG_CONFIG/zed/settings.json"
ln -s "$CONFIGS_DIR/zed/keymap.json" "$XDG_CONFIG/zed/keymap.json"
ln -s "$CONFIGS_DIR/bash" "$XDG_CONFIG/bash"
ln -s "$CONFIGS_DIR/bash/rc" "$HOME/.bashrc"
ln -s "$CONFIGS_DIR/zsh/.zshrc" "$HOME/.zshrc"
ln -s "$CONFIGS_DIR/tmux/tmux.conf" "$HOME/.tmux.conf"

# Now some mac stuff
if [[ "$(uname)" == "Darwin" ]]; then
  MACOS_CONFIGS_DIR="$HOME/Library/Application Support"
  echo "# macOS configuration Directory: $MACOS_CONFIGS_DIR"

  echo "Linking Sublime $MACOS_CONFIGS_DIR/Sublime Text/Packages/User"
  rm -rf "$MACOS_CONFIGS_DIR/Sublime Text/Packages/User"
  mkdir -p "$MACOS_CONFIGS_DIR/Sublime Text/Packages"
  ln -nsf "$CONFIGS_DIR/sublime" "$MACOS_CONFIGS_DIR/Sublime Text/Packages/User"

  echo "Linking VSCode $MACOS_CONFIGS_DIR/Code/User"
  rm -rf "$MACOS_CONFIGS_DIR/Code/User/settings.json"
  rm -rf "$MACOS_CONFIGS_DIR/Code/User/keybindings.json"
  ln -nsf "$CONFIGS_DIR/code/settings.json" "$MACOS_CONFIGS_DIR/Code/User/settings.json"
  ln -nsf "$CONFIGS_DIR/code/keybindings.json" "$MACOS_CONFIGS_DIR/Code/User/keybindings.json"

  echo "Linking Cursor $MACOS_CONFIGS_DIR/Cursor/User/settings.json"
  rm -rf "$MACOS_CONFIGS_DIR/Cursor/User/settings.json"
  rm -rf "$MACOS_CONFIGS_DIR/Cursor/User/keybindings.json"
  ln -nsf "$CONFIGS_DIR/code/settings.json" "$MACOS_CONFIGS_DIR/Cursor/User/settings.json"
  ln -nsf "$CONFIGS_DIR/code/keybindings.json" "$MACOS_CONFIGS_DIR/Cursor/User/keybindings.json"
fi

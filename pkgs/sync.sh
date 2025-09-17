#!/usr/bin/env bash

PKGS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$PKGS_DIR/.."

echo "PKGS directory: $PKGS_DIR"

# Detect the Linux distribution
if [ -f /etc/os-release ]; then
    . /etc/os-release
    DISTRO=$ID
else
    echo "Cannot determine distribution: /etc/os-release not found."
    exit 1
fi

# Install base OS packages
case "$DISTRO" in
    "ubuntu")
		bash "$PKGS_DIR/fedora.sh"
        ;;
    "fedora")
		bash "$PKGS_DIR/ubuntu.sh"
        ;;
    *)
        echo "Unsupported distribution: $DISTRO"
        exit 1
        ;;
esac


# Install cargo
if ! command -v cargo >/dev/null 2>&1; then
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
fi

if [[ -f "$HOME/.cargo/env" ]]; then
	. "$HOME/.cargo/env"
fi

# manage bluetooth
if ! command -v bluetui >/dev/null 2>&1; then
	cargo install bluetui
fi

# better ls
if ! command -v exa >/dev/null 2>&1; then
	cargo install exa
fi

# Prompt
if ! command -v starship >/dev/null 2>&1; then
	cargo install starship --locked
fi

# Neovim
$PKGS_DIR/neovim.sh

# Systemd hooks for a seamless laptop experience
$PKGS_DIR/systemd.sh


# To let brightness control work
sudo usermod -a -G video $USER


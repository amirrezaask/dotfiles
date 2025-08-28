#!/usr/bin/env bash

BASE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DOTFILES_DIR="$BASE_DIR/.."

echo "Base directory: $BASE_DIR"

# Detect the Linux distribution
if [ -f /etc/os-release ]; then
    . /etc/os-release
    DISTRO=$ID
else
    echo "Cannot determine distribution: /etc/os-release not found."
    exit 1
fi

case "$DISTRO" in
    "ubuntu")
        echo "Installing Ubuntu packages"
        # Ubuntu-specific commands
		bash "$BASE_DIR/ubuntu/ubuntu.sh"
        ;;
    "fedora")
        echo "Installing Fedora packages"
        # Fedora-specific commands
		bash "$BASE_DIR/fedora/fedora.sh"
        ;;
    *)
        echo "Unsupported distribution: $DISTRO"
        exit 1
        ;;
esac

if !command -v starship &> /dev/null; then
	curl -sS https://starship.rs/install.sh | sh
fi


ln -nsf "$DOTFILES_DIR/themes" "$HOME/.config/themes"

"$DOTFILES_DIR/bin/theme-set" "Tokyo Night"

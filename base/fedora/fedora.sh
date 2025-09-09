#!/usr/bin/env bash

set -e

# sudo dnf update
sudo dnf copr -y enable solopasha/hyprland 
sudo dnf copr -y enable markupstart/SwayOSD
sudo dnf copr -y enable dejan/lazygit
sudo dnf copr -y enable scottames/ghostty
sudo dnf copr -y enable azandure/clipse
sudo dnf copr -y enable lihaohong/yazi

# Install base packages
sudo dnf -y install \
	git \
	curl \
	wget \
	btop \
	hyprland \
	hyprlock \
	hypridle \
	mako \
	fish \
	alacritty \
	swaybg \
	waybar \
	wl-clipboard \
	wofi \
	chromium \
	golang-go \
	dbus-devel  \
	pavucontrol \
	pkgconf-pkg-config \
	swayosd \
	lazygit \
	ghostty \
	kitty \
	clipse \
	yazi \
	fzf


# Install hyprshot for screenshots
if [[ ! -f /usr/bin/hyprshot ]]; then
	sudo curl https://raw.githubusercontent.com/Gustash/Hyprshot/refs/heads/main/hyprshot > ~/hyprshot
	sudo mv ~/hyprshot /usr/bin/hyprshot
	sudo chmod +x /usr/bin/hyprshot
fi

# Install cargo
if command -v nvim >/dev/null 2>&1; then
	curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
fi

if [[ -f "$HOME/.cargo/env" ]]; then
	. "$HOME/.cargo/env"
fi

# manage bluetooth
cargo install bluetui

# better ls
cargo install exa

# To let brightness control work
sudo usermod -a -G video $USER


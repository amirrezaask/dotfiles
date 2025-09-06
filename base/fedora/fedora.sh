#!/usr/bin/env bash

set -e

# sudo dnf update
sudo dnf copr -y enable solopasha/hyprland 
sudo dnf copr -y enable markupstart/SwayOSD
sudo dnf copr -y enable dejan/lazygit

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
	fzf


# Install hyprshot for screenshots
sudo curl https://raw.githubusercontent.com/Gustash/Hyprshot/refs/heads/main/hyprshot > ~/hyprshot
sudo mv ~/hyprshot /usr/bin/hyprshot
sudo chmod +x /usr/bin/hyprshot

# Install cargo
# curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

. "$HOME/.cargo/env"

# manage bluetooth
cargo install bluetui

# better ls
cargo install exa

# To let brightness control work
sudo usermod -a -G video $USER


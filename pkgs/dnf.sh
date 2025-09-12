#!/usr/bin/env bash

set -e

sudo dnf copr -y enable solopasha/hyprland 
sudo dnf copr -y enable markupstart/SwayOSD
sudo dnf copr -y enable dejan/lazygit
sudo dnf copr -y enable azandure/clipse

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
	clipse \
	ripgrep \
	fzf


# Install hyprshot for screenshots
if [[ ! -f /usr/bin/hyprshot ]]; then
	sudo curl https://raw.githubusercontent.com/Gustash/Hyprshot/refs/heads/main/hyprshot > ~/hyprshot
	sudo mv ~/hyprshot /usr/bin/hyprshot
	sudo chmod +x /usr/bin/hyprshot
fi


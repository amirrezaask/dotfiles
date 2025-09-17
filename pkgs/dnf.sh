#!/usr/bin/env bash

set -e

sudo dnf copr -y enable solopasha/hyprland 
sudo dnf copr -y enable markupstart/SwayOSD
sudo dnf copr -y enable dejan/lazygit
sudo dnf copr -y enable azandure/clipse


sudo dnf -y install \
    https://download1.rpmfusion.org/free/fedora/rpmfusion-free-release-42.noarch.rpm \
    https://download1.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-42.noarch.rpm

sudo dnf groupupdate multimedia --setop="install_weak_deps=False" --exclude=PackageKit-gstreamer-plugin
sudo dnf -y install gstreamer1-libav gstreamer1-plugins-{base,good,bad-free,bad-free-extras,ugly} ffmpeg

# Install base packages
sudo dnf -y install \
	git \
	curl \
	wget \
	btop \
	hyprland \
	hypridle \
	mako \
	fish \
	alacritty \
	swaybg \
	sway \
	swayidle \
	swaylock \
	waybar \
	wl-clipboard \
	wofi \
	chromium \
	golang-go \
	dbus-devel  \
	pavucontrol \
	pkgconf-pkg-config \
	swayosd \
	mpv \
	lazygit \
	clipse \
	ripgrep \
	cmake \
	fzf


# Install hyprshot for screenshots
if [[ ! -f /usr/bin/hyprshot ]]; then
	sudo curl https://raw.githubusercontent.com/Gustash/Hyprshot/refs/heads/main/hyprshot > ~/hyprshot
	sudo mv ~/hyprshot /usr/bin/hyprshot
	sudo chmod +x /usr/bin/hyprshot
fi



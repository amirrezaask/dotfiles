sudo add-apt-repository -y ppa:cppiber/hyprland
sudo add-apt-repository -y ppa:longsleep/golang-backports
sudo apt update && sudo apt upgrade -y


# Install base packages
sudo apt install -y \
	git \
    curl \
    wget \
	btop \
	hyprland \
	hyprlock \
	hypridle \
	pulsemixer \
	mako-notifier \
	fish \
    alacritty \
	swaybg \
	waybar \
	wl-clipboard \
	wofi \
	swayosd \
	polkit-gnome \
	chromium \
	golang-go \
	mpv \
	fzf

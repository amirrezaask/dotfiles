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
	fzf

# Install hyprshot for screenshots
sudo curl https://raw.githubusercontent.com/Gustash/Hyprshot/refs/heads/main/hyprshot > ~/hyprshot
sudo mv ~/hyprshot /usr/bin/hyprshot
sudo chmod +x /usr/bin/hyprshot

# Install cargo
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# manage bluetooth
cargo install bluetui

# To let brightness control work
sudo usermod -a -G video $USER


# Install clipse which is a simple clipboard manager
go install github.com/savedra1/clipse@v1.1.0

# Install google-chrome
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb
sudo apt-get install -f

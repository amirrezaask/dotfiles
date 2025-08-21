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
	mako-notifier \
	fish \
    alacritty \
	swaybg \
	waybar \
	wl-clipboard \
	wofi \
	swayosd \
	polkit-gnome \
	golang-go

# Install hyprshot for screenshots
sudo curl https://raw.githubusercontent.com/Gustash/Hyprshot/refs/heads/main/hyprshot > /usr/bin/hyprshot
sudo chmod +x /usr/bin/hyprshot

# Install cargo
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y

# manage bluetooth
cargo install bluetui

# Install Google-Chrome
wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb -O /tmp/google-chrome-stable_current_amd64.deb
sudo apt install -y /tmp/google-chrome-stable_current_amd64.deb

# Install Ghostty
wget $(wget -qO- https://api.github.com/repos/mkasberg/ghostty-ubuntu/releases/latest | grep "browser_download_url.*deb" | cut -d\" -f4) -O ghostty.deb && sudo apt install -y ./ghostty.deb

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
	hypridle \
	swaylock \
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

# Let wm handle these events to have more control.
sudo tee /etc/systemd/logind.conf > /dev/null <<'EOF'
[Login]
HandlePowerKey=poweroff
HandleSuspendKey=ignore
HandleHibernateKey=ignore
HandleLidSwitch=ignore
HandleLidSwitchExternalPower=ignore
KillUserProcesses=no
IdleAction=ignore
EOF

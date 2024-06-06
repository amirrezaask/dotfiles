#!/usr/bin/env bash

echo "Installing packages"
sudo apt update -y
sudo apt install -y \
	git curl btop \
	docker.io \
	libu2f-udev \
	build-essential pkg-config autoconf \
	openfortivpn
  
echo "Installing Postman"
if command -v snap &>/dev/null
then
    sudo snap install --classic postman
else
    flatpak install flathub com.getpostman.Postman
fi

echo "Installing Skype"
if command -v snap &>/dev/null
then
    sudo snap install --classic skype
else
    flatpak install flathub com.skype.Client

fi
echo "Installing google-chrome"
cd ~/Downloads
wget -nc https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
sudo dpkg -i google-chrome-stable_current_amd64.deb
cd -

echo "Installing Code"
cd ~/Downloads
wget -nc -O vscode.deb -c 'https://code.visualstudio.com/sha/download?build=stable&os=linux-deb-x64'
sudo apt install ./vscode.deb

echo "Installing Go Compiler"
sudo add-apt-repository -y ppa:longsleep/golang-backports
sudo apt update
sudo apt install -y golang-go

# .local/bin
mkdir -p ~/.local/bin

# Setup git aliases
echo "Setting up git"
git config --global alias.co checkout
git config --global alias.br branch
git config --global alias.ci commit
git config --global alias.st status
git config --global pull.rebase true

# Setup Docker
echo "Add user to docker group (needs restart to become effective)"
sudo usermod -aG docker ${USER}

echo "Setting up Nekoray"
cd ~/Downloads
wget -O nekoray.deb -c "https://github.com/MatsuriDayo/nekoray/releases/download/3.26/nekoray-3.26-2023-12-09-debian-x64.deb"
sudo apt install -y ./nekoray.deb
cd -

echo "Installing Neovim"
sudo add-apt-repository ppa:neovim-ppa/unstable -y
sudo apt update
sudo apt install -y make gcc ripgrep unzip git xclip neovim

echo "Installing Fish"
sudo apt install -y fish
chsh -s $(which fish)

echo "Installing Alacritty"
sudo add-apt-repository ppa:aslatter/ppa -y
sudo apt update
sudo apt install -y alacritty

echo "Installing Zellij"
cd ~/Downloads
wget -O zellij.tar.gz -c 'https://github.com/zellij-org/zellij/releases/download/v0.40.1/zellij-x86_64-unknown-linux-musl.tar.gz'
tar -xf zellij.tar.gz
chmod +x ./zellij
mv zellij ~/.local/bin
cd ~

mkdir -p ~/w
cd ~/w
git clone --depth 1 https://github.com/amirrezaask/dotfiles 
make -f ~/w/dotfiles/Makefile install

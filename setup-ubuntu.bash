#! /usr/bin/env bash

sudo add-apt-repository ppa:longsleep/golang-backports
sudo apt update && sudo apt upgrade
sudo apt install -y git curl apt-transport-https wget gpg

# Installing VSCode
wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > packages.microsoft.gpg
sudo install -D -o root -g root -m 644 packages.microsoft.gpg /etc/apt/keyrings/packages.microsoft.gpg
sudo sh -c 'echo "deb [arch=amd64,arm64,armhf signed-by=/etc/apt/keyrings/packages.microsoft.gpg] https://packages.microsoft.com/repos/code stable main" > /etc/apt/sources.list.d/vscode.list'
sudo rm -f packages.microsoft.gpg

# Installing nekoray
wget -c 'https://github.com/MatsuriDayo/nekoray/releases/download/3.26/nekoray-3.26-2023-12-09-debian-x64.deb' -O $HOME/Downloads/nekoray.deb
sudo dpkg -i $HOME/Downloads/nekoray.deb

# Add Golang repository
sudo apt install -y golang-go code zsh gettext cmake build-essential

# Clone dotfiles
mkdir $HOME/w
git clone --recurse-submodules git@github.com:amirrezaask/dotfiles.git $HOME/w/dotfiles
ln -s $HOME/w/dotfiles/nvim $HOME/.config/nvim

# Clone neovim
git clone --depth 1 git@github.com:neovim/neovim $HOME/w/neovim
cd $HOME/w/neovim && make -j8 CMAKE_BUILD_TYPE=Release && sudo make install

# Install brave
sudo curl -fsSLo /usr/share/keyrings/brave-browser-archive-keyring.gpg https://brave-browser-apt-release.s3.brave.com/brave-browser-archive-keyring.gpg

echo "deb [signed-by=/usr/share/keyrings/brave-browser-archive-keyring.gpg] https://brave-browser-apt-release.s3.brave.com/ stable main"|sudo tee /etc/apt/sources.list.d/brave-browser-release.list

sudo apt update
sudo apt install -y brave-browser
# Install wezterm
curl -fsSL https://apt.fury.io/wez/gpg.key | sudo gpg --yes --dearmor -o /usr/share/keyrings/wezterm-fury.gpg
echo 'deb [signed-by=/usr/share/keyrings/wezterm-fury.gpg] https://apt.fury.io/wez/ * *' | sudo tee /etc/apt/sources.list.d/wezterm.list

sudo apt update && apt install -y wezterm

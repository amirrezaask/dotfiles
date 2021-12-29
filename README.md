# Amirreza Configurations

# My Dev Setup
- Linux/WSL2
- Shell: ZSH ( sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" )
- Editor Acme


# How to setup a new system
1. Install plan9port => `sudo git clone https://github.com/9fans/plan9port /usr/local/plan9 && sudo apt install xorg-dev && /usr/local/plan9/INSTALL`
2. create correct dir for src `mkdir -p ~/src/github.com/amirrezaask/`
3. clone dotfiles `cd ~/src/github.com/amirrezaask && git clone https://github.com/amirrezaask/dotfiles`
4. `rm -rf ~/.profile && ln -s ~/src/github.com/amirrezaask/dotfiles/.profile ~/.profile`
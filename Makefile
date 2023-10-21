THIS_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
UNAME := $(shell uname)

all: profile zsh neovim alacritty kitty tmux emacs git

git:
	rm -rf $(HOME)/.gitconfig
	ln -s $(THIS_DIR)/.gitconfig $(HOME)/.gitconfig

codium:
	mkdir -p $(HOME)/.config/VSCodium/User
	rm -rf $(HOME)/.config/VSCodium/User/settings.json
	rm -rf $(HOME)/.config/VSCodium/User/keybindings.json
	ln -s $(THIS_DIR)/vscode-settings.json $(HOME)/.config/VSCodium/User/settings.json
	ln -s $(THIS_DIR)/vscode-keybindings.json $(HOME)/.config/VSCodium/User/keybindings.json
	echo "Installing/Upgrading extensions"
	codium --install-extension felipecaputo.git-project-manager
	codium --install-extension ms-azuretools.vscode-docker 
	codium --install-extension usernamehw.errorlens 
	codium --install-extension waderyan.gitblame 
	codium --install-extension GitLab.gitlab-workflow 
	codium --install-extension qcz.text-power-tools 
	codium --install-extension golang.go 
	codium --install-extension ziglang.vscode-zig 
	codium --install-extension rust-lang.rust-analyzer 
	
emacs:
	rm -rf $(HOME)/.emacs $(HOME)/.emacs.d/init.el
	ln -s $(THIS_DIR)/.emacs $(HOME)/.emacs

profile:
	rm -rf $(HOME)/.profile
	ln -s $(THIS_DIR)/.profile $(HOME)/.profile

zsh:
	rm -rf $(HOME)/.zshrc
	ln -s $(THIS_DIR)/.zshrc $(HOME)/.zshrc

neovim:
	rm -rf $(HOME)/.config/nvim
	mkdir -p $(HOME)/.config/nvim
	ln -s $(THIS_DIR)/init.lua $(HOME)/.config/nvim/init.lua

alacritty:
	rm -rf $(HOME)/.config/alacritty/ $(HOME)/.config/alacritty.yml
	ln -s $(THIS_DIR)/alacritty.yml $(HOME)/.config/alacritty.yml

kitty:
	rm -rf $(HOME)/.config/kitty
	mkdir -p $(HOME)/.config/kitty
	ln -s $(THIS_DIR)/kitty.conf $(HOME)/.config/kitty/kitty.conf

tmux:
	rm -rf $(HOME)/.tmux.conf
	ln -s $(THIS_DIR)/.tmux.conf $(HOME)/.tmux.conf

gopls:
	go install golang.org/x/tools/gopls@latest

rust-analyzer-linux:
	mkdir -p ~/.local/bin
	curl -L https://github.com/rust-lang/rust-analyzer/releases/latest/download/rust-analyzer-x86_64-unknown-linux-gnu.gz | gunzip -c - > ~/.local/bin/rust-analyzer
	chmod +x ~/.local/bin/rust-analyzer

linux:
	gopls
	rust-analyzer-linux

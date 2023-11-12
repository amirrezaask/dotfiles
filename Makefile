THIS_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
UNAME := $(shell uname)

all: profile zsh neovim alacritty kitty tmux emacs git

fish:
	rm -rf $(HOME)/.config/fish
	mkdir -p $(HOME)/.config/fish
	ln -s $(THIS_DIR)/config.fish $(HOME)/.config/fish/config.fish

profile:
	rm -rf $(HOME)/.profile
	ln -s $(THIS_DIR)/.profile $(HOME)/.profile
wezterm:
	rm -rf $(HOME)/.wezterm.lua
	ln -s $(THIS_DIR)/wezterm.lua $(HOME)/.wezterm.lua

emacs:
	rm -rf $(HOME)/.emacs
	ln -s $(THIS_DIR)/.emacs $(HOME)/.emacs

zsh:
	rm -rf $(HOME)/.zshrc
	ln -s $(THIS_DIR)/.zshrc $(HOME)/.zshrc

neovim:
	rm -rf $(HOME)/.config/nvim
	ln -s $(THIS_DIR)/nvim $(HOME)/.config/nvim

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

wezterm:
	rm -rf $(HOME)/.wezterm.lua
	ln -s $(THIS_DIR)/wezterm.lua $(HOME)/.wezterm.lua

awesome:
	mkdir -p $(HOME)/.config/awesome
	rm -rf $(HOME)/.config/awesome/rc.lua
	ln -s $(THIS_DIR)/awesome-rc.lua $(HOME)/.config/awesome/rc.lua

codium:
	rm -rf $(HOME)/.config/VSCodium/User/settings.json
	rm -rf $(HOME)/.config/VSCodium/User/keybindings.json
	ln -s $(THIS_DIR)/vscode-settings.json $(HOME)/.config/VSCodium/User/settings.json
	ln -s $(THIS_DIR)/vscode-keybindings.json $(HOME)/.config/VSCodium/User/keybindings.json

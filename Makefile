THIS_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
UNAME := $(shell uname)

all: profile zsh neovim kitty tmux emacs 

fish:
	rm -rf $(HOME)/.config/fish
	ln -s $(THIS_DIR)/fish $(HOME)/.config/fish

profile:
	rm -rf $(HOME)/.profile
	ln -s $(THIS_DIR)/.profile $(HOME)/.profile

wezterm:
	rm -rf $(HOME)/.wezterm.lua
	rm -rf $(HOME)/.config/wezterm
	ln -s $(THIS_DIR)/wezterm/ $(HOME)/.config/wezterm

emacs:
	rm -rf $(HOME)/.emacs
	ln -s $(THIS_DIR)/.emacs $(HOME)/.emacs

zsh:
	rm -rf $(HOME)/.zshrc
	ln -s $(THIS_DIR)/zsh/.zshrc $(HOME)/.zshrc

neovim:
	rm -rf $(HOME)/.config/nvim
	ln -s $(THIS_DIR)/nvim $(HOME)/.config/nvim

kitty:
	rm -rf $(HOME)/.config/kitty
	ln -s $(THIS_DIR)/kitty $(HOME)/.config/kitty

tmux:
	rm -rf $(HOME)/.tmux.conf
	ln -s $(THIS_DIR)/.tmux.conf $(HOME)/.tmux.conf

awesome:
	mkdir -p $(HOME)/.config/awesome
	rm -rf $(HOME)/.config/awesome/rc.lua
	ln -s $(THIS_DIR)/awesome-rc.lua $(HOME)/.config/awesome/rc.lua

codium:
	rm -rf $(HOME)/.config/VSCodium/User/settings.json
	rm -rf $(HOME)/.config/VSCodium/User/keybindings.json
	ln -s $(THIS_DIR)/codium/vscode-settings.json $(HOME)/.config/VSCodium/User/settings.json
	ln -s $(THIS_DIR)/codium/vscode-keybindings.json $(HOME)/.config/VSCodium/User/keybindings.json

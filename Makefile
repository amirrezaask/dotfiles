THIS_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
UNAME := $(shell uname)

all: profile zsh neovim alacritty kitty tmux emacs git

profile:
	rm -rf $(HOME)/.profile
	ln -s $(THIS_DIR)/.profile $(HOME)/.profile

emacs:
	rm -rf $(HOME)/.emacs
	ln -s $(THIS_DIR)/.emacs $(HOME)/.emacs

zsh:
	rm -rf $(HOME)/.zshrc
	ln -s $(THIS_DIR)/.zshrc $(HOME)/.zshrc

neovim:
	rm -rf $(HOME)/.config/nvim
	mkdir -p $(HOME)/.config/nvim
	ln -s $(THIS_DIR)/nvim-init.lua $(HOME)/.config/nvim/init.lua

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

DOTFILES_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG := $(HOME)/.config

install:
	mkdir -p $(XDG_CONFIG)
	rm -rf $(XDG_CONFIG)/fish
	rm -rf $(XDG_CONFIG)/ghostty
	rm -rf $(HOME)/.zshrc
	rm -rf $(HOME)/.gitconfig
	rm -rf $(XDG_CONFIG)/starship.toml
	rm -rf $(HOME)/.emacs
	mkdir -p $(XDG_CONFIG)/fish
	mkdir -p $(XDG_CONFIG)/ghostty
	ln -s $(DOTFILES_DIR)/fish-config.fish $(XDG_CONFIG)/fish/config.fish
	ln -s $(DOTFILES_DIR)/ghostty-config $(XDG_CONFIG)/ghostty/ghostty.conf
	ln -s $(DOTFILES_DIR)/starship.toml $(XDG_CONFIG)/starship.toml
	ln -s $(DOTFILES_DIR)/zshrc $(HOME)/.zshrc
	ln -s $(DOTFILES_DIR)/emacs-init.el $(HOME)/.emacs
	ln -s $(DOTFILES_DIR)/.gitconfig $(HOME)/.gitconfig

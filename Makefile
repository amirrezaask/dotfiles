DOTFILES_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG := $(HOME)/.config

install:
	mkdir -p $(XDG_CONFIG)
	rm -rf $(XDG_CONFIG)/i3
	rm -rf $(XDG_CONFIG)/alacritty
	rm -rf $(XDG_CONFIG)/nvim
	rm -rf $(XDG_CONFIG)/fish
	rm -rf $(HOME)/.zshrc
	rm -rf $(HOME)/.tmux.conf
	rm -rf $(HOME)/.emacs
	rm -rf $(XDG_CONFIG)/wezterm
	mkdir -p $(XDG_CONFIG)/i3
	mkdir -p $(XDG_CONFIG)/alacritty
	mkdir -p $(XDG_CONFIG)/nvim
	mkdir -p $(XDG_CONFIG)/fish
	mkdir -p $(XDG_CONFIG)/wezterm
	ln -s $(DOTFILES_DIR)/i3-config $(XDG_CONFIG)/i3/config
	ln -s $(DOTFILES_DIR)/alacritty.toml $(XDG_CONFIG)/alacritty/alacritty.toml
	ln -s $(DOTFILES_DIR)/config.fish $(XDG_CONFIG)/fish/config.fish
	ln -s $(DOTFILES_DIR)/init.lua $(XDG_CONFIG)/nvim/init.lua
	ln -s $(DOTFILES_DIR)/.emacs $(HOME)/.emacs
	ln -s $(DOTFILES_DIR)/.zshrc $(HOME)/.zshrc
	ln -s $(DOTFILES_DIR)/wezterm.lua $(XDG_CONFIG)/wezterm/wezterm.lua

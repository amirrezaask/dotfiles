DOTFILES_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG := $(HOME)/.config

install:
	mkdir -p $(XDG_CONFIG)
	rm -rf $(XDG_CONFIG)/nvim
	rm -rf $(XDG_CONFIG)/fish
	rm -rf $(XDG_CONFIG)/ghostty
	rm -rf $(HOME)/.zshrc
	rm -rf $(HOME)/.gitconfig
	rm -rf $(HOME)/.tmux.conf
	rm -rf $(XDG_CONFIG)/starship.toml
	rm -rf $(XDG_CONFIG)/zellij
	rm -rf $(HOME)/.emacs.d
	rm -rf $(HOME)/.profile
	ln -s $(DOTFILES_DIR)/shell/fish $(XDG_CONFIG)/fish
	ln -s $(DOTFILES_DIR)/terminal/ghostty $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/mux/tmux/tmux.conf $(HOME)/.tmux.conf
	ln -s $(DOTFILES_DIR)/shell/starship/starship.toml $(XDG_CONFIG)/starship.toml
	ln -s $(DOTFILES_DIR)/editor/nvim $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/shell/zsh/zshrc $(HOME)/.zshrc
	ln -s $(DOTFILES_DIR)/editor/emacs $(HOME)/.emacs.d
	ln -s $(DOTFILES_DIR)/shell/profile $(HOME)/.profile
	ln -s $(DOTFILES_DIR)/.gitconfig $(HOME)/.gitconfig

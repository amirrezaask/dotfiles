DOTFILES_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG := $(HOME)/.config

install:
	mkdir -p $(XDG_CONFIG)
	rm -rf $(XDG_CONFIG)/i3
	rm -rf $(XDG_CONFIG)/alacritty
	rm -rf $(XDG_CONFIG)/nvim
	rm -rf $(XDG_CONFIG)/fish
	rm -rf $(XDG_CONFIG)/kitty
	rm -rf $(HOME)/.zshrc
	rm -rf $(HOME)/.tmux.conf
	rm -rf $(XDG_CONFIG)/wezterm
	rm -rf $(XDG_CONFIG)/starship.toml
	rm -rf $(HOME)/go/bin/tmux-windowizer
	rm -rf $(HOME)/go/bin/tmux-sessionizer
	rm -rf $(HOME)/go/bin/tmux-switch
	rm -rf $(XDG_CONFIG)/zellij
	rm -rf $(XDG_CONFIG)/wezterm
	rm -rf $(HOME)/.emacs.d
	rm -rf $(HOME)/.profile
	mkdir -p $(XDG_CONFIG)/i3
	mkdir -p $(HOME)/go/bin
	ln -s $(DOTFILES_DIR)/i3-config $(XDG_CONFIG)/i3/config
	ln -s $(DOTFILES_DIR)/fish $(XDG_CONFIG)/fish
	ln -s $(DOTFILES_DIR)/wezterm $(XDG_CONFIG)/wezterm
	ln -s $(DOTFILES_DIR)/kitty $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/alacritty $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/tmux/tmux.conf $(HOME)/.tmux.conf
	ln -s $(DOTFILES_DIR)/starship.toml $(XDG_CONFIG)/starship.toml
	ln -s $(DOTFILES_DIR)/zellij $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/nvim $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/zsh/zshrc $(HOME)/.zshrc
	ln -s $(DOTFILES_DIR)/emacs $(HOME)/.emacs.d
	ln -s $(DOTFILES_DIR)/.profile $(HOME)/.profile

install-sublime-mac:
	rm -rf "$(HOME)/Library/Application Support/Sublime Text/Packages"
	ln -s "$(DOTFILES_DIR)/Sublime" "$(HOME)/Library/Application Support/Sublime Text/Packages"

sync-vscode-macos:
	cp "$(HOME)/Library/Application Support/Code/User/settings.json" ./vscode/settings.json
	cp "$(HOME)/Library/Application Support/Code/User/keybindings.json" ./vscode/keybindings.json

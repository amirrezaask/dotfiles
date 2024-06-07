DOTFILES_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG := $(HOME)/.config

install:
	mkdir -p $(XDG_CONFIG)
	rm -rf $(XDG_CONFIG)/i3
	rm -rf $(XDG_CONFIG)/alacritty
	rm -rf $(XDG_CONFIG)/nvim
	rm -rf $(XDG_CONFIG)/zellij
	rm -rf $(XDG_CONFIG)/fish
	rm -rf $(XDG_CONFIG)/kitty
	rm -rf $(XDG_CONFIG)/Code/User/settings.json
	rm -rf $(XDG_CONFIG)/Code/User/keybindings.json
	ln -s $(DOTFILES_DIR)/i3 $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/alacritty $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/nvim $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/zellij $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/fish $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/kitty $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/vscode/settings.json $(XDG_CONFIG)/Code/User/settings.json
	ln -s $(DOTFILES_DIR)/vscode/keybindings.json $(XDG_CONFIG)/Code/User/keybindings.json
	$(DOTFILES_DIR)/vscode/extensions.sh

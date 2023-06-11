THIS_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))

shell:
	rm -rf $(HOME)/.profile && ln -s $(THIS_DIR)/.profile $(HOME)/.profile

zsh: shell
	rm -rf $(HOME)/.zshrc && ln -s $(THIS_DIR)/.zshrc $(HOME)/.zshrc

bash: shell
	rm -rf $(HOME)/.bashrc && ln -s $(THIS_DIR)/.bashrc $(HOME)/.bashrc

neovim:
	rm -rf $(HOME)/.config/nvim/ && mkdir -p $(HOME)/.config/nvim && ln -s $(THIS_DIR)/init.lua $(HOME)/.config/nvim/init.lua

emacs:
	ln -s $(THIS_DIR)/.emacs $(HOME)/.emacs

alacritty:
	rm -rf $(HOME)/.config/alacritty/ $(HOME)/.config/alacritty.yml && mkdir -p $(HOME)/.config/alacritty && ln -s $(THIS_DIR)/alacritty.yml $(HOME)/.config/alacritty/alacritty.yml

kitty:
	rm -rf $(HOME)/.config/kitty && mkdir -p $(HOME)/.config/kitty && ln -s $(THIS_DIR)/kitty.conf $(HOME)/.config/kitty/kitty.conf

zellij:
	rm -rf $(HOME)/.config/zellij && mkdir -p $(HOME)/.config/zellij && ln -s $(THIS_DIR)/config.kdl $(HOME)/.config/zellij/config.kdl

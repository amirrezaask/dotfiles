THIS_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
UNAME := $(shell uname)

all: zsh neovim alacritty kitty tmux vscode

zsh:
	rm -rf $(HOME)/.zshrc
	ln -s $(THIS_DIR)/.zshrc $(HOME)/.zshrc

neovim:
	rm -rf $(HOME)/.config/nvim
	ln -s $(THIS_DIR)/nvim/ $(HOME)/.config/nvim

alacritty:
	rm -rf $(HOME)/.config/alacritty/ $(HOME)/.config/alacritty.yml
	ln -s $(THIS_DIR)/alacritty $(HOME)/.config/alacritty

kitty:
	rm -rf $(HOME)/.config/kitty
	ln -s $(THIS_DIR)/kitty $(HOME)/.config/kitty

tmux:
	rm -rf $(HOME)/.tmux.conf
	ln -s $(THIS_DIR)/.tmux.conf $(HOME)/.tmux.conf

vscode:
ifeq ($(UNAME), Linux)
	rm -rf $(HOME)/.config/Code/User/keybindings.json
	ln -s $(THIS_DIR)/vscode/keybindings.json $(HOME)/.config/Code/User/keybindings.json

	rm -rf $(HOME)/.config/Code/User/settings.json
	ln -s $(THIS_DIR)/vscode/settings.json $(HOME)/.config/Code/User/settings.json
endif

ifeq ($(UNAME), Darwin)
	rm -rf "$(HOME)/Library/Application Support/Code/User/keybindings.json"
	ln -s $(THIS_DIR)/vscode/keybindings.json "$(HOME)/Library/Application Support/Code/User/keybindings.json"
	rm -rf "$(HOME)/Library/Application Support/Code/User/settings.json"
	ln -s $(THIS_DIR)/vscode/settings.json "$(HOME)/Library/Application Support/Code/User/settings.json"
endif


sublime:
ifeq ($(UNAME), Darwin)
	rm -rf "$(HOME)/Library/Application Support/Sublime Text/Packages/User"
	ln -s "$(THIS_DIR)/sublimetext" "$(HOME)/Library/Application Support/Sublime Text/Packages/User"
endif

ifeq ($(UNAME), Linux)
	rm -rf "$(HOME)/.config/sublime-text/Packages/User"
	ln -s "$(THIS_DIR)/sublimetext" "$(HOME)/.config/sublime-text/Packages/User"
endif

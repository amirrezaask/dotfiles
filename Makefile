DOTFILES_DIR := $(dir $(abspath $(firstword $(MAKEFILE_LIST))))
XDG_CONFIG := $(HOME)/.config
NEOVIM_RELEASE="master"

provision:
	echo "Installing homebrew..."
	@if ! command -v brew &> /dev/null; then \
		/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"; \
	fi
	brew bundle install $(DOTFILES_DIR)/Brewfile

build-install-neovim:
	if ! test -d $(HOME)/.neovim; then \
		git clone https://github.com/neovim/neovim.git $(HOME)/.neovim; \
	fi;
	cd $(HOME)/.neovim && git pull && git checkout $(NEOVIM_RELEASE) && make CMAKE_BUILD_TYPE=Release && sudo make install

configure:
	mkdir -p $(XDG_CONFIG)
	rm -rf $(XDG_CONFIG)/fish
	rm -rf $(XDG_CONFIG)/ghostty
	rm -rf $(HOME)/.zshrc
	rm -rf $(HOME)/.gitconfig
	rm -rf $(XDG_CONFIG)/starship.toml
	rm -rf $(HOME)/.emacs
	rm -rf $(XDG_CONFIG)/nvim
	mkdir -p $(XDG_CONFIG)/fish
	mkdir -p $(XDG_CONFIG)/ghostty
	mkdir -p $(XDG_CONFIG)/nvim
	ln -s $(DOTFILES_DIR)/fish-config.fish $(XDG_CONFIG)/fish/config.fish
	ln -s $(DOTFILES_DIR)/ghostty-config $(XDG_CONFIG)/ghostty/config
	ln -s $(DOTFILES_DIR)/starship.toml $(XDG_CONFIG)/starship.toml
	ln -s $(DOTFILES_DIR)/zshrc $(HOME)/.zshrc
	ln -s $(DOTFILES_DIR)/emacs-init.el $(HOME)/.emacs
	ln -s $(DOTFILES_DIR)/.gitconfig $(HOME)/.gitconfig
	ln -s $(DOTFILES_DIR)/nvim-init.lua $(XDG_CONFIG)/nvim/init.lua
	if test -d "$(HOME)/Library/Application Support/Cursor"; then \
		rm -rf "$(HOME)/Library/Application Support/Cursor/User/keybindings.json"; \
		rm -rf "$(HOME)/Library/Application Support/Cursor/User/settings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode-keybindings.json" "$(HOME)/Library/Application Support/Cursor/User/keybindings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode-settings.json" "$(HOME)/Library/Application Support/Cursor/User/settings.json"; \
	fi
	if test -d "$(HOME)/Library/Application Support/Code"; then \
		rm -rf "$(HOME)/Library/Application Support/Code/User/keybindings.json"; \
		rm -rf "$(HOME)/Library/Application Support/Code/User/settings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode-keybindings.json" "$(HOME)/Library/Application Support/Code/User/keybindings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode-settings.json" "$(HOME)/Library/Application Support/Code/User/settings.json"; \
	fi; \

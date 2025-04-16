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
	cd $(HOME)/.neovim && git pull && git checkout $(NEOVIM_RELEASE) && make distclean && make clean && make CMAKE_BUILD_TYPE=Release && sudo make install

configure:
	mkdir -p $(XDG_CONFIG)
	rm -rf $(XDG_CONFIG)/fish
	rm -rf $(XDG_CONFIG)/ghostty
	rm -rf $(HOME)/.zshrc
	rm -rf $(HOME)/.gitconfig
	rm -rf $(XDG_CONFIG)/starship
	rm -rf $(XDG_CONFIG)/emacs
	rm -rf $(XDG_CONFIG)/nvim
	rm -rf $(XDG_CONFIG)/wezterm
	ln -s $(DOTFILES_DIR)/fish/ $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/ghostty/ $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/starship/ $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/zsh/zshrc $(HOME)/.zshrc
	ln -s $(DOTFILES_DIR)/emacs $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/.gitconfig $(HOME)/.gitconfig
	ln -s $(DOTFILES_DIR)/nvim $(XDG_CONFIG)/
	ln -s $(DOTFILES_DIR)/wezterm/ $(XDG_CONFIG)
	if test -d "$(HOME)/Library/Application Support/Cursor"; then \
		rm -rf "$(HOME)/Library/Application Support/Cursor/User/keybindings.json"; \
		rm -rf "$(HOME)/Library/Application Support/Cursor/User/settings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode/keybindings.json" "$(HOME)/Library/Application Support/Cursor/User/keybindings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode/settings.json" "$(HOME)/Library/Application Support/Cursor/User/settings.json"; \
	fi
	if test -d "$(HOME)/Library/Application Support/Code"; then \
		rm -rf "$(HOME)/Library/Application Support/Code/User/keybindings.json"; \
		rm -rf "$(HOME)/Library/Application Support/Code/User/settings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode/keybindings.json" "$(HOME)/Library/Application Support/Code/User/keybindings.json"; \
		ln -s "$(DOTFILES_DIR)/vscode/settings.json" "$(HOME)/Library/Application Support/Code/User/settings.json"; \
	fi; 
	code --install-extension bmewburn.vscode-intelephense-client \
	code --install-extension golang.go \
	code --install-extension ms-vscode-remote.remote-wsl \
	code --install-extension raunofreiberg.vesper \
	code --install-extension redhat.vscode-yaml \
	code --install-extension rust-lang.rust-analyzer \
	code --install-extension supermaven.supermaven \
	code --install-extension usernamehw.errorlens \
	code --install-extension vscodevim.vim \
	code --install-extension z4yross.anysphere-dark \

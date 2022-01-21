install:
	@sudo apt install kitty gettext clang cmake libtool-bin ripgrep feh
	@mkdir -p ~/tmp
	@git clone git@github.com:neovim/neovim.git ~/tmp/neovim
	@cd ~/tmp/neovim && make && sudo make install
	@mkdir -p ~/.local/share/nvim/site/pack/packer/start
	@git clone https://github.com/wbthomason/packer.nvim/ ~/.local/share/nvim/site/pack/packer/start/packer.nvim
	@echo "export WALLPAPERS_PATH=$(pwd)/wallpapers" > ~/.profile

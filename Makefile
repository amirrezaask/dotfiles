install:
	@echo "Make sure you have 'neovim >0.5', 'ripgrep' installed
	@git clone https://github.com/wbthomason/packer.nvim/ ~/.local/share/nvim/site/pack/packer/start/packer.nvim
	cp -r nvim ~/.config/
	@nvim +PackerInstall

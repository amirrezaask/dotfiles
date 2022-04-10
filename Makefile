install-vim-config:
	./.scripts/install_vim_config.sh

install-kitty-config:
	@ln -s $(pwd)/kitty ~/.config/kitty

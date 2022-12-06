install:
	cp -r ~/.config/nvim ~/.config/nvim_back
	rm -rf ~/.config/nvim
	ln -s nvim ~/.config/nvim

	cp -r ~/.config/zsh ~/.config/zsh_back
	rm -rf ~/.config/zsh
	ln -s zsh ~/.config/zsh

	cp -r ~/.zshrc ~/.zshrc_back
	rm -rf ~/.zshrc
	ln -s ~/.config/zsh/.zshrc ~/.zshrc

	cp -r ~/.config/kitty ~/.config/kitty_back
	rm -rf ~/.config/kitty
	ln -s kitty ~/.config/kitty

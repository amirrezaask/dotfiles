source $HOME/.profile

source $DOTFILES/zsh/antigen.zsh

antigen use oh-my-zsh

antigen bundle git

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

command -v 'starship' > /dev/null

if [ "$?" != '0' ]; then
    sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi
antigen apply
# eval "$(starship init zsh)"


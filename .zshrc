source ~/.profile
[ ! -d "$HOME/.oh-my-zsh" ] && git clone https://github.com/ohmyzsh/ohmyzsh.git --single-branch --depth 1 .oh-my-zsh

alias reload='source ~/.zshrc'
export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"

plugins=()

source $ZSH/oh-my-zsh.sh

if ! command -v starship &> /dev/null
then
    curl -sS https://starship.rs/install.sh | sh
fi

eval "$(starship init zsh)"


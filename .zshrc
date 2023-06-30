source $HOME/.profile

# Oh My ZSH
[ ! -d "$HOME/.oh-my-zsh" ] && git clone https://github.com/ohmyzsh/ohmyzsh.git --single-branch --depth 1 .oh-my-zsh

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"

plugins=(
    git
    fzf
)

source $ZSH/oh-my-zsh.sh

eval "$(starship init zsh)"

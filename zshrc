export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
alias svpn="sudo openfortivpn -c $HOME/snappdc.conf"
alias vim='nvim'
[ -f "$HOME/cargo/env" ] && . "$HOME/.cargo/env"

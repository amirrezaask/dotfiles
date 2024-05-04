if ! test -d $HOME/.oh-my-zsh 
then
    git clone https://github.com/ohmyzsh/ohmyzsh.git ~/.oh-my-zsh
fi
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

if command -v codium &> /dev/null
then
    alias code='codium'
fi

export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
alias svpn="sudo openfortivpn -c $HOME/snappdc.conf"
alias vim='nvim'
[ -f "$HOME/cargo/env" ] && . "$HOME/.cargo/env"

export PATH="/home/amirreza/jetbrains/DataGrip-2024.1.2/bin:$PATH"

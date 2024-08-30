if ! test -d $HOME/.oh-my-zsh 
then
    git clone https://github.com/ohmyzsh/ohmyzsh.git ~/.oh-my-zsh
fi
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

if command -v fzf &>/dev/null
then
    source <(fzf --zsh)
fi

if command -v nvim &>/dev/null
then
    alias vim='nvim'
    alias v='nvim'
fi

if command -v subl &>/dev/null
then
    export EDITOR='subl -w'
    export GIT_EDITOR='subl -w'
    alias s='subl .'
fi

if command -v brew &>/dev/null
then
    eval $(brew shellenv)
fi

export NEOVIDE_FORK=1
export NEOVIDE_TABS=0
export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
alias svpn='sudo openfortivpn  --otp $(totpgen ADS)'
[ -f "$HOME/cargo/env" ] && . "$HOME/.cargo/env"

oclogs() {
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
}

export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"


if command -v starship &>/dev/null
then
    eval "$(starship init zsh)"
fi

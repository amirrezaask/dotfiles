if ! test -d $HOME/.oh-my-zsh 
then
    git clone https://github.com/ohmyzsh/ohmyzsh.git ~/.oh-my-zsh
fi
export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=()

# Git stuff
alias gs='git status'
alias gst='git status'
alias gdd='git diff HEAD'
alias gg='git push origin $(git_current_branch)'

source $ZSH/oh-my-zsh.sh

if command -v fzf &>/dev/null
then
    source <(fzf --zsh)
fi

if command -v subl &>/dev/null
then
    alias s='subl'
    alias ss='subl .'
fi

if command -v code &>/dev/null
then
    alias c='code'
    alias cc='code .'
fi

if command -v brew &>/dev/null
then
    eval $(brew shellenv)
fi

if command -v nvim &>/dev/null
then
    alias vim='nvim'
    alias v='nvim'
    alias n='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
fi

export NEOVIDE_FORK=1
export NEOVIDE_TABS=0
export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:$HOME/prg/bin"
export PLAN9=/Users/amirrezaask/plan9 
export PATH=$PATH:$PLAN9/bin 
export DOTFILES="$HOME/w/dotfiles"
export PATH="$PATH:$DOTFILES/bin"

alias svpn='sudo openfortivpn  --otp $(totpgen ADS)'
[ -f "$HOME/cargo/env" ] && . "$HOME/.cargo/env"

oclogs() {
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
}

reload() {
    source ~/.zshrc
}


export GOPRIVATE=gitlab.snappcloud.io
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"


tns() {
    if [[ ! -z $1 ]]; then
        tmux new-session -d -s"$1"
        tmux switch-client -t"$1"
    fi
}

if command -v starship &>/dev/null
then
    eval "$(starship init zsh)"
fi

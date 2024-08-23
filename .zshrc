if ! test -d $HOME/.oh-my-zsh 
then
    git clone https://github.com/ohmyzsh/ohmyzsh.git ~/.oh-my-zsh
fi

if ! test -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
then
    git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions
fi

if ! test -d ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
then
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-syntax-highlighting
fi

if ! command -v starship &>/dev/null
then
   curl -sS https://starship.rs/install.sh | sh
fi

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"
# ZSH_THEME="powerlevel10k/powerlevel10k"

plugins=(git)

source $ZSH/oh-my-zsh.sh

if command -v fzf &>/dev/null
then
    source <(fzf --zsh)
fi

if command -v nvim &>/dev/null
then
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
    alias vim='nvim'
fi
if command -v neovide &>/dev/null
then
    alias nv='neovide'
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

eval "$(starship init zsh)"

# Nicer programs
if command -v eza &>/dev/null
then
    alias ls='eza'
fi


gdoc() {

    if [[ $# -eq 1 ]]; then
        selected=$1
    else
        selected=$(find ~/w -mindepth 1 -maxdepth 1 -type d | fzf)
    fi

    if [[ -z $selected ]]; then
        exit 0
    fi

    PORT=$2

    if [[ -z $PORT ]]; then
        PORT=$(python3 -c 'import socket; s=socket.socket(); s.bind(("", 0)); print(s.getsockname()[1]); s.close()');
    fi
    printf "Listening on http://localhost:%d\n" $PORT
    pushd $selected

    godoc -http=":$PORT"&
    
    popd
}


alias tabtitle='wezterm cli set-tab-title'

export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"

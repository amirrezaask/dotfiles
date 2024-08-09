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

export ZSH="$HOME/.oh-my-zsh"

ZSH_THEME="robbyrussell"

plugins=(git)

source $ZSH/oh-my-zsh.sh

if command -v fzf &>/dev/null
then
    source <(fzf --zsh)
fi

# if command -v emacs &>/dev/null # use emacs as editor
# then
#     export EDITOR='emacs -nw'
#     export GIT_EDITOR='emacs -nw'
# fi
if command -v nvim &>/dev/null
then
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
    alias vim='nvim'
fi

export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
alias svpn='sudo openfortivpn  --otp $(totpgen ADS)'
[ -f "$HOME/cargo/env" ] && . "$HOME/.cargo/env"

oclogs() {
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
}

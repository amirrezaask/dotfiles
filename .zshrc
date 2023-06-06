[ ! -d "$HOME/.oh-my-zsh" ] && git clone https://github.com/ohmyzsh/ohmyzsh.git --single-branch --depth 1 .oh-my-zsh

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="robbyrussell"

plugins=(
    git
    fzf
)

source $ZSH/oh-my-zsh.sh
[ -f "$HOME/snapp-shell" ] && source "$HOME/snapp-shell"
export GO111MODULE='on'
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='goproxy.io,direct'
export EDITOR='vim'

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="/Users/amirreza/Library/Python/3.10/bin:$PATH"
export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"

eval $(brew shellenv)

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export HOMEBREW_NO_AUTO_UPDATE=1

export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

reload() {
    source ~/.zshrc
}

tmux-open() {
    tmux new-session -A -t $(realpath $1) -c $(realpath $1)
}

alias o='tmux-open'
alias to='tmux-open'
alias tl='tmux list-sessions'
alias tks="tmux kill-server"
alias tk="tmux kill-session -t"
alias ta="tmux attach -t"

alias mabna-up='sudo ipsec up corp'
alias mabna-down='sudo ipsec down corp'

if command -v nvim &> /dev/null
then
    alias vim='nvim'
    export EDITOR='nvim'
fi

if ! command -v starship &> /dev/null
then
    curl -sS https://starship.rs/install.sh | sh
fi

# for git diff
if ! command -v delta &> /dev/null
then
    if command -v brew &> /dev/null
    then
        brew install git-delta
    fi
    if command -v apt &> /dev/null
    then
        sudo apt install git-delta
    fi
fi

eval "$(starship init zsh)"

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# opam configuration
[[ ! -r /Users/amirreza/.opam/opam-init/init.zsh ]] || source /Users/amirreza/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

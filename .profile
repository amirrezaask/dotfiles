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

if command -v brew &> /dev/null
then
    eval $(brew shellenv)
fi

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export HOMEBREW_NO_AUTO_UPDATE=1
export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

alias reload='source ~/.profile'
# Tmux
alias o='tmux-open'
alias to='tmux-open'
alias tl='tmux list-sessions'
alias tks="tmux kill-server"
alias tk="tmux kill-session -t"
alias ta="tmux attach -t"

# Mabna
alias mabna-up='sudo ipsec up corp'
alias mabna-down='sudo ipsec down corp'

if command -v nvim &> /dev/null
then
    alias vim='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
fi
export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
[[ ! -r /Users/amirreza/.opam/opam-init/init.zsh ]] || source /Users/amirreza/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null
export PS2=''

# Fig pre block. Keep at the top of this file.
. "$HOME/.fig/shell/zshrc.pre.zsh"
# Go related stuff
export GO111MODULE='on'
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'
export EDITOR='vim'
export OSS="$HOME/src/github.com/amirrezaask"
export DOTFILES="$HOME/src/github.com/amirrezaask/dotfiles"
export SNAPP="$HOME/src/gitlab.snapp.ir"
export ESPAD="$HOME/src/gitlab.espadev.ir"

export PATH="$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$DOTFILES/bin:$HOME/.composer/vendor/bin"

# Setup ghc and haskell stuff
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env

# Aliases
# alias open='xdg-open'
alias lock='i3lock -c000000'
alias reload='source ~/.zshrc'

alias freenet="echo ${VPN_PASSWORD} | sudo openconnect --no-dtls --passwd-on-stdin --user ${VPN_USERNAME} ${VPN_SERVER}"
alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'
alias baly='echo ${BALY_PASSWORD} | sudo openconnect --user amirreza.askarpour vpn-aws.snapp.ir:443'

alias gs='git status'

# Workspace stuff
alias dots="cd ${DOTFILES}"
alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake
alias zr='zig build run'
alias zt='zig build test'
[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

if command -v nvim &> /dev/null
then
    alias v='nvim'
    alias vi='nvim'
    alias vim='nvim'
    export EDITOR='nvim'
fi

if command -v subl &> /dev/null
then
    alias s='subl .'
fi

if command -v exa &> /dev/null
then
    alias ls='exa'
    alias ll='exa -la'
    alias l='exa -la'
fi



alias luamake=/Users/amirreza/.local/lua-language-server/3rd/luamake/luamake
[ -f "/Users/amirreza/.ghcup/env" ] && source "/Users/amirreza/.ghcup/env" # ghcup-env

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export HOMEBREW_NO_AUTO_UPDATE=1


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

# export IDE='code'
export IDE='nvim'

function work {
    PROJECT=$(find $LOOKUP_DIR/* -type d -maxdepth 0 | fzf)
    if [ "$PROJECT" = '' ]; then
        echo "No project selected"
        return
    fi
    if [[ "$IDE" = 'vim' || "$IDE" = 'nvim' ]]; then
        echo "IDE is vim"
        tmux new-session -c $PROJECT -s $(basename $PROJECT) -n "shell" -d
        tmux neww -n editor -c $PROJECT
        tmux attach-session -t $(basename $PROJECT)
    elif [ "$IDE" = 'code' ]; then
        echo "IDE is code"
        code $PROJECT
    fi
}

function snapp {
    LOOKUP_DIR="$HOME/src/gitlab.snapp.ir" work
}

function oss {
    LOOKUP_DIR="$HOME/src/github.com/amirrezaask" work
}

function golobby {
    LOOKUP_DIR="$HOME/src/github.com/golobby" work
}

alias tl='tmux ls'
alias ta='tmux attach -t'
alias tks='tmux kill-session -t'

alias ca='cargo'
alias car='cargo run'
alias cab='cargo build'

alias oss="cd $OSS"
alias dots="cd $DOTFILES"
alias snapp="cd $SNAPP"
alias golobby="cd $GOLOBBY"

# Install starship
command -v 'starship' > /dev/null

if [ "$?" != '0' ]; then
 sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi

source $DOTFILES/zsh/antigen.zsh
antigen bundle zsh-users/zsh-autosuggestions
antigen use oh-my-zsh
antigen bundle git
antigen apply

# using starship for prompt
eval "$(starship init zsh)"

# Fig post block. Keep at the bottom of this file.
. "$HOME/.fig/shell/zshrc.post.zsh"

if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi
source ~/env

# Go related stuff
export GO111MODULE='on'
# export BASEGOROOT='/usr/local/go'
# export GOVERSION='1.18beta1'
# export GOVERSION='1.17.5'
#export GOROOT="${BASEGOROOT}-${GOVERSION}"
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'

export ELIXIR="/usr/local/elixir"

export EDITOR='vim'

export PLAN9="$HOME/plan9"

export OSS="$HOME/src/github.com/amirrezaask"
export DOTFILES="$HOME/src/github.com/amirrezaask/dotfiles"
export SNAPP="$HOME/src/gitlab.snapp.ir"
export ESPAD="$HOME/src/gitlab.espadev.ir"

export PATH="/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$PLAN9/bin"

# Aliases
# alias open='xdg-open'
alias lock='i3lock -c000000'
alias reload='source ~/.zshrc'

alias freenet="echo ${VPN_PASSWORD} | sudo openconnect --no-dtls --passwd-on-stdin --user ${VPN_USERNAME} ${VPN_SERVER}"
alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'
alias baly='echo ${BALY_PASSWORD} | sudo openconnect --user amirreza.askarpour vpn-aws.snapp.ir:443'

alias gs='git status'

# Workspace stuff
alias oss='cd ~/src/github.com/amirrezaask'
alias golobby='cd ~/src/github.com/golobby'
alias snapp="cd $SNAPP"
alias espad="cd $ESPAD"
alias dots="cd ${DOTFILES}"
alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake
[ -f "/home/amirreza/.ghcup/env" ] && source "/home/amirreza/.ghcup/env" # ghcup-env
if command -v nvim &> /dev/null
then
    alias v=nvim
    alias vim=nvim
    alias n=nvim
fi
. "$HOME/.cargo/env"

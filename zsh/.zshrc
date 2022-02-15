source $DOTFILES/zsh/antigen.zsh

antigen use oh-my-zsh

antigen bundle git

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

command -v 'starship' > /dev/null

if [ "$?" != '0' ]; then
    sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi
antigen theme robbyrussell
antigen apply
# eval "$(starship init zsh)"

alias luamake=/Users/amirreza/.local/lua-language-server/3rd/luamake/luamake

# Env stuff
[ -f "$HOME/env" ] && source ~/env

# Go related stuff
export GO111MODULE='on'
# export BASEGOROOT='/usr/local/go'
# export GOVERSION='1.18beta1'
# export GOVERSION='1.17.5'
# export GOROOT="${BASEGOROOT}-${GOVERSION}"
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
alias oss='cd ~/src/github.com/amirrezaask'
alias golobby='cd ~/src/github.com/golobby'
alias snapp="cd $SNAPP"
alias espad="cd $ESPAD"
alias dots="cd ${DOTFILES}"
alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake
[ -f "/home/amirreza/.ghcup/env" ] && source "/home/amirreza/.ghcup/env" # ghcup-env
. "$HOME/.cargo/env"

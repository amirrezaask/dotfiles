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
alias oss='cd ~/src/github.com/amirrezaask'
alias golobby='cd ~/src/github.com/golobby'
alias snapp="cd $SNAPP"
alias espad="cd $ESPAD"
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



source $DOTFILES/zsh/antigen.zsh

antigen use oh-my-zsh

antigen bundle git
antigen theme robbyrussell
antigen apply

# Install starship
command -v 'starship' > /dev/null

if [ "$?" != '0' ]; then
 sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi

alias luamake=/Users/amirreza/.local/lua-language-server/3rd/luamake/luamake
[ -f "/Users/amirreza/.ghcup/env" ] && source "/Users/amirreza/.ghcup/env" # ghcup-env

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"
export HOMEBREW_NO_AUTO_UPDATE=1


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'


eval "$(starship init zsh)"

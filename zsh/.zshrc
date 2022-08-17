if [ ! -f "$HOME/.antigen.zsh" ]; then
    curl -L git.io/antigen > .antigen.zsh
fi

source $HOME/.antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

antigen theme robbyrussell 

antigen apply

# Go related stuff
export GO111MODULE='on'
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'
export EDITOR='vim'
export OSS="$HOME/personal"
export DOTFILES="$HOME/personal/dotfiles"
export SNAPP="$HOME/work/snapp"
export ESPAD="$HOME/work/espad"

export PATH="$HOME/.emacs.d/bin/:/Applications/Emacs.app/Contents/MacOS:$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$DOTFILES/bin:$HOME/.composer/vendor/bin"

# Aliases
alias reload='source ~/.zshrc'

alias snapp="cd $SNAPP"
alias oss="cd $OSS"
alias golobby="cd $GOLOBBY"
alias dots="cd ${DOTFILES}"

alias gs='git status'

alias freenet="echo ${VPN_PASSWORD} | sudo openconnect --no-dtls --passwd-on-stdin --user ${VPN_USERNAME} ${VPN_SERVER}"
alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'
alias baly='echo ${BALY_PASSWORD} | sudo openconnect --user amirreza.askarpour vpn-aws.snapp.ir:443'

alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

if command -v nvim &> /dev/null
then
    alias v='nvim'
    alias vi='nvim'
    alias vim='nvim'
    export EDITOR='nvim'
fi

if command -v exa &> /dev/null
then
    alias ls='exa'
    alias ll='exa -la'
    alias l='exa -la'
fi

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

export HOMEBREW_NO_AUTO_UPDATE=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

subl() {
   SUBL_BIN='' 
   /Applications/Sublime\ Text.app/Contents/MacOS/sublime_text $1 > /dev/null 2>&1 &
}
POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true

alias ta='tmux attach -t '
alias tl='tmux list-sessions'
alias tns='tmux new-session -s '

# export LIBRARY_PATH="$LIBRARY_PATH:$(brew --prefix)/lib"

# export C_INCLUDE_PATH="$C_INCLUDE_PATH:$(brew --prefix)/include"

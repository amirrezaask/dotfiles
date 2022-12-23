#!/usr/bin/env zsh
source $HOME/.config/zsh/antigen.zsh
antigen use oh-my-zsh
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen apply

alias emacs='emacs -nw'
# alias emacs='emacs'
export GO111MODULE='on'
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
# export GOPROXY='https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'
export GOPROXY='goproxy.io,direct'
export EDITOR='emacs -nw'
export OSS="$HOME/personal"
export DOTFILES="$HOME/dev/dotfiles"
export SNAPP="$HOME/work/snapp"
export PLAN9=/Users/amirreza/plan9
export PATH="$HOME/.emacs.d/bin/:/Applications/Emacs.app/Contents/MacOS:$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$DOTFILES/bin:$HOME/.composer/vendor/bin:$PLAN9/bin:$DOTFILES/acme-bin"

# Aliases
alias reload='source ~/.zshrc'

alias gs='git status'
alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'
alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake

alias ca="cargo"

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

export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'
alias dots='cd ~/dev/dotfiles'

ss_proxy() {
    export http_proxy='http://localhost:1087'
    export https_proxy='http://localhost:1087'
}

alias snapp='cd ~/dev/snapp/'

[[ $- == *i* ]] && source "$HOME/.config/zsh/completion.zsh" 2> /dev/null

source "$HOME/.config/zsh/key-bindings.zsh"
if ! command -v starship &> /dev/null
then
    curl -sS https://starship.rs/install.sh | sh
fi

eval "$(starship init zsh)"

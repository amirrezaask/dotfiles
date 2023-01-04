#!/usr/bin/env zsh

[ ! -f "$HOME/.antigen.zsh" ] && curl -L git.io/antigen > antigen.zsh
source $HOME/.antigen.zsh

antigen use oh-my-zsh
antigen bundle fzf
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen apply

export GO111MODULE='on'
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='goproxy.io,direct'
export EDITOR='nvim'
export PATH="$HOME/.emacs.d/bin/:/Applications/Emacs.app/Contents/MacOS:$GOPATH/bin:/opt/homebrew/bin::$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:$PATH:$HOME/.composer/vendor/bin"

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env" # ghcup-env
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export HOMEBREW_NO_AUTO_UPDATE=1

export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

reload() {
    source ~/.zshrc
}

ss_proxy() {
    export http_proxy='http://localhost:1087'
    export https_proxy='http://localhost:1087'
}

if command -v nvim &> /dev/null
then
    alias vim='nvim'
fi

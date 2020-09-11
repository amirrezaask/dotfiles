#!/usr/bin/env zsh

export DOTFILES="$HOME/w/dotfiles"
export GO111MODULE='on'
export GOPATH="$HOME/go"
export GOROOT="/usr/local/go" 
export PYTHONBINS="$HOME/.local/bin"
export EDITOR='nvim'
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='goproxy.io'
export PLAN9="$HOME/.local/plan9"
export _JAVA_AWT_WM_NONREPARENTING=1
export PATH="$PLAN9/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONBINS:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"

alias open='xdg-open'
alias snappvpn="sudo openfortivpn -c $HOME/snappDC.conf"
# alias v=nvim
# alias vim=nvim
# alias vi=nvim
alias g='git status'
alias vd="cd $DOTFILES && vim"
setxkbmap -layout "us,ir" -option "grp:shifts_toggle" -option "ctrl:nocaps"

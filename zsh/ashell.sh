#!/bin/bash
# Contains all my general shell vars and configs
reload_ashell() {
	source ~/.ashell
}
export GOPATH=/home/amirreza/go
export PYTHONPATH=/home/amirreza/.local/bin
export PATH=$HOME/.config/composer/vendor/bin:/usr/local/go/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONPATH:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH
alias agp="cd $GOPATH/src/github.com/amirrezaask"
alias pgp="cd $GOPATH/src/git.raad.cloud/cloud"
alias ppp="cd ~/src/paygear"
alias pinstall="sudo pacman -S"
alias psearch="sudo pacman -Ss"
alias dots="cd ~/projects/mine/dotfiles"
alias vim=nvim
alias ocdp="oc delete pod"
# reset default editor for git and etc
export EDITOR=nvim
# reset setxkbmap
setxkbmap -option
# Caps remapping
setxkbmap -layout us,ir -option "caps:swapescape" -option "grp:alt_shift_toggle"
# setxkbmap -layout us,ir -option "ctrl:nocaps" -option "grp:alt_shift_toggle"

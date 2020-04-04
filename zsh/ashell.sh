#!/bin/bash
# Contains all my general shell vars and configs
reload_ashell() {
	source ~/.ashell
}
export GOPATH=/home/amirreza/go
export GOROOT=/usr/local/go
export PYTHONPATH=/home/amirreza/.local/bin
export PATH=$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONPATH:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH
alias vim=nvim
# reset default editor for git and etc
export EDITOR=nvim
# reset setxkbmap
setxkbmap -option
# Caps remapping
# setxkbmap -layout us,ir -option "caps:swapescape" -option "grp:alt_shift_toggle"
setxkbmap -layout us,ir -option "ctrl:nocaps" -option "grp:alt_shift_toggle"

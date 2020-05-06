set -x EMACSCONFIG '~/.emacs.d'
set -x GO111MODULE 'on'
set -x GOPATH '/home/amirreza/go'
set -x GOROOT '/usr/local/go'
set -x PYTHONPATH '/home/amirreza/.local/bin'
set -x PATH "$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONPATH:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"
set -x EMACSTERMINAL "emacsclient -t"
set -x EDITOR "$EMACSTERMINAL"
set -x SHELL "/bin/zsh"

alias e "$EMACSTERMINAL"
alias open "xdg-open"
alias vim "nvim"

# reset setxkbmap
setxkbmap -option

setxkbmap -layout us,ir -option "ctrl:nocaps" -option "grp:alt_shift_toggle"

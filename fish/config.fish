set -x EMACSCONFIG '~/.emacs.d'
set -x GO111MODULE 'on'
set -x GOPATH '/home/amirreza/go'
set -x GOROOT '/usr/local/go'
set -x PLAN9 "$HOME/.local/plan9"
set -x PYTHONPATH '/home/amirreza/.local/bin'
set -x PATH "$PLAN9/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONPATH:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"
set -x EMACSTERMINAL "emacs -nw" 
set -x EDITOR nvim 
set -x GOPRIVATE "gitlab.snapp.ir"
set -x GOPROXY "goproxy.io"

alias e "$EMACSTERMINAL"
alias open "xdg-open"
alias g "git"
alias gs "git status"
alias snappvpn "sudo openfortivpn -c ~/snappDC.conf"
alias lock "i3lock -c 000000"
setxkbmap -layout "us,ir" -option "grp:shifts_toggle" -option "ctrl:nocaps"


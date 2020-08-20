set -x EMACSCONFIG '~/.emacs.d'
set -x GO111MODULE 'on'
set -x GOPATH '/home/amirreza/go'
set -x GOROOT '/usr/local/go'
set -x PYTHONPATH '/home/amirreza/.local/bin'
set -x PATH "$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONPATH:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"
set -x EMACSTERMINAL "emacsclient -t"
set -x EDITOR nvim 
set -x GOPRIVATE "gitlab.snapp.ir"
set -x GOPROXY "goproxy.io"

alias e "$EMACSTERMINAL"
alias open "xdg-open"
alias g "git"
alias gs "git status"
alias vim "nvim"
alias vi "nvim"
alias snappvpn "sudo openfortivpn -c ~/snappDC.conf"
setxkbmap -layout "us,ir" -option "grp:shifts_toggle" -option "ctrl:nocaps"


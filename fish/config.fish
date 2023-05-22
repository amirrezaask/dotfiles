set fish_greeting "" # no start greetings

set -gx GO111MODULE 'on'
set -gx GOPATH "$HOME"
set -gx GOPRIVATE "gitlab.snapp.ir"
set -gx GOPROXY 'goproxy.io,direct'

set -gx PATH "/opt/homebrew/bin:$PATH"

set -gx PATH "$HOME/.local/bin:$PATH"

set -gx PATH "$GOROOT/bin:$PATH"

set -gx PATH "$HOME/.config/composer/vendor/bin:$PATH"

set -gx PATH "$GOPATH/bin:$PATH"

set -gx PATH "$HOME/.cargo/bin:$PATH"

eval $(brew shellenv)

alias vim 'nvim'

set -gx EDITOR 'nvim'
set -gx HOMEBREW_NO_AUTO_UPDATE '1'

set -gx FZF_DEFAULT_OPTS '--height 20%'
set -gx FZF_DEFAULT_COMMAND 'rg --files'

alias snappvpn 'sudo openfortivpn -c ~/snapp-fortigate.conf'

function tmux-open
    tmux new-session -A -t (realpath $argv[1]) -c (realpath $argv[1])
end

alias o 'tmux-open'

# Git stuff
alias g 'git'
alias gs 'git status'
alias gd 'git diff'
alias gp 'git push'

alias c 'code'
alias v 'vim'
alias ca 'cargo'

alias reload 'source ~/.config/fish/config.fish'

if ! command -v starship &> /dev/null
    curl -sS https://starship.rs/install.sh | sh
end
starship init fish | source

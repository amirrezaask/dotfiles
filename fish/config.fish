# source ~/env.fish
set -x GO111MODULE 'auto'
set -x GOPATH "$HOME"
set -x PYTHONBINS "$HOME/.local/bin"
set -x EDITOR 'nvim'
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x DOTFILES "~/src/github.com/amirrezaask/dotfiles"
set -x SNAPP "~/src/gitlab.snapp.ir"
set -x OSS "~/src/github.com/amirrezaask"
set -x GOLOBBY "~/src/github.com/golobby"
set -x PATH "$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$DOTFILES/bin:$PLAN9/bin:$HOME/.composer/vendor/bin"

function freenet
    echo $VPN_PASSWORD | sudo openconnect --no-dtls --passwd-on-stdin --user $VPN_USERNAME $VPN_SERVER
end
alias lock='i3lock -c000000'

# Git
alias gs='git status'
alias gd='git diff'
alias gp='git push'
alias gpl='git pull'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gca='git commit -a'

# vim
alias vim='nvim'
alias v='nvim'

# FZF stuff
setenv FZF_DEFAULT_COMMAND 'fd --type file --follow'
setenv FZF_CTRL_T_COMMAND 'fd --type file --follow'
setenv FZF_DEFAULT_OPTS '--height 20%'

function reload
    source ~/.config/fish/config.fish
end

function snappvpn
    sudo openfortivpn -c ~/snappDC.conf
end

function golobby
    cd $GOLOBBY
end

function oss
    cd $OSS
end

function snapp
    cd $SNAPP
end

function dots
    cd $DOTFILES
end



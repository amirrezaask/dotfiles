# source ~/env.fish
set -x GO111MODULE 'auto'
set -x GOPATH "$HOME"
set -x PYTHONBINS "$HOME/.local/bin"
set -x EDITOR 'nvim'
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x DOTFILES "$HOME/src/github.com/amirrezaask/dotfiles"
set -x SNAPP "$HOME/src/gitlab.snapp.ir"
set -x OSS "$HOME/src/github.com/amirrezaask"
set -x GOLOBBY "$HOME/src/github.com/golobby"
set -x PATH "$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$HOME/.composer/vendor/bin"

function freenet
    echo $VPN_PASSWORD | sudo openconnect --no-dtls --passwd-on-stdin --user $VPN_USERNAME $VPN_SERVER
end
alias lock='i3lock -c000000'

# Git
alias gs='git status'
alias gl='git log'
alias gd='git diff'
alias gp='git push'
alias gpl='git pull'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gca='git commit -a'
alias gc='git commit'
alias ga='git add'
alias g='git'

# Vim stuff
# if type -q nvim
#     alias v=nvim
#     alias vim=nvim
#     alias vi=nvim
# end

# FZF stuff
setenv FZF_DEFAULT_OPTS '--height 20%'
setenv FZF_DEFAULT_COMMAND 'rg --files'

# prompt
function fish_prompt
    echo "$(prompt_pwd)$(set_color green)$(fish_git_prompt)> $(set_color normal)"
end

function fish_right_prompt
    # set_color purple
    date "+%d/%m/%y %H:%M"
end

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



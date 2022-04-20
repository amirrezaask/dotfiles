# source ~/env.fish
set -x GO111MODULE 'auto'
set -x GOPATH "$HOME"
set -x PYTHONBINS "$HOME/.local/bin"
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x DOTFILES "$HOME/src/github.com/amirrezaask/dotfiles"
set -x PATH "$DOTFILES/bin:$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$HOME/.composer/vendor/bin"

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

# FZF stuff
setenv FZF_DEFAULT_OPTS '--height 20%'
setenv FZF_DEFAULT_COMMAND 'rg --files'

# prompt
function fish_prompt
    echo "$(prompt_pwd)$(set_color green)$(fish_git_prompt)> $(set_color normal)"
end

function reload
    source ~/.config/fish/config.fish
end

function snappvpn
    sudo openfortivpn -c ~/snappDC.conf
end

function dots
    cd $DOTFILES
end

command -v 'nvim' > /dev/null

if test $status -eq '0'
    alias vim='nvim'
    alias vi='nvim'
    alias v='nvim'
    setenv EDITOR 'nvim'
end

command -v 'exa' > /dev/null
if test $status -eq '0'
    alias ls='exa -la'
    alias l='exa -la'
    alias ll='exa -la'
end

command -v 'subl' > /dev/null
if test $status -eq '0'
    alias s='subl .'
end

command -v 'code' > /dev/null
if test $status -eq '0'
    alias c='code .'
end


function work
    set PROJECT $(find $LOOKUP_DIR/* -type d -maxdepth 0 | fzf) 
    tmux new-session -c $PROJECT -s $(basename $PROJECT) -n "shell" -d
    tmux neww -n editor -c $PROJECT
    tmux attach-session -t $(basename $PROJECT)
end

function snapp
    set -x LOOKUP_DIR "$HOME/src/gitlab.snapp.ir/" && work
end

function oss
    set -x LOOKUP_DIR "$HOME/src/github.com/amirrezaask/" && work 
end

function golobby 
    set -x LOOKUP_DIR "$HOME/src/github.com/golobby/" && work 
end

# some tmux stuff
alias tl='tmux ls'
alias ta='tmux attach -t'
alias tks='tmux kill-session -t'

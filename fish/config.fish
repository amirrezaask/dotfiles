# source ~/env.fish
set -x GO111MODULE 'auto'
set -x GOPATH "$HOME/.local/go"
set -x PYTHONBINS "$HOME/.local/bin"
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x GOPROXY 'https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'
set -x PERSONAL "$HOME/personal"
set -x SNAPP "$HOME/work/snapp"
set -x ESPAD "$HOME/work/espad"
set -x SEDS "$HOME/work/seds"
set -x DOTFILES "$PERSONAL/dotfiles"
set -x GHCUP "$HOME/.ghcup"
set -x PATH "$GHCUP/bin:$HOME/.emacs.d/bin/:/Applications/Emacs.app/Contents/MacOS:$DOTFILES/bin:$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$HOME/.composer/vendor/bin"

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

alias snapp="cd $SNAPP"
alias espad="cd $ESPAD"
alias seds="cd $SEDS"
alias oss="cd $PERSONAL"

# some tmux stuff
alias tl='tmux ls'
alias ta='tmux attach -t'
alias tks='tmux kill-session -t'

function fish_prompt 
    # This shows up as USER@HOST /home/user/ >, with the directory colored
    # $USER and $hostname are set by fish, so you can just use them
    # instead of using `whoami` and `hostname`
    printf '%s%s%s%s > '  (prompt_pwd) (set_color green) (fish_git_prompt) 
end

source ~/env

# Variables
export GO111MODULE='on'
export GOPATH="$HOME"
export PYTHONBINS="$HOME/.local/bin"
export EDITOR='nvim'
export GOPRIVATE='devheroes.codes,gitlab.com,gitlab.espadev.ir'
export PLAN9="$HOME/.local/plan9"
export ZIGPATH="/usr/local/zig"
export CARGO_TARGET_DIR="~/.local/cargo/"
export PATH="$ZIGPATH:/usr/local/go/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONBINS:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"
export DOTFILES="~/src/github.com/amirrezaask/dotfiles/"
# Aliases
alias open='xdg-open'
alias g='git status'
alias ga='git add'
alias gcl='git clone'
alias gpl='git pull'
alias gd='git diff'
alias gc='git commit'
alias gp='git push'
alias gcm='git commit -m'
alias gf='git fetch'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gpsup='gp --set-upstream origin $(git_current_branch)'
alias kcl='kubectl'
alias dots="cd ${DOTFILES}"
alias prj="cd ${GOPATH}/src/github.com/amirrezaask"
alias freenet="echo ${VPN_PASSWORD} | sudo openconnect --no-dtls --passwd-on-stdin --user ${VPN_USERNAME} ${VPN_SERVER}"
alias lock='i3lock -c000000'
alias tf='terraform'
alias tg='terragrunt'
alias reload='source ~/.zshrc'
alias sik='pkill'
alias gg='git push'
alias ez='git commit -m'
alias goland='~/jetbrains/GoLand-2020.3/bin/goland.sh 2>/dev/null  &'
if type nvim > /dev/null 2>&1; then
    alias vim='nvim'
    alias vi='nvim'
    alias v='nvim'
fi
if type bat > /dev/null 2>&1; then
    alias cat='bat'
fi


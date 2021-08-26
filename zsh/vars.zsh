source ~/env

# Variables
export GO111MODULE='on'
export GOPATH="$HOME"
export PYTHONBINS="$HOME/.local/bin"
export EDITOR='nvim'
export GOPRIVATE='devheroes.codes,gitlab.com,gitlab.espadev.ir'
export GOPROXY='https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'
export PLAN9="$HOME/.local/plan9"
export ZIGPATH="/usr/local/zig"
export ENCORE_INSTALL="/home/amirreza/.encore"
export DENO_PATH="/home/amirreza/.deno"
export BIN_PATH="$HOME/.local/bin"
export PATH="$DENO_PATH/bin:$ENCORE_INSTALL/bin:$PATH:$HOME/.luarocks/bin:$ZIGPATH:/usr/local/go/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONBINS:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"
export DOTFILES="~/src/github.com/amirrezaask/dotfiles"

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
alias ez='git commit -m'
alias goland='~/jetbrains/GoLand-2020.3/bin/goland.sh 2>/dev/null  &'
alias pstorm='~/jetbrains/PhpStorm-203.7148.74/bin/phpstorm.sh 2>/dev/null  &'
alias pycharm='~/jetbrains/pycharm-2020.3.3/bin/phpstorm.sh 2>/dev/null  &'
alias gdoc='godoc -http=:6060'
alias flua='stylua --config-path ~/.stylua.toml'
alias snapp='sudo openfortivpn -c ~/snappDC.conf'
alias baly='echo ${BALY_PASSWORD} | sudo openconnect --user amirreza.askarpour vpn-aws.snapp.ir:443'
alias gpm='git push origin master'
alias gplm='git pull origin master'
alias gs='git status'
alias work='cd ~/src/gitlab.snapp.ir/'
alias oss='cd ~/src/github.com/amirrezaask'
alias golobby='cd ~/src/github.com/golobby'
if command -v nvim &> /dev/null
then
    alias vim=nvim
    alias vi=nvim
fi

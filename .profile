if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi
source ~/env

# Variables
export GO111MODULE='on'
export BASEGOROOT='/usr/local/go'
export GOVERSION='1.18beta1'
# export GOVERSION='1.17.5'
export GOROOT="${BASEGOROOT}-${GOVERSION}"
export GOPATH="$HOME"
export EDITOR='nvim'
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='https://repo.snapp.tech/repository/goproxy,goproxy.io,direct'
export PLAN9="/usr/local/plan9"
export ENCORE_INSTALL="/home/amirreza/.encore"
export DENO_PATH="/home/amirreza/.deno"
export OSS="$HOME/src/github.com/amirrezaask"
export DOTFILES="$HOME/src/github.com/amirrezaask/dotfiles"
export SNAPP="$HOME/src/gitlab.snapp.ir"
export ESPAD="$HOME/src/gitlab.espadev.ir"
export BINS="$DOTFILES/bin"
export PATH="$DOTFILES/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH:$PLAN9/bin"

# Aliases
alias open='xdg-open'
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
alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'
alias baly='echo ${BALY_PASSWORD} | sudo openconnect --user amirreza.askarpour vpn-aws.snapp.ir:443'
alias gpm='git push origin master'
alias gplm='git pull origin master'
alias gs='git status'
alias oss='cd ~/src/github.com/amirrezaask'
alias golobby='cd ~/src/github.com/golobby'
alias snapp="cd $SNAPP"
alias espad="cd $ESPAD"
if command -v nvim &> /dev/null
then
    alias v=nvim
    alias vim=nvim
    alias n=nvim
fi

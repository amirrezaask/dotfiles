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
if command -v nvim &> /dev/null
then
    alias v=nvim
    alias vim=nvim
    alias n=nvim
fi

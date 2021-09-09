source ~/env.fish
set -x GO111MODULE 'on'
set -x GOPATH "$HOME/go"
set -x PYTHONBINS "$HOME/.local/bin"
set -x EDITOR 'nvim'
set -x GOROOT '/usr/local/go'
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x PLAN9 "$HOME/.local/plan9"
set -x ZIGPATH "/usr/local/zig"
set -x PATH "$ZIGPATH:/usr/local/go/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONBINS:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"
set -x DOTFILES "~/src/repos/personal/dotfiles/"

alias open='xdg-open'
alias gs='git status'
alias gpsup='gp --set-upstream origin (git_current_branch)'
alias kcl='kubectl'
alias dots="cd $DOTFILES"
alias oss='cd ~/src/repos/personal/'
alias snapp='cd ~/src/repos/snapp/'
function freenet
    echo $VPN_PASSWORD | sudo openconnect --no-dtls --passwd-on-stdin --user $VPN_USERNAME $VPN_SERVER
end
alias lock='i3lock -c000000'
alias tf='terraform'
alias tg='terragrunt'

alias goland="goland 2>/dev/null &"
alias pstorm="phpstorm 2>/dev/null &"
alias webstorm="webstorm 2>/dev/null &"
alias pycharm="pycharm 2>/dev/null &"
alias intelij="intelij 2>/dev/null &"
alias datagrip="datagrip 2>/dev/null &"
alias gdoc='godoc -http=:6060'
alias vim='nvim'
alias vi='nvim'
alias v='nvim'

function reload
    source ~/.config/fish/config.fish
end

function snappvpn
    sudo openfortivpn -c ~/snappDC.conf
end

# install starship if not installed
if not command -v starship &> /dev/null
then
    curl -fsSL 'https://starship.rs/install.sh'
end

starship init fish | source


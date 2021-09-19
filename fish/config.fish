source ~/env.fish
set -x GO111MODULE 'on'
set -x GOPATH "$HOME"
set -x PYTHONBINS "$HOME/.local/bin"
set -x EDITOR 'nvim'
set -x GOROOT '/usr/local/go'
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x PLAN9 "$HOME/.local/plan9"
set -x ZIGPATH "/usr/local/zig"
set -x RUSTUP "$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin"
set -x PATH "$RUSTUP:$ZIGPATH:/usr/local/go/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONBINS:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"
alias open='xdg-open'
alias gs='git status'
alias gpsup='gp --set-upstream origin (git_current_branch)'
alias kcl='kubectl'
alias oss='cd ~/src/github.com/amirrezaask'
set -x DOTFILES "~/src/github.com/amirrezaask/dotfiles"
alias dots="cd $DOTFILES"
alias snapp='cd ~/src/gitlab.snapp.ir'
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

set -x PROMPT_ENGINE "none" # Also starship, none available

# install oh-my-posh
if not command -v oh-my-posh &> /dev/null
then
    sudo wget https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/posh-linux-amd64 -O /usr/local/bin/oh-my-posh
    sudo chmod +x /usr/local/bin/oh-my-posh
end

# Greeting message from fish
function fish_greeting
    # welcome
end

if [ $PROMPT_ENGINE = "starship" ]
    starship init fish | source
end

if [ $PROMPT_ENGINE = "oh-my-posh" ]
    oh-my-posh --init --shell fish --config ~/src/github.com/amirrezaask/dotfiles/powershell/amirreza.omp.json | source
end

if [ $PROMPT_ENGINE = "none" ]
    # Fish git prompt
    set __fish_git_prompt_showuntrackedfiles 'yes'
    set __fish_git_prompt_showdirtystate 'yes'
    set __fish_git_prompt_showstashstate ''
    set __fish_git_prompt_showupstream 'none'
    set -g fish_prompt_pwd_dir_length 3

    function fish_prompt
        set_color brblack
        echo -n "["(date "+%H:%M")"] "
        set_color brblue
        echo -n (hostname)
        if [ $PWD != $HOME ]
            set_color brblack
            echo -n ':'
            set_color yellow
            echo -n (basename $PWD)
        end
        set_color green
        printf '%s ' (__fish_git_prompt)
        set_color red
        echo -n '| '
        set_color normal
    end
end

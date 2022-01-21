source ~/env.fish
set -x GO111MODULE 'on'
set -x GOPATH "$HOME"
set -x PYTHONBINS "$HOME/.local/bin"
set -x EDITOR 'nvim'
# set -x GOROOT '/usr/local/go'
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x ZIGPATH "/usr/local/zig"
set -x RUSTUP "$HOME/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/bin"
set -x PATH "$RUSTUP:$ZIGPATH:/usr/local/go/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$GOPATH/bin:$HOME/.cargo/bin:$PYTHONBINS:/usr/local/Postman:$HOME/.local/elixir-ls/:$PATH"

set -x DOTFILES "~/src/github.com/amirrezaask/dotfiles"

alias dots="cd $DOTFILES"
alias snapp='cd ~/src/gitlab.snapp.ir'
alias oss='cd ~/src/github.com/amirrezaask'

function freenet
    echo $VPN_PASSWORD | sudo openconnect --no-dtls --passwd-on-stdin --user $VPN_USERNAME $VPN_SERVER
end
alias lock='i3lock -c000000'

# Git
alias gs='git status'
alias gd='git diff'
alias gp='git push'
alias gco='git checkout'
alias gcb='git checkout -b'

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

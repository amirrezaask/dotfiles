# source ~/env.fish
set -x GO111MODULE 'on'
set -x GOPATH "$HOME"
set -x PYTHONBINS "$HOME/.local/bin"
set -x EDITOR 'nvim'
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x DOTFILES "~/src/github.com/amirrezaask/dotfiles"
set -x PATH "$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:/usr/local/Postman:$HOME/.local/elixir-ls/:$HOME/.cache/rebar3/bin:$PATH:$DOTFILES/bin:$PLAN9/bin:$HOME/.composer/vendor/bin"

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

# vim
alias vim='nvim'
alias v='nvim'

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

# function fish_prompt
#     set_color brblack
#     echo -n "["(date "+%H:%M")"] "
#     set_color brblue
#     echo -n (hostname)
#     if [ $PWD != $HOME ]
#         set_color brblack
#         echo -n ':'
#         set_color yellow
#         echo -n (basename $PWD)
#     end
#     set_color green
#     printf '%s ' (__fish_git_prompt)
#     set_color red
#     echo -n '| '
#     set_color normal
# end

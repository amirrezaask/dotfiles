[ ! -d "$HOME/.oh-my-zsh" ] && git clone https://github.com/ohmyzsh/ohmyzsh.git --single-branch --depth 1 .oh-my-zsh

export GO111MODULE='on'
export GOPATH="$HOME"
export GOPRIVATE='gitlab.snapp.ir'
export GOPROXY='goproxy.io,direct'
export EDITOR='vim'

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="/Users/amirreza/Library/Python/3.10/bin:$PATH"
export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
export PLAN9=/Users/amirreza/plan9
export PATH=$PATH:$HOME/dev/dotfiles/bin
export PATH=$PATH:$PLAN9/bin


if command -v brew &> /dev/null
then
    eval $(brew shellenv)
fi

[ -f "$HOME/.ghcup/env" ] && source "$HOME/.ghcup/env"
[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"

export HOMEBREW_NO_AUTO_UPDATE=1
export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

alias reload='source ~/.zshrc'

projects() {
    find $HOME/dev $HOME/dev/ocaml $HOME/w -type d -exec sh -c 'cd "{}"; git rev-parse --git-dir 2> /dev/null 1>&2' \; -prune -print
}

w() {
    dir=$(projects | fzf)
    if [ "$dir" != "" ]; then
        tmux new-window -c $dir -n $(basename $dir)
    fi
}

c() {
    dir=$(projects | fzf)
    if [ "$dir" != "" ]; then
        cd $dir
    fi
}

alias gd='git diff'
alias gds='git diff --staged'
alias gs='git status'
alias g='git'

gco() {
    branch=$(git branch -l | fzf | sed -e 's/^[[:space:]]*//')
    if [ "$branch" != "" ]; then
        git checkout "$branch"
    fi
}

alias ta='tmux attach -t'
alias tl='tmux ls'

# Mabna
alias mabna-up='sudo ipsec up corp'
alias mabna-down='sudo ipsec down corp'
alias mabna-dns='networksetup -setdnsservers Wi-Fi 192.168.10.1'
alias normal-dns='networksetup -setdnsservers Wi-Fi 8.8.8.8 4.2.2.4 '
alias shekan2-dns='networksetup -setdnsservers Wi-Fi 10.202.10.202 10.202.10.102 192.168.10.1'
alias shekan-dns='networksetup -setdnsservers Wi-Fi 178.22.122.100 185.51.200.2 192.168.10.1'


# ACME
a() {
    SHELL='rc' $PLAN9/bin/acme -f /mnt/font/'JetBrainsMono-Regular'/16a/font
}
alias tvim='vim -c"setlocal buftype=nofile"'

if command -v nvim &> /dev/null
then
    alias vim='nvim'
    alias vi='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
fi

alias jvim='vim -c "set syntax=json" -c"setlocal buftype=nofile"'

[[ ! -r /Users/amirreza/.opam/opam-init/init.zsh ]] || source /Users/amirreza/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

export ZSH="$HOME/.oh-my-zsh"
ZSH_THEME="agnoster"

plugins=(
    fzf
)

source $ZSH/oh-my-zsh.sh

eval "$(starship init zsh)"

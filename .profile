alias gs='git status'
alias gst='git status'
alias gdd='git diff HEAD'
alias gg='git push origin $(git_current_branch)'
export GOPRIVATE=gitlab.snappcloud.io
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"

if command -v brew &>/dev/null
then
    eval $(brew shellenv)
fi

if command -v nvim &>/dev/null
then
    alias vim='nvim'
    alias v='nvim'
    alias n='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
fi

export NEOVIDE_FORK=1
export NEOVIDE_TABS=0
export PATH="/usr/local/go/bin:$PATH"
export PATH="$HOME/go/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$PATH:$HOME/prg/bin"
export PLAN9=/Users/amirrezaask/plan9 
export PATH=$PATH:$PLAN9/bin 
export DOTFILES="$HOME/w/dotfiles"
export PATH="$PATH:$DOTFILES/bin"

alias svpn='sudo openfortivpn  --otp $(totpgen ADS)'

[ -f "$HOME/cargo/env" ] && . "$HOME/.cargo/env"

oclogs() {
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
}

export color_prompt='yes'

if [ -n "$BASH" ]; then
    export PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
fi

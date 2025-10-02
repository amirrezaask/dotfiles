export ZSH="$HOME/.oh-my-zsh"
if [ ! -d "$ZSH" ]; then # Installing Oh My ZSH if not installed.
	echo "Installing Oh My Zsh..."
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi

plugins=(git)
ZSH_THEME="robbyrussell"

source "$ZSH/oh-my-zsh.sh"

export HISTSIZE=100000
export HISTFILESIZE=100000

if command -v fzf &> /dev/null
then
    source <(fzf --zsh)
fi

reload() { source ~/.zshrc }

if command -v nvim &> /dev/null
then
	alias vim='nvim'
	alias vi='nvim'
	alias v='nvim'
fi


if command -v subl &> /dev/null
then
    export EDITOR='subl -w'
	export GIT_EDITOR='subl -w'
    alias ss='subl .'
fi

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'
unalias wip 2>/dev/null

wip() {
    branch=$(git symbolic-ref --short HEAD 2>/dev/null)
    if [ -z "$branch" ]; then
        echo "Not on a git branch."
        return 1
    fi
    git add .
    git commit -m "wip"
    git push origin "$branch"
}

ref() {
    if [ -z "$1" ]; then
        echo "Usage: ref <branch-name>"
        return 1
    fi
    git checkout -b "ref-$1"
}

fix() {
    if [ -z "$1" ]; then
        echo "Usage: fix <branch-name>"
        return 1
    fi
    git checkout -b "fix-$1"
}

feat() {
    if [ -z "$1" ]; then
        echo "Usage: feat <branch-name>"
        return 1
    fi
    git checkout -b "feat-$1"
}

if [[ "$(uname)" == "Darwin" ]]; then
    alias goland='open -na "Goland.app" --args'
    alias pstorm='open -na "Goland.app" --args'
fi
# if command -v starship &>/dev/null
# then
# 	eval "$(starship init zsh)"
# fi

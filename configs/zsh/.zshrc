export ZSH="$HOME/.oh-my-zsh"
if [ ! -d "$ZSH" ]; then
	echo "Installing Oh My Zsh..."
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi
plugins=(git)
ZSH_THEME="robbyrussell"

source "$ZSH/oh-my-zsh.sh"

export HISTSIZE=100000
export HISTFILESIZE=100000

source <(fzf --zsh)

reload() { source ~/.zshrc }

if command -v nvim &> /dev/null
then
	alias vim='nvim'
	alias vi='nvim'
	alias v='nvim'
	# export EDITOR='nvim'
	# export MANPAGER='nvim +Man!'
fi


if command -v subl &> /dev/null
then
    export EDITOR='subl'
    alias ss='subl .'
fi

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'
unalias gwip 2>/dev/null

gwip() {
    branch=$(git symbolic-ref --short HEAD 2>/dev/null)
    if [ -z "$branch" ]; then
        echo "Not on a git branch."
        return 1
    fi
    timestamp=$(date "+%Y-%m-%d %H:%M:%S")
    git add .
    git commit -m "Automated WIP Commit: $timestamp"
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

# if command -v starship &>/dev/null
# then
# 	eval "$(starship init zsh)"
# fi
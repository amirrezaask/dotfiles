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
	export EDITOR='nvim'
	# export MANPAGER='nvim +Man!'
fi

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'

alias gs='git status'
alias gd='git diff'
alias ga='git add'
alias gc='git commit'
alias gp='git push origin $(basename $(git rev-parse --abbrev-ref HEAD))'
alias gl='git pull $(basename $(git rev-parse --abbrev-ref HEAD))'
alias gll='git pull --all'
alias glg='git pull --rebase'


function gwip() {
    branch=`git symbolic-ref --short HEAD`
    timestamp=`date "+%Y-%m-%d %H:%M:%S"`
    git add .
    git commit -m "Automated WIP Commit: $timestamp"
    git push origin $branch
}

function ref() {
	git checkout -b "ref-$argv[1]"
}

function fix() {
	git checkout -b "fix-$argv[1]"
}

function feat() {
	git checkout -b "feat-$argv[1]"
}

if command -v starship &>/dev/null
then
	eval "$(starship init zsh)"
fi
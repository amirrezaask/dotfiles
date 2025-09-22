export ZSH="$HOME/.oh-my-zsh"
if [ ! -d "$ZSH" ]; then
	echo "Installing Oh My Zsh..."
	sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi

plugins=(git)
ZSH_THEME="robbyrussell"

source "$ZSH/oh-my-zsh.sh"

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
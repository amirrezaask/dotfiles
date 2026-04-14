ZSH="$HOME/.oh-my-zsh"

if [ ! -d "$ZSH" ]; then
    echo "Oh My Zsh not found, installing..."
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi

ZSH_THEME="robbyrussell"
plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.opencode/bin:$PATH"

alias vim='nvim'
alias vi='nvim'
alias v='nvim'
export GIT_EDITOR='nvim'
export EDITOR='nvim'

alias g='git'
alias nah='git restore --staged . && git restore . && git clean -fd'
alias gcm='git commit -m'
alias gcam='git commit -am'
alias gca='git commit -a'
alias gc='git commit'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gcd='git clone'
alias gd='git diff'
alias gdc='git diff --cached'
alias gds='git diff --staged'
alias gdt='git difftool'
alias gl='git pull --tags --prune'
alias glg='git log'
alias ga='git add'
alias gp='git push'
alias gpsup='git push --set-upstream origin $(git symbolic-ref --short HEAD)'
alias gs='git status'
alias gf='git fetch --all --prune -f'

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'


if command -v fzf &> /dev/null; then
	source <(fzf --zsh)
fi

reload() {
	source ~/.zshrc
}

wip() {
	local branch=$(git symbolic-ref --short HEAD 2>/dev/null)
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

unsetopt BEEP

export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac


export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# GapCode
export PATH="/Users/amirrezaask/.gapcode/bin:$PATH"

eval "$(starship init zsh)"


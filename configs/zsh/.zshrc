
# History configuration
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000
setopt SHARE_HISTORY
setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_IGNORE_SPACE
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY

# Directory navigation
setopt AUTO_CD
setopt AUTO_PUSHD
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_SILENT

# Extended globbing
setopt EXTENDED_GLOB
setopt GLOB_COMPLETE
setopt NO_CASE_GLOB

# Completion
autoload -Uz compinit
compinit
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
zstyle ':completion:*' menu select
zmodload zsh/complist

# Plugin setup (minimal auto-install without a manager)
ZSH_PLUGIN_DIR="${HOME}/.zsh/plugins"
ZSH_AUTOSUGGESTIONS_DIR="${ZSH_PLUGIN_DIR}/zsh-autosuggestions"
ZSH_SYNTAX_HIGHLIGHTING_DIR="${ZSH_PLUGIN_DIR}/zsh-syntax-highlighting"

if [ ! -d "${ZSH_AUTOSUGGESTIONS_DIR}" ]; then
	mkdir -p "${ZSH_PLUGIN_DIR}"
	git clone https://github.com/zsh-users/zsh-autosuggestions "${ZSH_AUTOSUGGESTIONS_DIR}" 2>/dev/null
fi

if [ ! -d "${ZSH_SYNTAX_HIGHLIGHTING_DIR}" ]; then
	mkdir -p "${ZSH_PLUGIN_DIR}"
	git clone https://github.com/zsh-users/zsh-syntax-highlighting "${ZSH_SYNTAX_HIGHLIGHTING_DIR}" 2>/dev/null
fi

# if [ -f "${ZSH_AUTOSUGGESTIONS_DIR}/zsh-autosuggestions.zsh" ]; then
# 	source "${ZSH_AUTOSUGGESTIONS_DIR}/zsh-autosuggestions.zsh"
# fi
#
# if [ -f "${ZSH_SYNTAX_HIGHLIGHTING_DIR}/zsh-syntax-highlighting.zsh" ]; then
# 	source "${ZSH_SYNTAX_HIGHLIGHTING_DIR}/zsh-syntax-highlighting.zsh"
# fi

# Keybindings
bindkey -e

# Word navigation - Option + Arrow (macOS)
bindkey "\e\e[D" backward-word
bindkey "\e\e[C" forward-word

# Word navigation - Option + Arrow (alternative sequences)
bindkey "\e[1;3D" backward-word
bindkey "\e[1;3C" forward-word

# Word navigation - Ctrl + Arrow
bindkey "\e[1;5D" backward-word
bindkey "\e[1;5C" forward-word

# Word navigation - Ctrl + Arrow (alternative)
bindkey "\e[5D" backward-word
bindkey "\e[5C" forward-word

# Delete word - Option + Delete/Backspace
bindkey "\e[3;3~" kill-word
bindkey "\e\b" backward-kill-word
bindkey "\e[3;5~" kill-word

# Home/End keys
bindkey "\e[H" beginning-of-line
bindkey "\e[F" end-of-line
bindkey "\e[1~" beginning-of-line
bindkey "\e[4~" end-of-line
bindkey "\eOH" beginning-of-line
bindkey "\eOF" end-of-line

# Delete key
bindkey "\e[3~" delete-char

# Better word boundaries (treat / as word separator)
autoload -U select-word-style
select-word-style bash

# History search with up/down arrows
autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "^[[A" up-line-or-beginning-search
bindkey "^[[B" down-line-or-beginning-search
bindkey "^P" up-line-or-beginning-search
bindkey "^N" down-line-or-beginning-search

# Path additions
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.opencode/bin:$PATH"

# Editor configuration
if command -v nvim &> /dev/null; then
	alias vim='nvim'
	alias vi='nvim'
	alias v='nvim'
	export GIT_EDITOR='nvim'
	export EDITOR='nvim'
fi

if command -v cursor &> /dev/null; then
	alias c='cursor'
fi

# Git aliases
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

# ls aliases
alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'

# Sublime Text aliases
if command -v subl &> /dev/null; then
	alias s='subl'
	alias ss='subl .'
fi

# eza aliases (if eza is available)
if command -v eza &> /dev/null; then
	alias ls='eza'
	alias ll='eza -l'
fi

# Suffix aliases
alias -s go='go run'
alias -s ts='ts-node'
alias -s js='node'
alias -s py='python3'
alias -s sh='sh'

# Homebrew
if command -v brew &>/dev/null; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# fzf integration
if command -v fzf &> /dev/null; then
	source <(fzf --zsh)
fi

# Custom functions
reload() {
	source ~/.zshrc
}

# Theme change function
theme() {
	local script_path
	
	# Find the change-theme.py script
	if [ -n "$DOTFILES_DIR" ] && [ -f "$DOTFILES_DIR/change-theme.py" ]; then
		script_path="$DOTFILES_DIR/change-theme.py"
	elif [ -f ~/dev/dotfiles/change-theme.py ]; then
		script_path=~/dev/dotfiles/change-theme.py
	elif [ -f ./change-theme.py ]; then
		script_path="./change-theme.py"
	else
		echo "Error: Could not find change-theme.py script"
		echo "Please ensure your dotfiles are in ~/dev/dotfiles or set DOTFILES_DIR environment variable"
		return 1
	fi
	
	# Call the Python script with all arguments
	python3 "$script_path" "$@"
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

# macOS specific aliases
if [[ "$(uname)" == "Darwin" ]]; then
	alias idea='open -na "Intellij IDEA.app" --args'
	alias jj='idea .'

	alias goland='open -na "GoLand.app" --args'
	alias gg='goland .'

	alias pstorm='open -na "PhpStorm.app" --args'
	alias pp='pstorm .'
fi

# Git prompt function
autoload -Uz vcs_info
precmd() { vcs_info }
zstyle ':vcs_info:git:*' formats ' %b'
setopt PROMPT_SUBST

# Custom prompt (similar to fish prompt)
PROMPT='%F{cyan}%~%f%F{blue}${vcs_info_msg_0_}%f
🚀 '

# Disable greeting
unsetopt BEEP

# Starship prompt
if ! command -v starship &> /dev/null; then
    echo "Starship not found, installing..."
    curl -sS https://starship.rs/install.sh | sh
fi

if command -v starship &> /dev/null; then
    eval "$(starship init zsh)"
fi

export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME:"*) ;;
  *) export PATH="$PNPM_HOME:$PATH" ;;
esac

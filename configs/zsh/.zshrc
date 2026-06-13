# Profiling - run 'profile' to see startup times
zmodload zsh/datetime 2>/dev/null
typeset -g _profile_start=$EPOCHREALTIME

profile() {
  echo "Zsh startup: $(( $EPOCHREALTIME - $_profile_start ))ms"
}

# Plugins directory
ZSH_PLUGINS="$HOME/.zsh-plugins"

mkdir -p "$ZSH_PLUGINS"

# Install plugins if missing
if [ ! -d "$ZSH_PLUGINS/zsh-syntax-highlighting" ]; then
	git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "$ZSH_PLUGINS/zsh-syntax-highlighting"
fi
if [ ! -d "$ZSH_PLUGINS/zsh-completions" ]; then
	git clone https://github.com/zsh-users/zsh-completions.git "$ZSH_PLUGINS/zsh-completions"
fi
if [ ! -d "$ZSH_PLUGINS/zsh-autosuggestions" ]; then
	git clone https://github.com/zsh-users/zsh-autosuggestions.git "$ZSH_PLUGINS/zsh-autosuggestions"
fi

# Add plugins to fpath for completions
fpath=("$ZSH_PLUGINS/zsh-completions/src" $fpath)

# Completion system
autoload -Uz compinit
compinit -C

# Completion settings
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the string to insert%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:warnings' format '%F{yellow}-- No matches for: %d --%f'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.zcache"

# Load plugins
source "$ZSH_PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
source "$ZSH_PLUGINS/zsh-autosuggestions/zsh-autosuggestions.zsh"

# Starship prompt
if command -v starship &> /dev/null; then
	eval "$(starship init zsh)"
fi

# Neovim
alias vim='nvim'
alias vi='nvim'
alias v='nvim'
export GIT_EDITOR='nvim'
export EDITOR='nvim'

alias o='opencode'
alias c='claude'
alias gap='gapcode'
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
alias gl='git pull --tags --prune --ff-only'
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
alias gg='go build -v ./...'
alias ss='subl .'

if command -v fzf &> /dev/null; then
	source <(fzf --zsh)
fi

if command -v eza &> /dev/null; then
  alias l='eza -lah'
  alias la='eza -lAh'
  alias ll='eza -lh'
  alias ls='eza -G'
  alias lsa='eza -lah'
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

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.opencode/bin:$PATH"

# Lazy load NVM (biggest startup cost)
export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
nvm() {
  unfunction nvm
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  nvm "$@"
}
node() {
  unfunction node npm npx
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  node "$@"
}
npm() {
  unfunction node npm npx
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  npm "$@"
}
npx() {
  unfunction node npm npx
  [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
  npx "$@"
}

export PATH="/Users/amirrezaask/.gapcode/bin:$PATH"

export PATH=/Users/amirrezaask/.opencode/bin:$PATH

export PATH="$HOME/.local/share/nvim/mason/bin:$PATH"

export PATH="/opt/homebrew/bin:$PATH"

# bun completions
[ -s "/Users/amirrezaask/.bun/_bun" ] && source "/Users/amirrezaask/.bun/_bun"

# >>> grok installer >>>
export PATH="$HOME/.grok/bin:$PATH"
fpath=(~/.grok/completions/zsh $fpath)
# <<< grok installer <<<

# pnpm
export PNPM_HOME="/Users/amirrezaask/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME/bin:"*) ;;
  *) export PATH="$PNPM_HOME/bin:$PATH" ;;
esac
# pnpm end

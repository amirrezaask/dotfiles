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

# History (oh-my-zsh style)
HISTFILE="${HISTFILE:-$HOME/.zsh_history}"
HISTSIZE=50000
SAVEHIST=10000
setopt extended_history hist_expire_dups_first hist_ignore_dups hist_ignore_space
setopt hist_verify share_history

# Directory / shell niceties (oh-my-zsh style)
setopt auto_cd auto_pushd pushd_ignore_dups pushdminus
setopt interactive_comments multios long_list_jobs
unsetopt BEEP

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -- -='cd -'

# Completion settings (oh-my-zsh style)
zmodload -i zsh/complist
unsetopt menu_complete flowcontrol
setopt auto_menu complete_in_word always_to_end
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{[:lower:][:upper:]}={[:upper:][:lower:]}' 'r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt '%SAt %p: Hit TAB for more, or the string to insert%s'
zstyle ':completion:*' group-name ''
zstyle ':completion:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:warnings' format '%F{yellow}-- No matches for: %d --%f'
zstyle ':completion:*' squeeze-slashes true
zstyle ':completion:*' special-dirs true
zstyle ':completion:*' use-cache on
zstyle ':completion:*' cache-path "$HOME/.zcache"
bindkey -M menuselect '^o' accept-and-infer-next-history

# Bracketed paste + URL quoting (built-in zsh, oh-my-zsh style)
autoload -Uz bracketed-paste-magic url-quote-magic
zle -N bracketed-paste bracketed-paste-magic
zle -N self-insert url-quote-magic

# Emacs line editing + macOS/Ghostty keybindings (macos-option-as-alt)
autoload -Uz select-word-style
select-word-style bash

bindkey -e

if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
  zle-line-init() { echoti smkx }
  zle-line-finish() { echoti rmkx }
  zle -N zle-line-init
  zle -N zle-line-finish
fi

# Single-char delete (plain Backspace/Delete)
bindkey '^?' backward-delete-char
bindkey '^H' backward-delete-char
if [[ -n "${terminfo[kdch1]}" ]]; then
  bindkey "${terminfo[kdch1]}" delete-char
else
  bindkey '^[[3~' delete-char
fi

# Word kill — Option+Backspace/Delete with option-as-alt (Ghostty, VS Code, iTerm)
bindkey '^[^?' backward-kill-word
bindkey '^[^H' backward-kill-word
bindkey '^[d' kill-word
bindkey '^[D' kill-word
bindkey '^[[3;3~' kill-word
bindkey '^[[3;5~' kill-word

# Word navigation — Option+Arrow
bindkey '\e\e[D' backward-word
bindkey '\e\e[C' forward-word
bindkey '\e[1;3D' backward-word
bindkey '\e[1;3C' forward-word

# Word navigation — Ctrl+Arrow
bindkey '^[[1;5D' backward-word
bindkey '^[[1;5C' forward-word

# Home / End
if [[ -n "${terminfo[khome]}" ]]; then
  bindkey "${terminfo[khome]}" beginning-of-line
fi
if [[ -n "${terminfo[kend]}" ]]; then
  bindkey "${terminfo[kend]}" end-of-line
fi
bindkey '\e[H' beginning-of-line
bindkey '\e[F' end-of-line
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
bindkey '\eOH' beginning-of-line
bindkey '\eOF' end-of-line

# History search — type prefix + Up/Down
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey '^[[A' up-line-or-beginning-search
bindkey '^[[B' down-line-or-beginning-search
if [[ -n "${terminfo[kcuu1]}" ]]; then
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
if [[ -n "${terminfo[kcud1]}" ]]; then
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi
bindkey '^P' up-line-or-beginning-search
bindkey '^N' down-line-or-beginning-search

bindkey ' ' magic-space
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^X^E' edit-command-line

# Load plugins (syntax-highlighting must be last)
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=240'
ZSH_AUTOSUGGEST_STRATEGY=(history completion)
source "$ZSH_PLUGINS/zsh-autosuggestions/zsh-autosuggestions.zsh"
source "$ZSH_PLUGINS/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"

# Robby Russell-style prompt
autoload -U colors && colors
setopt PROMPT_SUBST
PROMPT='%(?:%{$fg_bold[green]%}➜%{$reset_color%}:%{$fg_bold[red]%}➜%{$reset_color%}) %{$fg_bold[blue]%}%1~%{$reset_color%}$(git_prompt_info) %{$fg_bold[blue]%}➜%{$reset_color%} '

git_prompt_info() {
  if git rev-parse --git-dir >/dev/null 2>&1; then
    local branch
    branch=$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
    if [ -n "$branch" ]; then
      if git diff --quiet 2>/dev/null && git diff --cached --quiet 2>/dev/null; then
        echo " %{$fg_bold[cyan]%}git:(%{$fg_bold[red]%}${branch}%{$fg_bold[cyan]%})%{$reset_color%}"
      else
        echo " %{$fg_bold[cyan]%}git:(%{$fg_bold[red]%}${branch}%{$fg_bold[cyan]%})%{$fg[yellow]%} ✗%{$reset_color%}"
      fi
    fi
  fi
}

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

export PNPM_HOME="$HOME/Library/pnpm"
case ":$PATH:" in
  *":$PNPM_HOME/bin:"*) ;;
  *) export PATH="$PNPM_HOME/bin:$PATH" ;;
esac

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.opencode/bin:$PATH"
export PATH="$HOME/.gapcode/bin:$PATH"
export PATH="$HOME/.grok/bin:$PATH"
export PATH="$HOME/.local/share/nvim/mason/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"

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

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

fpath=("$HOME/.grok/completions/zsh" $fpath)

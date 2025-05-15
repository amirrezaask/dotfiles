# Enable persistent history
HISTFILE=~/.zsh_history
HISTSIZE=100000
SAVEHIST=100000

# Options
setopt autocd              # cd into directories without typing 'cd'
setopt correct             # auto-correct mistakes in commands
setopt no_beep             # no beep on errors
setopt append_history      # append rather than overwrite history
setopt hist_ignore_dups    # don't store duplicate commands in history
setopt share_history       # share history between all sessions

autoload -U colors && colors

autoload -Uz vcs_info

autoload -U compinit 
zmodload zsh/complist
compinit 
zstyle ':completion:*' menu select                        # Use menu completion when there is a list of choices
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' # Case-insensitive matching
zstyle ':vcs_info:git:*' formats '%b'
_comp_options+=(globdots)		# Include hidden files.

# Keybindings
bindkey -e
bindkey "\e[A" history-beginning-search-backward
bindkey "\e[B" history-beginning-search-forward

# Prompt
precmd() {  ## Always re evaluate prompt
  vcs_info 
  PS1="%{$fg[magenta]%}%~ %{$fg[red]%}${vcs_info_msg_0_}%{$reset_color%} "
}

PS1="%{$fg[magenta]%}%~ %{$fg[red]%}${vcs_info_msg_0_}%{$reset_color%} "

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'
alias gs='git status'
alias gd='git diff'
alias ga='git add'
alias gc='git commit'
alias gp='git push'
alias gl='git pull'
alias gll='git pull --all'
alias glg='git pull --rebase'

export PATH="$HOME/go/bin:$PATH"

reload() { source ~/.zshrc }

function set-title() { print -Pn "\e]0;${PWD:t}\a" }
precmd_functions+=(set-title)

if command -v nvim &> /dev/null
then
  alias vim='nvim'
  alias vi='nvim'
  alias v='nvim'
  export EDITOR='nvim'
  export GIT_EDITOR="$EDITOR"
fi

if command -v cursor &> /dev/null
then
  alias code='cursor'
fi

function git_branch() {
  local branch
  branch=$(git symbolic-ref --short HEAD 2>/dev/null)
  if [[ -n "$branch" ]]; then
    echo "$branch"
  else
    echo ""
  fi
}


alias gwip='git add .; git commit -m "Automated WIP Commit: $(date +"%Y-%m-%d %H:%M:%S")"; git push origin $vcs_info_msg_0_'

if command -v fzf &> /dev/null
then
  source <(fzf --zsh)
fi

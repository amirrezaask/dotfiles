# Oh My Zsh installation and initialization
export ZSH="$HOME/.oh-my-zsh"

# Install Oh My Zsh if not already installed
if [ ! -d "$ZSH" ]; then
  echo "Installing Oh My Zsh..."
  sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)" "" --unattended
fi

# Oh My Zsh configuration
ZSH_THEME="robbyrussell"
plugins=(git)

# Source Oh My Zsh
if [ -f "$ZSH/oh-my-zsh.sh" ]; then
  source "$ZSH/oh-my-zsh.sh"
fi

# Define ZSH_PLUGINS_DIR if not already defined
if [ -z "$ZSH_PLUGINS_DIR" ]; then
  export ZSH_PLUGINS_DIR="$HOME/.zsh/plugins"
  mkdir -p "$ZSH_PLUGINS_DIR"
fi

# Plugins
if [ ! -d "$ZSH_PLUGINS_DIR/zsh-syntax-highlighting" ]; then
  git clone https://github.com/zsh-users/zsh-syntax-highlighting.git ${ZSH_PLUGINS_DIR}/zsh-syntax-highlighting
fi

if [ ! -d "$ZSH_PLUGINS_DIR/zsh-completions" ]; then
  git clone https://github.com/zsh-users/zsh-completions ${ZSH_PLUGINS_DIR}/zsh-completions
fi

source ${ZSH_PLUGINS_DIR}/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source ${ZSH_PLUGINS_DIR}/zsh-completions/zsh-completions.plugin.zsh

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'
alias gs='git status'
alias gd='git diff'
alias ga='git add'
alias gc='git commit'
alias gp='git push origin $vcs_info_msg_0_'
alias gl='git pull $vcs_info_msg_0_'
alias gll='git pull --all'
alias glg='git pull --rebase'
alias proxyon='networksetup -setsocksfirewallproxystate Wi-Fi on'
alias proxyoff='networksetup -setsocksfirewallproxystate Wi-Fi off'
alias w='networksetup -setnetworkserviceenabled Wi-Fi off && networksetup -setnetworkserviceenabled Wi-Fi on'

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



# Added by Windsurf
export PATH="/Users/amirrezaask/.codeium/windsurf/bin:$PATH"

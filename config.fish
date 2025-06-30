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

if test (uname) = "Darwin" # on macos
  alias proxyon='networksetup -setsocksfirewallproxystate Wi-Fi on'
  alias proxyoff='networksetup -setsocksfirewallproxystate Wi-Fi off'
  alias w='networksetup -setnetworkserviceenabled Wi-Fi off && networksetup -setnetworkserviceenabled Wi-Fi on'
end

export PATH="$HOME/go/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"

function reload
  source ~/.config/fish/config.fish 
end

if type -q nvim 
  alias vim='nvim'
  alias vi='nvim'
  alias v='nvim'
  export EDITOR='nvim'
  export GIT_EDITOR="$EDITOR"
end

if type -q fzf
    fzf --fish | source
end

function gwip
    set branch (git symbolic-ref --short HEAD)
    set timestamp (date "+%Y-%m-%d %H:%M:%S")
    git add .
    git commit -m "Automated WIP Commit: $timestamp"
    git push origin $branch
end

function fish_greeting
end

function fish_right_prompt
      set_color brblack
      echo -n "["(date "+%H:%M")"] "
end

function fish_prompt
  echo -n " "
  set_color blue
  if [ $PWD != $HOME ]
	  set_color yellow
	  echo -n (basename $PWD)
  end
  set_color green
  printf '%s ' (__fish_git_prompt)
  set_color red
  echo -n '| '
  set_color normal
end

export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'

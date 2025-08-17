alias gs='git status'
alias gd='git diff'
alias ga='git add'
alias gc='git commit'
alias gp='git push origin $vcs_info_msg_0_'
alias gl='git pull $vcs_info_msg_0_'
alias gll='git pull --all'
alias glg='git pull --rebase'
alias o='xdg-open'

export PATH="$HOME/go/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="/home/amirreza/.local/bin:$PATH"
export PATH="/home/amirreza/p/dotfiles/bin:$PATH"

export GOPROXY=goproxy.io
export GOPRIVATE=gitlab.snappcloud.io

function reload
  source ~/.config/fish/config.fish 
end

if type -q nvim 
  alias vim='nvim'
  alias vi='nvim'
  alias v='nvim'
  export EDITOR='nvim'
  export GIT_EDITOR="$EDITOR"
  export MANPAGER='nvim +Man!'
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

function ref
	git checkout -b "ref-$argv[1]"
end

function fix
	git checkout -b "fix-$argv[1]"
end

function feat
	git checkout -b "feat-$argv[1]"
end


set -g theme_name "cyberdream"

if type -q starship
	eval (starship init fish)
end

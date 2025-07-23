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

export PATH="$HOME/go/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"

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

function fish_prompt
    # Colors
    set_color normal
	set -l color_dir (set_color green)
    set -l color_branch (set_color yellow)
    set -l color_reset (set_color normal)

    # Abbreviated path
    set -l cwd (prompt_pwd)

    # Git branch if inside repo
    set -l branch ""
    if git rev-parse --is-inside-work-tree >/dev/null 2>&1
        set branch (git rev-parse --abbrev-ref HEAD 2>/dev/null)
        if test -n "$branch"
            set branch " ($branch)"
        end
    end

    # Line 1: colored directory and branch
    echo -n "$color_dir$cwd$color_reset$branch> "

    # Line 2: prompt
    # echo -e "\n\$ "
end




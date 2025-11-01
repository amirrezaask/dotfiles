if command -v fzf &> /dev/null
	fzf --fish | source
end

if command -v nvim &> /dev/null
	alias vim='nvim'
	alias vi='nvim'
	alias v='nvim'
	export GIT_EDITOR='nvim'
	export EDITOR='nvim'
end


if command -v subl &> /dev/null
	alias s='subl'
	alias ss='subl .'
end

function reload
	source ~/.config/fish/config.fish
end

function wip
	set branch (git symbolic-ref --short HEAD 2>/dev/null)
	if test -z "$branch"
		echo "Not on a git branch."
		return 1
	end
	git add .
	git commit -m "wip"
	git push origin "$branch"
end
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
alias gdt='git difftool'
alias gl='git pull --tags --prune'
alias glg='git log'
alias ga='git add'
alias gp='git push'
alias gpsup='git push --set-upstream origin $(git symbolic-ref --short HEAD)'
alias gs='git status'
alias gf='git fetch --all --prune -f'

function ref 
    if [ -z "$1" ]; then
        echo "Usage: ref <branch-name>"
        return 1
	end
    git checkout -b "ref-$1"
end

function fix 
    if [ -z "$1" ]; then
        echo "Usage: fix <branch-name>"
        return 1
	end
    git checkout -b "fix-$1"
end

set system (uname)

function feat
    if [ -z "$1" ]; then
        echo "Usage: feat <branch-name>"
        return 1
	end
    git checkout -b "feat-$1"
end

if test "$system" = "Darwin"
    alias idea='open -na "Intellij IDEA.app" --args'
    alias jj='idea .'

    alias goland='open -na "GoLand.app" --args'
    alias gg='goland .'

    alias pstorm='open -na "PhpStorm.app" --args'
    alias pp='pstorm .'
end

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'

function fish_prompt
	set -l branch (git symbolic-ref --short HEAD 2>/dev/null)
	if test -n "$branch"
		set_color normal
		echo -n ' '
		set_color blue
		echo -n (prompt_pwd)
		set_color green
		echo -n " ($branch)"
		set_color normal
		echo -n ' '
	else
		set_color red
		echo -n " "
		echo -n (prompt_pwd)
		set_color normal
		echo -n ' '
    end
end


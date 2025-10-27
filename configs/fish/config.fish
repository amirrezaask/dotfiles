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
alias gdt='git difftool'
alias gdt='git difftool'
alias gl='git pull --tags --prune'
alias glg='git log'
alias ga='git add'

alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'

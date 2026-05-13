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
alias gl='git pull --tags --prune --force'
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

if command -v fzf &> /dev/null
	fzf --fish | source
end

function reload
	source ~/.config/fish/config.fish
end

function wip
	local branch=$(git symbolic-ref --short HEAD 2>/dev/null)
	if test -z "$branch"
		echo "Not on a git branch."
		return 1
	end
	git add .
	git commit -m "wip"
	git push origin "$branch"
end

function ref
	if test -z "$1"
		echo "Usage: ref <branch-name>"
		return 1
	end
	git checkout -b "ref-$1"
end

function fix
	if test -z "$1"
		echo "Usage: fix <branch-name>"
		return 1
	end
	git checkout -b "fix-$1"
end

function feat
	if test -z "$1"
		echo "Usage: feat <branch-name>"
		return 1
	end
	git checkout -b "feat-$1"
end

export PNPM_HOME="$HOME/Library/pnpm"
export PATH="$PNPM_HOME:$PATH"

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.opencode/bin:$PATH"

export NVM_DIR=(test -z "$XDG_CONFIG_HOME"; and printf %s "$HOME/.nvm"; or printf %s "$XDG_CONFIG_HOME/nvm")

export PATH="/Users/amirrezaask/.gapcode/bin:$PATH"

export PATH="/Users/amirrezaask/.opencode/bin:$PATH"

export PATH="$HOME/.local/share/nvim/mason/bin:$PATH"

alias wstorm='open -na "WebStorm.app" .'
alias goland='open -na "GoLand.app" .'

function fish_prompt --description 'Write out the prompt'
    set -l last_pipestatus $pipestatus
    set -lx __fish_last_status $status # Export for __fish_print_pipestatus.
    set -l normal (set_color --reset)

    # Color the prompt differently when we're root
    set -l color_cwd $fish_color_cwd
    set -l suffix '>'
    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix '#'
    end

    # Write pipestatus
    # If the status was carried over (if no command is issued or if `set` leaves the status untouched), don't bold it.
    set -l bold_flag --bold
    set -q __fish_prompt_status_generation; or set -g __fish_prompt_status_generation $status_generation
    if test $__fish_prompt_status_generation = $status_generation
        set bold_flag
    end
    set __fish_prompt_status_generation $status_generation
    set -l status_color (set_color $fish_color_status)
    set -l statusb_color (set_color $bold_flag $fish_color_status)
    set -l prompt_status (__fish_print_pipestatus "[" "]" "|" "$status_color" "$statusb_color" $last_pipestatus)

    echo -n -s (set_color brgreen) (prompt_pwd) $normal (fish_vcs_prompt) $normal " "$prompt_status $suffix " "
end


function fish_greeting
end

# PATH configuration
set -gx PNPM_HOME "$HOME/Library/pnpm"

set -gx PATH "$PNPM_HOME" $PATH
set -gx PATH "$HOME/.local/bin" $PATH
set -gx PATH "$HOME/.opencode/bin" $PATH
set -gx PATH "/Users/amirrezaask/.gapcode/bin" $PATH
set -gx PATH "$HOME/.local/share/nvim/mason/bin" $PATH
set -gx PATH "/opt/homebrew/bin" $PATH
set -gx PATH "$HOME/.cargo/bin" $PATH
set -gx PATH "$HOME/go/bin" $PATH
set -gx PATH "/usr/local/bin" $PATH


# Neovim
alias vim='nvim'
alias vi='nvim'
alias v='nvim'
set -gx GIT_EDITOR 'nvim'
set -gx EDITOR 'nvim'

# General aliases
alias o='opencode'
alias c='claude'
alias gap='gapcode'

# Git aliases
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
alias gpsup='git push --set-upstream origin (git symbolic-ref --short HEAD)'
alias gs='git status'
alias gf='git fetch --all --prune -f'

# ls aliases
alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias ls='ls -G'
alias lsa='ls -lah'

# Go alias
alias gg='go build -v ./...'

# Editor aliases
alias s='subl'
alias ss='subl .'
alias wstorm='open -na "WebStorm.app" .'
alias goland='open -na "GoLand.app" .'

# fzf integration
if type -q fzf
    fzf --fish | source
end

# Starship prompt
if type -q starship
    starship init fish | source
end

# Functions
function reload
    source ~/.config/fish/config.fish
end

function wip
    set -l branch (git symbolic-ref --short HEAD 2>/dev/null)
    if test -z "$branch"
        echo "Not on a git branch."
        return 1
    end
    git add .
    git commit -m "wip"
    git push origin "$branch"
end

function ref
    if test -z "$argv[1]"
        echo "Usage: ref <branch-name>"
        return 1
    end
    git checkout -b "ref-$argv[1]"
end

function fix
    if test -z "$argv[1]"
        echo "Usage: fix <branch-name>"
        return 1
    end
    git checkout -b "fix-$argv[1]"
end

function feat
    if test -z "$argv[1]"
        echo "Usage: feat <branch-name>"
        return 1
    end
    git checkout -b "feat-$argv[1]"
end

# Disable greeting
function fish_greeting
end

# Added by LM Studio CLI (lms)
set -gx PATH $PATH /Users/amirrezaask/.lmstudio/bin
# End of LM Studio CLI section


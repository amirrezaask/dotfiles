fish_add_path -g $HOME/.local/bin $HOME/.opencode/bin

if type -q nvim
    alias vim nvim; alias vi nvim; alias v nvim
    set -gx EDITOR nvim
    set -gx GIT_EDITOR nvim
end

alias g git
alias nah 'git restore --staged .; and git restore .; and git clean -fd'
alias gcm 'git commit -m'
alias gcam 'git commit -am'
alias gca 'git commit -a'
alias gc 'git commit'
alias gco 'git checkout'
alias gcb 'git checkout -b'
alias gcd 'git clone'
alias gd 'git diff'
alias gdc 'git diff --cached'
alias gds 'git diff --staged'
alias gdt 'git difftool'
alias gl 'git pull --tags --prune'
alias glg 'git log'
alias ga 'git add'
alias gp 'git push'
alias gpsup 'git push --set-upstream origin (git symbolic-ref --short HEAD)'
alias gs 'git status'
alias gf 'git fetch --all --prune -f'

if type -q eza
    alias ls eza
    alias ll 'eza -l'
    alias la 'eza -lAh'
    alias l 'eza -lah'
    alias lsa 'eza -lah'
else
    alias ls 'ls -G'
    alias ll 'ls -lh'
    alias la 'ls -lAh'
    alias l 'ls -lah'
    alias lsa 'ls -lah'
end

if test -x /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv)
else if test -x /usr/local/bin/brew
    eval (/usr/local/bin/brew shellenv)
end

if type -q fzf
    if type -q brew
        set -l bp (brew --prefix)
        test -f $bp/opt/fzf/shell/completion.fish; and source $bp/opt/fzf/shell/completion.fish
        test -f $bp/opt/fzf/shell/key-bindings.fish; and source $bp/opt/fzf/shell/key-bindings.fish
    end
    test -f $HOME/.fzf.fish; and source $HOME/.fzf.fish
end

function reload; source ~/.config/fish/config.fish; end

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
    if test (count $argv) -lt 1
        echo "Usage: ref <branch-name>"
        return 1
    end
    git checkout -b "ref-$argv[1]"
end

function fix
    if test (count $argv) -lt 1
        echo "Usage: fix <branch-name>"
        return 1
    end
    git checkout -b "fix-$argv[1]"
end

function feat
    if test (count $argv) -lt 1
        echo "Usage: feat <branch-name>"
        return 1
    end
    git checkout -b "feat-$argv[1]"
end

if not type -q starship
    curl -sS https://starship.rs/install.sh | sh -s -- -y
end
type -q starship; and starship init fish | source
set -gx PNPM_HOME "$HOME/Library/pnpm"
fish_add_path -g $PNPM_HOME
if set -q GHOSTTY_RESOURCES_DIR
    for f in "$GHOSTTY_RESOURCES_DIR/shell-integration/fish/ghostty-integration.fish" "$GHOSTTY_RESOURCES_DIR/shell-integration/fish/ghostty-integration"
        test -f $f; and source $f
    end
end


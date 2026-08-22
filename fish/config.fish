# ===================== Fish config (mirrors ~/.zshrc) =====================

set fish_greeting

# ---- Editor ----
abbr vim nvim
abbr vi nvim
abbr v nvim
set -gx EDITOR nvim
set -gx GIT_EDITOR nvim

# ---- Tools ----------------------------------------------------
abbr o opencode2
abbr opencode opencode2
abbr c claude
abbr gap gapcode

# ---- Git ------------------------------------------------------
abbr g git
abbr gcm 'git commit -m'
abbr gcam 'git commit -am'
abbr gca 'git commit -a'
abbr gc 'git commit'
abbr gco 'git checkout'
abbr gcb 'git checkout -b'
abbr gcd 'git clone'
abbr gd 'git diff'
abbr gdc 'git diff --cached'
abbr gds 'git diff --staged'
abbr gdt 'git difftool'
abbr gl 'git pull --tags --prune --ff-only'
abbr glg 'git log'
abbr ga 'git add'
abbr gp 'git push'
abbr gpsup 'git push --set-upstream origin (git symbolic-ref --short HEAD)'
abbr gs 'git status'
abbr gf 'git fetch --all --prune -f'
abbr nah 'git restore --staged .; and git restore .; and git clean -fd'

# ---- Files ----------------------------------------------------
abbr l 'eza -lah'
abbr la 'eza -lAh'
abbr ll 'eza -lh'
abbr ls 'eza -G'
abbr lsa 'eza -lah'

# ---- Go / Sublime ------------------------------------------------
abbr gg 'go build -v ./...'
abbr ss 'subl .'

# ---- Directory shortcuts ----------------------------------------
abbr -- - 'cd -'
abbr ... 'cd ../..'
abbr .... 'cd ../../..'
abbr ..... 'cd ../../../..'

# ---- PATH (order mirrors ~/.zshrc) --------------------------------
# PNPM
set -gx PNPM_HOME "$HOME/Library/pnpm"
fish_add_path "$PNPM_HOME/bin"
fish_add_path /opt/homebrew/bin
fish_add_path "$HOME/.local/bin"
fish_add_path "$HOME/.opencode/bin"
fish_add_path "$HOME/.gapcode/bin"
fish_add_path "$HOME/.local/share/nvim/mason/bin"
# Bun
set -gx BUN_INSTALL "$HOME/.bun"
fish_add_path "$BUN_INSTALL/bin"
source ~/.cargo/env.fish

function node
    functions --erase node npm npx
    test -s "$NVM_DIR/nvm.sh"; and source "$NVM_DIR/nvm.sh"
    command node $argv
end

function npm
    functions --erase node npm npx
    test -s "$NVM_DIR/nvm.sh"; and source "$NVM_DIR/nvm.sh"
    command npm $argv
end

function npx
    functions --erase node npm npx
    test -s "$NVM_DIR/nvm.sh"; and source "$NVM_DIR/nvm.sh"
    command npx $argv
end


function fish_prompt
    set -l project (basename (pwd))
    set_color green
    printf '%s ' $project
    set_color normal
end

function fish_right_prompt
    set -l branch (git symbolic-ref --short HEAD 2>/dev/null)
    if test -n "$branch"
        set_color yellow
        printf '%s' "($branch)"
        set_color normal
    end
end

# ---- Starship prompt ------------------------------------------------
if command -q starship
    starship init fish | source
end

# ---- fzf key bindings ------------------------------------------------
if command -q fzf
    fzf --fish | source
end

# ---- Functions --------------------------------------------------------

function reload --description 'Reload fish config'
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

function profile --description 'Show fish startup timing'
    /usr/bin/time fish --command 'exit' 2>&1 | tail -1
end


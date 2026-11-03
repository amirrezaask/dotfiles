# Nushell config mirroring fish/config.fish.

$env.config.show_banner = false

# ---- Prompt ----------------------------------------------------------
def nu-git-branch [] {
    try {
        let result = (do { ^git symbolic-ref --quiet --short HEAD } | complete)
        if $result.exit_code == 0 { $result.stdout | str trim } else { "" }
    } catch { "" }
}

$env.PROMPT_COMMAND = {||
    let relative = (try { $env.PWD | path relative-to $nu.home-dir } catch { null })
    let directory = match $relative {
        null => $env.PWD
        "" => "~"
        _ => ([~ $relative] | path join)
    }
    let branch = (nu-git-branch)
    let suffix = if ($branch | is-empty) { "" } else {
        $" (ansi yellow)\(($branch)\)(ansi reset)"
    }
    $"(ansi green_bold)($directory)(ansi reset)($suffix)"
}

# ---- Completion ------------------------------------------------------
$env.config.completions.algorithm = "fuzzy"
$env.config.completions.sort = "smart"

# Return null on missing/unsupported commands so Nu can complete file paths.
def nu-external-completions [spans: list<string>] {
    if ($spans | length) < 2 { return null }
    try {
        let tokens = ($spans | update 0 {|command| $command | str replace --regex '^\^' '' })
        let expansion = (scope aliases | where name == $tokens.0 | get 0?.expansion?)
        let tokens = if $expansion == null { $tokens } else {
            ($expansion | split row " " | where {|word| not ($word | is-empty) })
            | append ($tokens | skip 1)
        }
        let result = (do { ^carapace $tokens.0 nushell ...$tokens } | complete)
        if $result.exit_code != 0 { return null }
        let candidates = ($result.stdout | from json)
        if ($candidates | is-empty) { null } else { $candidates }
    } catch { null }
}

if not (which carapace | is-empty) {
    $env.config.completions.external.completer = {|spans| nu-external-completions $spans }
}

# ---- History ---------------------------------------------------------
# SQLite records the directory where each command was run. Run `history import`
# once after switching to preserve old plaintext commands; those older entries
# have no directory metadata, so only new commands appear in the directory menu.
$env.config.history.file_format = "sqlite"

# Match before limiting, and keep only the most recent occurrence of a command.
# `history --long` returns a SQLiteQueryBuilder on Nu 0.113, not a list.
def nu-history-suggestions [entries: any, query: string, --cwd: string] {
    $entries
    | reverse
    | where {|entry|
        ($cwd == null or $entry.cwd? == $cwd) and ($entry.command | str contains --ignore-case $query)
    }
    | get command
    | uniq
    | take 1000
    | each {|command| {value: $command}}
}

# Ctrl-R stays directory-local; Ctrl-Alt-R searches all directories.
$env.config.menus ++= [{
    name: history_menu
    only_buffer_difference: false
    marker: "? "
    type: {
        layout: list
        page_size: 10
    }
    style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
    }
    source: {|buffer, position|
        nu-history-suggestions (history --long) $buffer --cwd $env.PWD
    }
} {
    name: global_history_menu
    only_buffer_difference: false
    marker: "all? "
    type: {
        layout: list
        page_size: 10
    }
    style: {
        text: green
        selected_text: green_reverse
        description_text: yellow
    }
    source: {|buffer, position|
        nu-history-suggestions (history --long) $buffer
    }
}]
$env.config.keybindings ++= [{
    name: directory_history
    modifier: control
    keycode: char_r
    mode: [emacs vi_normal vi_insert]
    event: {until: [{send: menu name: history_menu} {send: menunext}]}
} {
    name: global_history
    modifier: control_alt
    keycode: char_r
    mode: [emacs vi_normal vi_insert]
    event: {until: [{send: menu name: global_history_menu} {send: menunext}]}
}]

# ---- Editor -----------------------------------------------------------
alias vim = nvim
alias vi = nvim
alias v = nvim

# ---- Git --------------------------------------------------------------
alias g = git
alias gcm = git commit -m
alias gcam = git commit -am
alias gca = git commit -a
alias gc = git commit
alias gco = git checkout
alias gcb = git checkout -b
alias gcd = git clone
alias gd = git diff
alias gdc = git diff --cached
alias gds = git diff --staged
alias gdt = git difftool
alias gl = git pull --tags --prune --ff-only
alias glg = git log
alias ga = git add
alias gp = git push
alias gs = git status
alias gf = git fetch --all --prune -f

def gpsup [] {
    let branch = (git symbolic-ref --short HEAD)
    git push --set-upstream origin $branch
}

def nah [] {
    git restore --staged .
    if $env.LAST_EXIT_CODE != 0 { return }
    git restore .
    if $env.LAST_EXIT_CODE != 0 { return }
    git clean -fd
}

# ---- Files ------------------------------------------------------------
alias l = ls
alias la = ls -la
alias ll = ls -la
alias lsa = ls -la

# ---- Go / Sublime -----------------------------------------------------
alias gg = go build -v ./...
alias ss = subl .

# ---- Directory shortcuts ---------------------------------------------
alias - = cd -
alias ... = cd ../..
alias .... = cd ../../..
alias ..... = cd ../../../..

# ---- Functions --------------------------------------------------------
def reload [] {
    exec nu
}

def wip [] {
    let result = (do { git symbolic-ref --short HEAD } | complete)
    let branch = ($result.stdout | str trim)
    if $result.exit_code != 0 or ($branch | is-empty) {
        error make {msg: "Not on a git branch."}
    }

    for args in [[add .] [commit -m wip] [push origin $branch]] {
        let step = (do { ^git ...$args } | complete)
        if not ($step.stdout | is-empty) { print --no-newline $step.stdout }
        if not ($step.stderr | is-empty) { print --stderr --no-newline $step.stderr }
        if $step.exit_code != 0 {
            error make {msg: $"git ($args.0) failed with exit code ($step.exit_code)."}
        }
    }
}

def profile [] {
    let startup = (timeit { nu --commands 'exit' })
    print $"Nushell startup: ($startup)"
}

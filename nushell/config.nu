# Nushell config mirroring fish/config.fish.

$env.config.show_banner = false

# ---- Editor -----------------------------------------------------------
alias vim = nvim
alias vi = nvim
alias v = nvim
alias opencode = opencode2

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
alias l = ls -la
alias la = ls -la
alias ll = ls
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
        print "Not on a git branch."
        return
    }

    git add .
    git commit -m "wip"
    git push origin $branch
}

def profile [] {
    let startup = (timeit { nu --commands 'exit' })
    print $"Nushell startup: ($startup)"
}

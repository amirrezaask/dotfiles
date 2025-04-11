export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/opt/homebrew/bin:/opt/homebrew/sbin"
export PATH="$PATH:$HOME/prg/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:/Applications/Emacs.app/Contents/MacOS"
export PATH="$PATH:/Applications/Emacs.app/Contents/MacOS/bin"

if command -v brew &>/dev/null
    eval (brew shellenv)
end

if command -v fzf &>/dev/null
    fzf --fish | source
end

if command -v brew &>/dev/null
   brew shellenv fish | source
end

if command -v nvim &>/dev/null
    alias vim='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
end

function oclogs -d "Read logs for given app name"
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
end

function fish_greeting
end

function fish_prompt
    printf '%s' (set_color green) (prompt_pwd) (set_color white) (fish_git_prompt) ' > '
end


function gwip
    set -l git_branch (git branch 2>/dev/null | sed -n '/\* /s///p')
    git add . && git commit -m "Work In Progress $(date +"%Y-%m-%d %H:%M:%S")" && git push origin $git_branch
end

function gd
    git diff
end

function ok
    git add . && git commit
end

function reload
    source ~/.config/fish/config.fish
end


function code
    if command -v cursor &>/dev/null
        cursor $argv
    else
        code $argv
    end
end

function emacs
    emacsclient -cn --alternate-editor='' $argv
end


alias gs='git status'

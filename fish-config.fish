export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/opt/homebrew/bin:/opt/homebrew/sbin"
export PATH="$PATH:$HOME/prg/bin"
export PATH="$PATH:/usr/local/bin"

# if ! command -v starship &>/dev/null
#   curl -sS https://starship.rs/install.sh | sh
# end

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
    printf '%s' (set_color cyan) (whoami) (set_color white) ' at ' (set_color green) (prompt_pwd) (set_color white) (fish_git_prompt) ' > '
end


function gwip
    set -l git_branch (git branch 2>/dev/null | sed -n '/\* /s///p')
    git add . && git commit -m "Work In Progress $(date +"%Y-%m-%d %H:%M:%S")" && git push origin $git_branch
end

function ok
    git add . && git commit
end


alias gs='git status'


# if command -v starship &>/dev/null
#   starship init fish | source
# end


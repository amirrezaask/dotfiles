export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/opt/homebrew/bin:/opt/homebrew/sbin"
export PATH="$PATH:$HOME/prg/bin"
export PATH="$PATH:/usr/local/bin"

# if ! command -v starship &>/dev/null
#   curl -sS https://starship.rs/install.sh | sh
# end
#
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

alias svpn="sudo openfortivpn --otp $(2fa ADS) --trusted-cert='f19a72523e24fff1dd45a1eacfce5dc9f6e5c5c460e4ceb0ac9dfc81c0228b42'"

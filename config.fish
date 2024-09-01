export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/opt/homebrew/bin:/opt/homebrew/sbin"
export PATH="$PATH:$HOME/prg/bin"

function fish_prompt -d "Write out the prompt"
    printf '%s %s%s%s > ' $USER \
        (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

#if ! command -v starship &>/dev/null
#   curl -sS https://starship.rs/install.sh | sh
#end

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

if command -v subl &>/dev/null
   alias s='subl .'
end

function oclogs -d "Read logs for given app name"
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
end

function fish_greeting
end

function fish_prompt
    printf '%s' (set_color green) (prompt_pwd) (set_color white) (fish_git_prompt) ' > '
end

alias svpn='sudo openfortivpn  --otp (totpgen ADS)'

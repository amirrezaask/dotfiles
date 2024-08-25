export GOPATH="$HOME/go"
export PATH="$PATH:$GOPATH/bin"
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/opt/homebrew/bin:/opt/homebrew/sbin"

function fish_prompt -d "Write out the prompt"
    printf '%s %s%s%s > ' $USER \
        (set_color $fish_color_cwd) (prompt_pwd) (set_color normal)
end

if ! command -v starship &>/dev/null
   curl -sS https://starship.rs/install.sh | sh
end

if command -v emacs &>/dev/null # use emacs as editor
    export EDITOR='emacs'
    export GIT_EDITOR='emacs'
end

if command -v fzf &>/dev/null
    fzf --fish | source
end
if command -v brew &>/dev/null
   brew shellenv fish | source
end

if command -v nvim &>/dev/null
    alias vim='nvim'
    alias v='nvim'
    alias vi='nvim'
    export GIT_EDITOR='nvim'
    export EDITOR='nvim'
end


function oclogs -d "Read logs for given app name"
    oc logs --prefix -f --selector "app.kubernetes.io/instance=snappdoctor-$1-prod, app.kubernetes.io/name=$1"
end

function fish_greeting
end

starship init fish | source

alias svpn='sudo openfortivpn  --otp (totpgen ADS)'

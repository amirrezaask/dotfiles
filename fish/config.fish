export GOPROXY='goproxy.io,direct'
export GOPATH="$HOME/go"
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"

if command -v nvim &> /dev/null
    alias vim='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
end

function e
    emacsclient -c -a '' $argv &
end

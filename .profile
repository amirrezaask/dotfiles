export GOPROXY='goproxy.io,direct'
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$(go env GOPATH)/bin:$PATH"
if command -v nvim &> /dev/null
then
    alias vim='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
fi

export GOPROXY='goproxy.io,direct'
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$(go env GOPATH)/bin:$PATH"
if command -v emacsclient &> /dev/null
then
    alias e='emacsclient -a "" -c'
    export EDITOR='e'
    export GIT_EDITOR='e'
fi

export GOPROXY='goproxy.io,direct'
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
if command -v code &> /dev/null
then
    alias code='code'
    export EDITOR='code -w'
    export GIT_EDITOR='code -w'
fi
if command -v codium &> /dev/null
then
    alias code='codium'
    export EDITOR='codium -w'
    export GIT_EDITOR='codium -w'
fi
if command -v emacsclient &> /dev/null
then
    alias e='emacsclient -c -a ""'
    export EDITOR='emacsclient -c -a ""'
    export GIT_EDITOR='emacsclient -c -a ""'
fi

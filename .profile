export GOPROXY='goproxy.io,direct'
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
if command -v emacs &> /dev/null
then
    export EDITOR='emacs'
    export GIT_EDITOR='emacs'
fi


export GOPROXY='goproxy.io,direct'
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$(go env GOPATH)/bin:$PATH"
if command -v codium &> /dev/null
then
    alias code='codium'
fi
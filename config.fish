export GOPROXY='goproxy.io,direct'
export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
if ! command -v starship &> /dev/null
    curl -sS https://starship.rs/install.sh | sh
end
if command -v nvim &> /dev/null
    alias vim='nvim'
end

starship init fish | source

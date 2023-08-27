export HOMEBREW_NO_AUTO_UPDATE=1
export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

export GO111MODULE='on'
export GOPATH="$HOME"
export GOPROXY='goproxy.io,direct'

export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH="/Users/amirreza/Library/Python/3.10/bin:$PATH"
export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
export PATH="$PATH:$HOME/dev/dotfiles/bin"

if command -v brew &> /dev/null
    eval (brew shellenv)
end

if command -v opam &> /dev/null
    eval (opam env)
end

if command -v nvim &> /dev/null
    export EDITOR=nvim
    alias vim=nvim
end

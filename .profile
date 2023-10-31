export HOMEBREW_NO_AUTO_UPDATE=1
export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'

export GO111MODULE='on'
export GOPATH="$HOME"
export GOPROXY='goproxy.io,direct'

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="$GOROOT/bin:$PATH"
export PATH="$HOME/.config/composer/vendor/bin:$PATH"
export PATH="/opt/homebrew/bin:$PATH"
export PATH="$GOPATH/bin:$PATH"
export PATH=$PATH:$HOME/dev/dotfiles/bin
export PATH=$PATH:$PLAN9/bin

[ -f "$HOME/.cargo/env" ] && . "$HOME/.cargo/env"
[[ ! -r /Users/amirreza/.opam/opam-init/init.zsh ]] || source /Users/amirreza/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

if command -v brew &> /dev/null
then
    eval $(brew shellenv)
fi

if command -v opam &> /dev/null
then
    eval $(opam env)
fi


if !command -v codium &> /dev/null
then
    echo "Install vscodium: https://github.com/VSCodium/vscodium/releases"
fi

alias code='codium'
export EDITOR='codium -w'
export GIT_EDITOR='codium -w'


if command -v emacsclient &> /dev/null
then
    export EDITOR='emacsclient -a "" '
    export GIT_EDITOR='emacsclient -a ""'
fi



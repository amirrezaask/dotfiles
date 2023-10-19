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
export PATH="/Users/amirreza/Library/Python/3.10/bin:$PATH"
export PATH="/Applications/Emacs.app/Contents/MacOS:$PATH"
export PLAN9=/Users/amirreza/plan9
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

# Mabna
alias mabna-up='sudo ipsec up corp'
alias mabna-down='sudo ipsec down corp'
alias mabna-dns='networksetup -setdnsservers Wi-Fi 192.168.10.1'
alias normal-dns='networksetup -setdnsservers Wi-Fi 8.8.8.8 4.2.2.4 '
alias shekan2-dns='networksetup -setdnsservers Wi-Fi 10.202.10.202 10.202.10.102 192.168.10.1'
alias shekan-dns='networksetup -setdnsservers Wi-Fi 178.22.122.100 185.51.200.2 192.168.10.1'

alias code='codium'
export EDITOR='codium -w'
export GIT_EDITOR='codium -w'



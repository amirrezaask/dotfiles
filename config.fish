set -x HOMEBREW_NO_AUTO_UPDATE 1
set -x FZF_DEFAULT_OPTS '--height 20%'
set -x FZF_DEFAULT_COMMAND 'rg --files'

set -x GO111MODULE 'on'
set -x GOPATH "$HOME"
set -x GOPRIVATE 'gitlab.snapp.ir'
set -x GOPROXY 'goproxy.io,direct'
set -x EDITOR 'vim'

set -x PATH "$HOME/.local/bin:$PATH"
set -x PATH "$HOME/.cargo/bin:$PATH"
set -x PATH "$GOROOT/bin:$PATH"
set -x PATH "$HOME/.config/composer/vendor/bin:$PATH"
set -x PATH "/opt/homebrew/bin:$PATH"
set -x PATH "$GOPATH/bin:$PATH"
set -x PATH "/Users/amirreza/Library/Python/3.10/bin:$PATH"
set -x PATH "/Applications/Emacs.app/Contents/MacOS:$PATH"
set -x PLAN9 /Users/amirreza/plan9
set -x PATH $PATH:$HOME/dev/dotfiles/bin
set -x PATH $PATH:$PLAN9/bin
set -g fish_greeting
if test -f "$HOME/.opam/opam-init/init.fish"
    source "$HOME/.opam/opam-init/init.fish"
end

if type -q "brew"
    eval (brew shellenv)
end

function mabna-up
    sudo ipsec up corp
end

function mabna-down
    sudo ipsec up corp
end

function mabna-dns
    switch (uname)
        case Linux
            sudo resolvectl dns (ip addr show | awk '/inet.*brd/{print $NF; exit}') 192.168.10.1
        case Darwin
            networksetup -setdnsservers Wi-Fi 192.168.10.1
    end
end

function shekan-dns
    switch (uname)
        case Darwin
            networksetup -setdnsservers Wi-Fi 178.22.122.100 185.51.200.2 192.168.10.
    end
end

if type -q nvim
    alias vim='nvim'
    alias vi='nvim'
    export EDITOR='nvim'
    export GIT_EDITOR='nvim'
end



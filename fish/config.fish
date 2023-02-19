set fish_greeting ""

set -gx GO111MODULE 'on'
set -gx GOPATH "$HOME"
set -gx GOPRIVATE "gitlab.snapp.ir"
set -gx GOPROXY 'goproxy.io,direct'

set -gx PATH "$HOME/.cargo/bin:/Applications/Emacs.app/Contents/MacOS:$GOPATH/bin:/opt/homebrew/bin:$ELIXIR/bin:$HOME/.luarocks/bin:$HOME/.config/composer/vendor/bin:$GOROOT/bin:$HOME/.cargo/bin:$HOME/.local/bin:$PATH:$HOME/.composer/vendor/bin"

set -gx EDITOR 'vim'
set -gx HOMEBREW_NO_AUTO_UPDATE '1'

set -gx FZF_DEFAULT_OPTS '--height 20%'
set -gx FZF_DEFAULT_COMMAND 'rg --files'

function ss_proxy
    set -gx http_proxy 'http://localhost:1087'
    set -gx https_proxy 'http://localhost:1087'
end

alias snappvpn='sudo openfortivpn -c ~/snappDC.conf'

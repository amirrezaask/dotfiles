set -x COMPOSERPATH ~/.composer/vendor/bin
set -x GOPATH ~/dev
set -x PYTHONPATH ~/.local/bin
set -x PATH $HOME/.cargo/bin:$HOME/.config/composer/vendor/bin:/usr/local/go/bin:$GOPATH/bin:$PYTHONPATH:/usr/local/Postman:$COMPOSERPATH:$PATH
set -x GO111MODULE on

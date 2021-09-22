$env:GOPROXY = "https://repo.snapp.tech/repository/goproxy,goproxy.io,direct"
$env:GOPRIVATE = "https://gitlab.snapp.ir"
if ($IsLinux) {
    $env:GOROOT = "/usr/local/go"
}
$env:GOPATH = "$HOME"
$env:DOTFILES = "~/src/github.com/amirrezaask/dotfiles/"
$env:PATH += ":/usr/local/go/bin:$HOME/.local/lua-language-server/bin/Linux:$env:GOPATH/bin:$HOME/.local/bin:$HOME/.config/composer/vendor/bin:$HOME/.cargo/bin"

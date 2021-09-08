$env:GOPROXY = "https://repo.snapp.tech/repository/goproxy,goproxy.io,direct"
$env:GOPRIVATE = "https://gitlab.snapp.ir"
$env:GOPATH = "$HOME"
$env:PATH += ":$env:GOPATH/bin:$HOME/.local/bin:$HOME/.config/composer/vendor/bin:$HOME/.cargo/bin"
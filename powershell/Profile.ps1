# notepad $PROFILE
#
# $BOOTSTRAP = "~/src/github.com/amirrezaask/dotfiles/powershell/Profile.ps1"
# . $BOOTSTRAP
# Install these ofcourse

Import-Module -Name PSReadLine
Import-Module -Name Terminal-Icons
Import-Module -Name oh-my-posh

# Prompt and stuff
Import-Module -Name posh-git
$GitPromptSettings.DefaultPromptAbbreviateHomeDirectory = $true
$GitPromptSettings.DefaultPromptPrefix.Text = '$(Get-Date -f "HH:mm:ss") '
$GitPromptSettings.DefaultPromptPrefix.ForegroundColor = [ConsoleColor]::Red
$GitPromptSettings.DefaultPromptPath.ForegroundColor = 'Orange'

Set-PoshPrompt rudolfs-dark

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Emacs

Set-PSReadLineKeyHandler -Chord 'Ctrl+d,Ctrl+c' -Function CaptureScreen
Set-PSReadLineKeyHandler -Key Alt+d -Function ShellKillWord
Set-PSReadLineKeyHandler -Key Alt+Backspace -Function ShellBackwardKillWord
Set-PSReadLineKeyHandler -Key Alt+b -Function ShellBackwardWord
Set-PSReadLineKeyHandler -Key Alt+f -Function ShellForwardWord
Set-PSReadLineKeyHandler -Key Alt+B -Function SelectShellBackwardWord
Set-PSReadLineKeyHandler -Key Alt+F -Function SelectShellForwardWord

function reload {
    . $PROFILE
}
function oss {
    Set-Location ~/src/github.com/amirrezaask 
}
function dots {
    Set-Location ~/src/github.com/amirrezaask/dotfiles
}
function snapp {
    Set-Location ~/src/gitlab.snapp.ir
}
function snappvpn {
    sudo openfortivpn -c ~/snappDC.conf
}

function freenet {
    Write-Output $env:VPN_PASSWORD | sudo openconnect --no-dtls --passwd-on-stdin --user $env:VPN_USERNAME $env:VPN_SERVER
}
function colors {

    $colors = [enum]::GetValues([System.ConsoleColor])
    Foreach ($bgcolor in $colors) {
        Foreach ($fgcolor in $colors) { Write-Host "$fgcolor|"  -ForegroundColor $fgcolor -BackgroundColor $bgcolor -NoNewLine }
        Write-Host " on $bgcolor"
    }
}

function gs { git status }
function e { explorer.exe . }

#TODO: maybe write a function to be alias for ls and if there were no arguments and flags run Get-ChileItem and if there was run /bin/ls


$env:GOPROXY = "https://repo.snapp.tech/repository/goproxy,goproxy.io,direct"
$env:GOPRIVATE = "https://gitlab.snapp.ir"
if ($IsLinux) {
    $env:GOROOT = "/usr/local/go"
}
$env:GOPATH = "$HOME"
$env:DOTFILES = "~/src/github.com/amirrezaask/dotfiles/"
$env:PATH += ":/usr/local/go/bin:$HOME/.local/lua-language-server/bin/Linux:$env:GOPATH/bin:$HOME/.local/bin:$HOME/.config/composer/vendor/bin:$HOME/.cargo/bin"
. $HOME/env.ps1

# notepad $PROFILE
#
# $Profile = "~/src/Personal/dotfiles/powershell/Profile.ps1"
# . $profile
# Install these ofcourse

Import-Module -Name PSReadLine
Import-Module -Name Terminal-Icons

# Prompt and stuff
Import-Module -Name posh-git
$GitPromptSettings.DefaultPromptAbbreviateHomeDirectory = $true
$GitPromptSettings.DefaultPromptPrefix.Text = '$(Get-Date -f "HH:mm:ss") '
$GitPromptSettings.DefaultPromptPrefix.ForegroundColor = [ConsoleColor]::Magenta
$GitPromptSettings.DefaultPromptPath.ForegroundColor = 'Orange'

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Emacs

$VARS = "$HOME/src/github.com/amirrezaask/dotfiles/powershell/Variables.ps1"
. $VARS

. $HOME/env.ps1

function reload {
    . $PROFILE
}
function oss {
    Set-Location ~/src/github.com/amirrezaask 
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

function gs { git status }
function e { explorer.exe . }

#TODO: maybe write a function to be alias for ls and if there were no arguments and flags run Get-ChileItem and if there was run /bin/ls

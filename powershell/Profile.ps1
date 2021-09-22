# notepad $PROFILE
#
# $Profile = "~/src/Personal/dotfiles/powershell/Profile.ps1"
# . $profile
# Install these ofcourse

Import-Module -Name posh-git
Import-Module -Name oh-my-posh
Import-Module -Name PSReadLine
Import-Module -Name Terminal-Icons
Import-Module -Name z


$PromptTheme = "gmay"

# Set Prompt
if ($IsWindows) {
    Set-PoshPrompt -Theme $PromptTheme
}
if ($IsLinux) {
    Set-PoshPrompt -Theme "~/.poshthemes/$PromptTheme.omp.json"
}

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Emacs

$VARS = "$HOME/src/github.com/amirrezaask/dotfiles/powershell/Variables.ps1"
. $VARS

. $HOME/env.ps1

function reload {
    . $PROFILE
}
function p {
    Set-Location ~/src/repos/Personal 
}
function s {
    Set-Location ~/src/repos/Snapp 
}
function snappvpn {
    sudo openfortivpn -c ~/snappDC.conf
}

function freenet {
    Write-Output $env:VPN_PASSWORD | sudo openconnect --no-dtls --passwd-on-stdin --user $env:VPN_USERNAME $env:VPN_SERVER
}

function gs { git status }

#TODO: maybe write a function to be alias for ls and if there were no arguments and flags run Get-ChileItem and if there was run /bin/ls

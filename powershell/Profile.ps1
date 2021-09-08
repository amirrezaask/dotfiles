# notepad $PROFILE
#
# $Profile = "~/src/Personal/dotfiles/powershell/Profile.ps1"
# . $profile
# Install these ofcourse

Import-Module -Name posh-git
Import-Module -Name oh-my-posh
Import-Module -Name PSReadLine
Import-Module -Name Terminal-Icons

# Set Prompt
Set-PoshPrompt marcduiker

$VARS = "$HOME/src/repos/Personal/dotfiles/powershell/Variables.ps1"
. $VARS

function reload {
    . $PROFILE
}
function p {
    Set-Location ~/src/repos/Personal 
}
function s {
    Set-Location ~/src/repos/Snapp 
}
function sn {
    sudo openfortivpn -c ~/snappDC.conf
}

function gs { git status }
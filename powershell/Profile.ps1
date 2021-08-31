# notepad $PROFILE
#
# $Profile = "~/source/repos/dotfiles/powershell/Profile.ps1"
# . $profile

function Install-And-Import($Name) {
    Set-ExecutionPolicy Unrestricted -Scope CurrentUser
    if (-Not (Get-Module -ListAvailable -Name $Name)) {
        Write-Host "Module does not exist" + $Name
        Install-Module $Name
    }
    Import-Module $Name
}

# install modules
Install-And-Import posh-git
Install-And-Import oh-my-posh
Install-And-Import PSReadLine
Install-And-Import -Name Terminal-Icons

# Set Prompt
Set-PoshPrompt marcduiker
# PSReadLine
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Windows

$VARS = "~/source/repos/github.com/amirrezaask/dotfiles/powershell/Variables.ps1"
. $VARS
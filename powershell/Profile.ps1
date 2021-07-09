function Install-And-Import($Name) {
    if (-Not (Get-Module -ListAvailable -Name $Name)) {
        Write-Host "Module does not exist" + $Name
        Install-Module $Name
    }
    Import-Module $Name
}
Install-And-Import posh-git
Install-And-Import oh-my-posh
Install-And-Import PSReadLine
Install-And-Import -Name Terminal-Icons

Set-ExecutionPolicy Unrestricted -Scope CurrentUser

Set-Theme Paradox

# PSReadLine
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Windows
function With-Module ($module){
    if (Get-Module -ListAvailable -Name $module) {
        Import-Module $module
    } 
    else {
        Install-Module $module
        Import-Module $module;
    }
}
With-Module PSReadLine
With-Module posh-git
With-Module oh-my-posh

Set-Theme Sorin
Set-Alias -Name dot -Value dotnet
Set-Alias rm Remove-Item -Force
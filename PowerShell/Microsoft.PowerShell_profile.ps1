Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Invoke-Expression (&starship init powershell)

Set-Alias vim nvim

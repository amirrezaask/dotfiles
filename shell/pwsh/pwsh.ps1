Import-Module PSReadLine
Import-Module posh-git
Set-PSReadLineOption -EditMode Emacs
# Set-PSReadLineOption -PredictionSource History
# Set-PSReadLineOption -PredictionViewStyle ListView
$PSStyle.FileInfo.Directory = ""

# if (Get-Command "starship" -ErrorAction SilentlyContinue) 
# {
#   $ENV:STARSHIP_CONFIG = "D:\src\dotfiles\shell\starship\starship.toml"
#   $ENV:STARSHIP_DISTRO = "者 Windows"
#   Invoke-Expression (&starship init powershell)
# }

New-Alias -Name gvim -Value neovide
New-Alias -Name vim -Value nvim

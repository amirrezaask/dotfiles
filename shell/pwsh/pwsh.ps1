$ENV:STARSHIP_CONFIG = "D:\projects\dotfiles\shell\starship\starship.toml"
Invoke-Expression (&starship init powershell)

Import-Module PSReadLine
Import-Module Terminal-Icons

Set-PSReadLineOption -EditMode Emacs

$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

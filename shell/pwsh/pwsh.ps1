Invoke-Expression (&starship init powershell)

Import-Module PSReadLine
Import-Module Terminal-Icons
# Import-Module posh-git


Set-PSReadLineOption -EditMode Emacs

$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

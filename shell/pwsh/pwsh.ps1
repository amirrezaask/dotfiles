Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Import-Module posh-git

$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs

$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}


Invoke-Expression (&starship init powershell)

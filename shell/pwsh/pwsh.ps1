Import-Module PSReadLine
Import-Module posh-git
Set-PSReadLineOption -EditMode Emacs

$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

# if (Get-Command "oh-my-posh" -ErrorAction SilentlyContinue) 
# {
#   oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH/amro.omp.json" | Invoke-Expression
# }

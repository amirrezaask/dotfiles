Import-Module PSReadLine
Import-Module posh-git
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView


$PSStyle.FileInfo.Directory = ""

$ChocolateyProfile = "$env:ChocolateyInstall\helpers\chocolateyProfile.psm1"
if (Test-Path($ChocolateyProfile)) {
  Import-Module "$ChocolateyProfile"
}

# if (Get-Command "oh-my-posh" -ErrorAction SilentlyContinue) 
# {
#   oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH/amro.omp.json" | Invoke-Expression
# }
if (Get-Command "starship" -ErrorAction SilentlyContinue) 
{
  $ENV:STARSHIP_CONFIG = "D:\src\dotfiles\shell\starship\starship.toml"
  $ENV:STARSHIP_DISTRO = "者 Windows"
  Invoke-Expression (&starship init powershell)
}

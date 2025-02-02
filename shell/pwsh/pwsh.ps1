Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadlineOption -BellStyle None

$PSStyle.FileInfo.Directory = ""
function reload() {
  . $PROFILE
}

if (Get-Command oh-my-posh -ErrorAction SilentlyContinue) {
  # oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH/amro.omp.json" | Invoke-Expression
  oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH/craver.omp.json" | Invoke-Expression
}

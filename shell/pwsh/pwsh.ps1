Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadlineOption -BellStyle None

$PSStyle.FileInfo.Directory = ""

# function prompt {
#   Write-Host -NoNewLine -ForegroundColor cyan "$($executionContext.SessionState.Path.CurrentLocation)>";
#   return " "
# }
if (Get-Command oh-my-posh -ErrorAction SilentlyContinue) {
  oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH/craver.omp.json" | Invoke-Expression
}

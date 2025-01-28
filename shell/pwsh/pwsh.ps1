Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadlineOption -BellStyle None

$PSStyle.FileInfo.Directory = ""

# if (Get-Command "starship" -ErrorAction SilentlyContinue) 
# {
#   Invoke-Expression (&starship init powershell)
# }

oh-my-posh init pwsh --config "$env:POSH_THEMES_PATH/craver.omp.json" | Invoke-Expression

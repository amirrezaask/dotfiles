Import-Module PSReadLine
Import-Module Terminal-Icons
Set-PSReadLineOption -EditMode Emacs
Set-PSReadlineOption -BellStyle None

$PSStyle.FileInfo.Directory = ""

if (Get-Command "starship" -ErrorAction SilentlyContinue) 
{
  Invoke-Expression (&starship init powershell)
}

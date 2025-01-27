Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
$PSStyle.FileInfo.Directory = ""

if (Get-Command "starship" -ErrorAction SilentlyContinue) 
{
  Invoke-Expression (&starship init powershell)
}

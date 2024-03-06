Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
function Invoke-Starship-PreCommand {
  $host.ui.RawUI.WindowTitle = "$pwd"
}
Invoke-Expression (&starship init powershell)

Set-Alias vim nvim

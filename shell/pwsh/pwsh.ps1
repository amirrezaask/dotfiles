Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadlineOption -BellStyle None

$PSStyle.FileInfo.Directory = ""

function prompt {
  "$($executionContext.SessionState.Path.CurrentLocation)> ";
}

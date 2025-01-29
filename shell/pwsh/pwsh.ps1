Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadlineOption -BellStyle None

$PSStyle.FileInfo.Directory = ""

function prompt {
  Write-Host -NoNewLine -ForegroundColor cyan "$($executionContext.SessionState.Path.CurrentLocation)>";
  return " "
}

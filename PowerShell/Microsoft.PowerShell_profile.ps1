if((Get-Module -ListAvailable -Name PSReadLine) -eq $null) {
    Install-Module -Scope CurrentUser PSReadLine
}

if((Get-Module -ListAvailable -Name Terminal-Icons) -eq $null) {
    Install-Module -Scope CurrentUser Terminal-Icons
}

if((Get-Module -ListAvailable -Name posh-git) -eq $null) {
    Install-Module -Scope CurrentUser posh-git
}

Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs

Import-Module -Name Terminal-Icons
Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Emacs

Set-PSReadLineKeyHandler -Chord 'Ctrl+d,Ctrl+c' -Function CaptureScreen
Set-PSReadLineKeyHandler -Key Alt+d -Function ShellKillWord
Set-PSReadLineKeyHandler -Key Alt+Backspace -Function ShellBackwardKillWord
Set-PSReadLineKeyHandler -Key Alt+b -Function ShellBackwardWord
Set-PSReadLineKeyHandler -Key Alt+f -Function ShellForwardWord
Set-PSReadLineKeyHandler -Key Alt+B -Function SelectShellBackwardWord
Set-PSReadLineKeyHandler -Key Alt+F -Function SelectShellForwardWord
Set-PSReadLineKeyHandler -Key Ctrl+LeftArrow ShellBackwardWord
Set-PSReadLineKeyHandler -Key Ctrl+RightArrow ShellForwardWord

Set-PSReadLineKeyHandler -Key Tab -Function Complete
Set-Alias vim nvim


function Invoke-Starship-PreCommand {
  $host.ui.RawUI.WindowTitle = "$pwd"
}


Invoke-Expression (&starship init powershell)

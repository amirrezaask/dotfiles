if((Get-Module -ListAvailable -Name PSReadLine) -eq $null) {
    Install-Module -Scope CurrentUser PSReadLine
}
Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs

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

# Aliases
Set-Alias vim nvim
Set-Alias subl 'C:\Program Files\Sublime Text\subl.exe'

function rgg {
    rg --vimgrep $args
}

function gs {
    git status
}

function ga {
    git add .
}

function gc {
    git commit
}

function reload {
    . $PROFILE
}

function prompt {
    "$(Get-Date -Format "yyyy/MM/dd-HH:mm")-$(Get-Location)> "
}

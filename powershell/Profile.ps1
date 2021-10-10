# notepad $PROFILE
#
# $BOOTSTRAP = "~/src/github.com/amirrezaask/dotfiles/powershell/Profile.ps1"
# . $BOOTSTRAP
# Install these ofcourse

Import-Module -Name PSReadLine
Import-Module -Name Terminal-Icons
Import-Module -Name oh-my-posh

# Prompt and stuff
Import-Module -Name posh-git
$GitPromptSettings.DefaultPromptAbbreviateHomeDirectory = $true
$GitPromptSettings.DefaultPromptPrefix.Text = '$(Get-Date -f "HH:mm:ss") '
$GitPromptSettings.DefaultPromptPrefix.ForegroundColor = [ConsoleColor]::Red
$GitPromptSettings.DefaultPromptPath.ForegroundColor = 'Orange'

Set-PoshPrompt craver

Set-PSReadLineOption -PredictionSource History
Set-PSReadLineOption -PredictionViewStyle ListView
Set-PSReadLineOption -EditMode Emacs

# CaptureScreen is good for blog posts or email showing a transaction
# of what you did when asking for help or demonstrating a technique.
Set-PSReadLineKeyHandler -Chord 'Ctrl+d,Ctrl+c' -Function CaptureScreen

# The built-in word movement uses character delimiters, but token based word
# movement is also very useful - these are the bindings you'd use if you
# prefer the token based movements bound to the normal emacs word movement
# key bindings.
Set-PSReadLineKeyHandler -Key Alt+d -Function ShellKillWord
Set-PSReadLineKeyHandler -Key Alt+Backspace -Function ShellBackwardKillWord
Set-PSReadLineKeyHandler -Key Alt+b -Function ShellBackwardWord
Set-PSReadLineKeyHandler -Key Alt+f -Function ShellForwardWord
Set-PSReadLineKeyHandler -Key Alt+B -Function SelectShellBackwardWord
Set-PSReadLineKeyHandler -Key Alt+F -Function SelectShellForwardWord

$VARS = "$HOME/src/github.com/amirrezaask/dotfiles/powershell/Variables.ps1"
. $VARS

. $HOME/env.ps1

function reload {
    . $PROFILE
}
function oss {
    Set-Location ~/src/github.com/amirrezaask 
}
function dots {
    Set-Location ~/src/github.com/amirrezaask/dotfiles
}
function snapp {
    Set-Location ~/src/gitlab.snapp.ir
}
function snappvpn {
    sudo openfortivpn -c ~/snappDC.conf
}

function freenet {
    Write-Output $env:VPN_PASSWORD | sudo openconnect --no-dtls --passwd-on-stdin --user $env:VPN_USERNAME $env:VPN_SERVER
}
function colors {

    $colors = [enum]::GetValues([System.ConsoleColor])
    Foreach ($bgcolor in $colors) {
        Foreach ($fgcolor in $colors) { Write-Host "$fgcolor|"  -ForegroundColor $fgcolor -BackgroundColor $bgcolor -NoNewLine }
        Write-Host " on $bgcolor"
    }
}

function gs { git status }
function e { explorer.exe . }

#TODO: maybe write a function to be alias for ls and if there were no arguments and flags run Get-ChileItem and if there was run /bin/ls

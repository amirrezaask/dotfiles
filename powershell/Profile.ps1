# notepad $PROFILE
#
# $Profile = "~/source/repos/github.com/amirrezaask/dotfiles/powershell/Profile.ps1"
# . $profile
# Install these ofcourse

Import-Module -Name posh-git
Import-Module -Name oh-my-posh
Import-Module -Name PSReadLine
Import-Module -Name Terminal-Icons

# Set Prompt
Set-PoshPrompt marcduiker

$VARS = "$HOME/source/repos/Personal/dotfiles/powershell/Variables.ps1"
. $VARS
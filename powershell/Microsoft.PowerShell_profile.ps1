# Proper history etc
Import-Module PSReadLine
Import-Module posh-git
Import-Module oh-my-posh
# Produce UTF-8 by default
# https://news.ycombinator.com/item?id=12991690
$PSDefaultParameterValues["Out-File:Encoding"] = "utf8"

# https://technet.microsoft.com/en-us/magazine/hh241048.aspx
$MaximumHistoryCount = 10000;

Set-Alias trash Remove-ItemSafely

function open($file) {
  invoke-item $file
}

function explorer {
  explorer.exe .
}

function edge {
  start microsoft-edge:
}
function settings {
  start-process ms-setttings:
}

# Oddly, Powershell doesn't have an inbuilt variable for the documents directory. So let's make one:
# From https://stackoverflow.com/questions/3492920/is-there-a-system-defined-environment-variable-for-documents-directory
$env:DOCUMENTS = [Environment]::GetFolderPath("mydocuments")

# PS comes preset with 'HKLM' and 'HKCU' drives but is missing HKCR 
New-PSDrive -Name HKCR -PSProvider Registry -Root HKEY_CLASSES_ROOT | Out-Null

# Truncate homedir to ~
function limit-HomeDirectory($Path) {
  $Path.Replace("$home", "~")
}

# Must be called 'prompt' to be used by pwsh 
# https://github.com/gummesson/kapow/blob/master/themes/bashlet.ps1
function prompt {
  & $GitPromptScriptBlock
  Return " "
}

Set-PSReadlineKeyHandler -Chord Tab -Function MenuComplete

# Like Unix touch, creates new files and updates time on old ones
# PSCX has a touch, but it doesn't make empty files

function touch($file) {
	if ( Test-Path $file ) {
		Set-FileTime $file
	} else {
		New-Item $file -type file
	}
}


function Invoke-Initialize() {
  Set-PSRepository -name PSGallery -InstallationPolicy Trusted
  Install-Module Pscx -Scope CurrentUser -AllowClobber
  Install-Module posh-git -Scope CurrentUser
  Install-Module oh-my-posh -Scope CurrentUser -AllowClobber

}
# From https://github.com/Pscx/Pscx
function sudo(){
	Invoke-Elevated @args
}

function reboot {
	shutdown /r /t 0
}

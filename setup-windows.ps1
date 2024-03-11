echo "You should run this in Administrator powershell."

winget import -i winget-export.json

Remove-Item -Force -Recurse $HOME\Documents\PowerShell
Remove-Item -Force -Recurse $Env:APPDATA\.emacs.d
Remove-Item -Force -Recurse $Env:LOCALAPPDATA\nvim
New-Item -Type SymbolicLink $HOME\Documents\PowerShell -Value C:\w\dotfiles\PowerShell\
New-Item -Type SymbolicLink $Env:APPDATA\.emacs.d -Value C:\w\dotfiles\emacs\
New-Item -Type SymbolicLink '$Env:APPDATA\Sublime Text\Packages\User' -Value C:\w\dotfiles\sublimetext\
New-Item -Type SymbolicLink $Env:LOCALAPPDATA\nvim -Value C:\w\dotfiles\nvim\

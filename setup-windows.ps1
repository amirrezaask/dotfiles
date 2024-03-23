echo "You should run this in Administrator powershell."

winget install -e --accept-package-agreements --accept-source-agreements --id="wez.wezterm",
winget install -e --accept-package-agreements --accept-source-agreements --id="OBSProject.OBSStudio",
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.Edge.Beta"
winget install -e --accept-package-agreements --accept-source-agreements --id="AlDanial.Cloc"
winget install -e --accept-package-agreements --accept-source-agreements --id="BurntSushi.ripgrep.MSVC"
winget install -e --accept-package-agreements --accept-source-agreements --id="Canonical.Ubuntu.2204"
winget install -e --accept-package-agreements --accept-source-agreements --id="Discord.Discord"
winget install -e --accept-package-agreements --accept-source-agreements --id="Docker.DockerDesktop"
winget install -e --accept-package-agreements --accept-source-agreements --id="Git.Git"
winget install -e --accept-package-agreements --accept-source-agreements --id="Google.Chrome"
winget install -e --accept-package-agreements --accept-source-agreements --id="Tonec.InternetDownloadManager"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.Edge"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.EdgeWebView2Runtime"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.AppInstaller"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.UI.Xaml.2.7"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.UI.Xaml.2.8"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VCLibs.Desktop.14"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.WindowsTerminal.Preview"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.WindowsTerminal"
winget install -e --accept-package-agreements --accept-source-agreements --id="Postman.Postman"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.Skype"
winget install -e --accept-package-agreements --accept-source-agreements --id="Valve.Steam"
winget install -e --accept-package-agreements --accept-source-agreements --id="JetBrains.Toolbox"
winget install -e --accept-package-agreements --accept-source-agreements --id="VideoLAN.VLC"
winget install -e --accept-package-agreements --accept-source-agreements --id="RARLab.WinRAR"
winget install -e --accept-package-agreements --accept-source-agreements --id="Obsidian.Obsidian"
winget install -e --accept-package-agreements --accept-source-agreements --id="cURL.cURL"
winget install -e --accept-package-agreements --accept-source-agreements --id="jqlang.jq"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VSTOR"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VCRedist.2010.x64"
winget install -e --accept-package-agreements --accept-source-agreements --id="Telegram.TelegramDesktop"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VCRedist.2008.x64"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.PowerShell.Preview"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VCRedist.2005.x86"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VisualStudioCode"
winget install -e --accept-package-agreements --accept-source-agreements --id="Starship.Starship"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VCRedist.2008.x86"
winget install -e --accept-package-agreements --accept-source-agreements --id="GoLang.Go"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.PowerShell"
winget install -e --accept-package-agreements --accept-source-agreements --id="OpenJS.NodeJS"
winget install -e --accept-package-agreements --accept-source-agreements --id="Fortinet.FortiClientVPN"
winget install -e --accept-package-agreements --accept-source-agreements --id="Microsoft.VCRedist.2010.x86"
winget install -e --accept-package-agreements --accept-source-agreements --id="Brave.Brave"

Remove-Item -Force -Recurse $HOME\Documents\PowerShell
Remove-Item -Force -Recurse $Env:APPDATA\.emacs.d
Remove-Item -Force -Recurse $Env:LOCALAPPDATA\nvim
New-Item -Type SymbolicLink $HOME\Documents\PowerShell -Value C:\w\dotfiles\PowerShell\
New-Item -Type SymbolicLink $Env:APPDATA\.emacs.d -Value C:\w\dotfiles\emacs\
New-Item -Type SymbolicLink '$Env:APPDATA\Sublime Text\Packages\User' -Value C:\w\dotfiles\sublimetext\
New-Item -Type SymbolicLink $Env:LOCALAPPDATA\nvim -Value C:\w\dotfiles\nvim\
New-Item -Type SymbolicLink $Env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminalPreview_8wekyb3d8bbwe\LocalState\settings.json  -Value C:\w\dotfiles\WindowsTerminal\settings.json
New-Item -Type SymbolicLink $Env:LOCALAPPDATA\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState\settings.json -Value C:\w\dotfiles\WindowsTerminal\settings.json

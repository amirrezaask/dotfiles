Import-Module posh-git
function Prompt { 
     $prompt = Write-Prompt ''
     $prompt += & $GitPromptScriptBlock
     return $prompt
 }

function paygearPythonProj() {
     Set-Location -Path $("D:\sources\paygear")
}

function paygearGoProj () {
     Set-Location -Path $($env:GOPATH + "\src\git.raad.cloud\cloud")
}
function amirrezaGoProj() {
     Set-Location -Path $($env:GOPATH + "\src\github.com\amirrezaask")
}
function vimRunner {
	wsl -e vim (wsl wslpath "'$args'")
}



Set-Alias -Name ppp -Value paygearPythonProj
Set-Alias -Name pgp -Value paygearGoProj
Set-Alias -Name vim -Value vimRunner
Set-Alias -Name agp -Value amirrezaGoProj
Set-PSReadlineKeyHandler -Key Tab -Function Complete




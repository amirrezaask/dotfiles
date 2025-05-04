#!/usr/bin/env bash
extensions=(
	'bmewburn.vscode-intelephense-client'
	'golang.go'
	'ms-vscode-remote.remote-wsl'
	'redhat.vscode-yaml'
	'rust-lang.rust-analyzer'
	'supermaven.supermaven'
	'usernamehw.errorlens'
	'vscodevim.vim'
)

for line in "${extensions[@]}"; do
	$1 --install-extension "$line"
done;


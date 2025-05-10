#!/usr/bin/env bash
vscode_extensions=(
	'bmewburn.vscode-intelephense-client'
	'golang.go'
	'ms-vscode-remote.remote-wsl'
	'redhat.vscode-yaml'
	'rust-lang.rust-analyzer'
	'supermaven.supermaven'
	'vscodevim.vim'
	'anweber.vscode-httpyac'
)

cursor_extensions=(
	'bmewburn.vscode-intelephense-client'
	'golang.go'
	'redhat.vscode-yaml'
	'rust-lang.rust-analyzer'
	'supermaven.supermaven'
	'vscodevim.vim'
	'anweber.vscode-httpyac'
)
echo ">>> Installing code extensions..."

for line in "${vscode_extensions[@]}"; do
	code --install-extension "$line"
done;

echo ">>> Installing cursor extensions..."

for line in "${cursor_extensions[@]}"; do
	cursor --install-extension "$line"
done;

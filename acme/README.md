# ACME
My collection of scripts for using ACME as my dev environment.

# Features
- Working LSP for Go, PHP
- F to fuzzy find using rg and fzf
- G for greping
- O for opening any path in current window
- Music controller using ryhthmbox-client
- Vol+/- manage system sound level
- Slide show
- handy git scripts ( ga, gc, gp, gd, gs)
- Some go snippets ( Goforr, Gofor, iferr )
- desktop file to have acme in your applications menu

# Scripts
- a: Acme launcher, sets font and runs plumber
- C: chtsh.sh shortcut, usage: C golang append to slice
- D: Documentation of something based on open file, for Golang is go doc $@
- F: Fuzzy finder using fzf and Rg
- G: Grep using multiple tools ( rg, git grep, grep )
- O: open the path in current window
- Tags: generate list of tags, both general tags like for LSP and also language specific ones
- Slide+: next slide in current dir, should be in a slide now.
- Slide-: prev slide in current dir, should be in a slide now.

## TO INSTALL
- go install github.com/eaburns/Watch@latest
- go install golang.org/x/tools/cmd/goimports@latest
- go install github.com/fhs/acme-lsp/cmd/acme-lsp@latest
- go install github.com/fhs/acme-lsp/cmd/L@latest
- go install git.sr.ht/~mkhl/xplor
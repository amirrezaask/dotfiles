let g:LanguageClient_autoStart = 1

let g:LanguageClient_serverCommands = {
    \ 'rust': ['~/.cargo/bin/rustup', 'run', 'stable', 'rls'],
    \ 'javascript': ['/usr/local/bin/javascript-typescript-stdio'],
    \ 'javascript.jsx': ['tcp://127.0.0.1:2089'],
    \ 'python': ['/usr/local/bin/pyls'],
    \ 'ruby': ['~/.rbenv/shims/solargraph', 'stdio'],
    \ 'go': ["$GOPATH/bin/gopls"],
    \ 'php': ['intelephense', '--stdio']
    \ }

nmap <F1> <Plug>(lcn-menu)

nmap <silent> gd <Plug>(lcn-definition)
nmap <silent> <F6> <Plug>(lcn-rename)

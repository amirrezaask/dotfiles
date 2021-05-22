" Extracted from https://github.com/tjdevries/gruvbuddy.nvim

" Vim editor {{{
    if g:transparent
      highlight Normal guibg=none
    else
      highlight Normal guibg=#282c34 guifg=#e0e0e0
    endif
    highlight InvNormal guibg=#c5c8c6 guifg=#282c34
    highlight NormalFloat guibg=#111317 guifg=#fafafa
    if g:transparent
        highlight LineNr guibg=none guifg=#e0e0e0
    else
        highlight LineNr guibg=#282a2e guifg=#969896
    endif
    highlight EndOfBuffer guifg=#969896
    highlight SignColumn guibg=#282a2e guifg=#969896
    highlight Visual guibg=#38556d
    highlight VisualMode guibg=#38556d
    highlight VisualLineMode guibg=#38556d
    highlight Cursor guibg=#e0e0e0 guifg=#282c34
    highlight CursorLine guibg=#333842
    highlight PMenu guibg=#373b41 guifg=#b4b7b4
    highlight PMenuSel guibg=#fbffad guifg=#282c34
    highlight PMenuSbar guibg=#282c34
    highlight PMenuThumb guibg=#b4b7b4
    highlight Search guibg=#f8fe7a guifg=#282a2e
    highlight TabLine guibg=#282a2e guifg=#5f89ad
    highlight TabLineFill guibg=#969896 guifg=#ebdbb2
    highlight TabLineSel guibg=#282a2e guifg=#ffffff
    highlight ColorColumn guibg=#3131bf guifg=#ffffff
" }}}

" Quickfix {{{
    highlight qfFileName guifg=#f8fe7a
" }}}

" Specials {{{
    highlight Special guifg=#aa92cd
    highlight SpecialChar guifg=#a3685a
    highlight NonText guifg=#4e545c
    highlight WhiteSpace guifg=#8e6fbd
    highlight Conceal guibg=#4e545c guifg=#282c34
" }}}

" Statusline {{{
    highlight StatusLine cterm=none gui=none guibg=#81a2be guifg=#373b41
    highlight StatusLineNC cterm=none gui=none guibg=#3f4349 guifg=#969896
    highlight User1 guibg=#f8fe7a guifg=#ffffff
    highlight User2 guibg=#cc6666 guifg=#ffffff
    highlight User3 guibg=#99cc99 guifg=#ffffff
    highlight CommandMode guibg=#99cc99 guifg=#ffffff
    highlight NormalMode guibg=#cc6666 guifg=#ffffff
    highlight InsertMode guibg=#f8fe7a guifg=#ffffff
    highlight ReplaceMode guibg=#f8fe7a guifg=#ffffff
    highlight TerminalMode guibg=#698b69 guifg=#ffffff
" }}}

" Standard Syntax {{{
    highlight Boolean guifg=#de935f
    highlight Comment guifg=#969896 gui=italic
    highlight Character guifg=#cc6666
    highlight Conditional guifg=#cc6666
    highlight Define guifg=#8abeb7
    highlight Error guifg=#d98c8c
    highlight Number guifg=#cc6666
    highlight Float guifg=#cc6666
    highlight Constant guifg=#de935f
    highlight Identifier guifg=#cc6666
    highlight Include guifg=#8abeb7
    highlight Keyword guifg=#b294bb
    highlight Label guifg=#f8fe7a
    highlight Operator guifg=#e6b3b3
    highlight PreProc guifg=#f8fe7a
    highlight Repeat guifg=#cc6666
    highlight Repeat guifg=#cc6666
    highlight Statement guifg=#c04040
    highlight StorageClass guifg=#f8fe7a
    highlight String guifg=#99cc99
    highlight Structure guifg=#b294bb
    highlight Tag guifg=#f8fe7a
    highlight Todo guifg=#f8fe7a
    highlight Typedef guifg=#f8fe7a
    highlight Type guifg=#b294bb
    highlight Folded guibg=#4e545c guifg=#7c7f7c
    highlight Function guifg=#f8fe7a
    highlight pythonBuiltinFunc guifg=#f8fe7a
    highlight vimFunction guifg=#f8fe7a
    highlight MatchParen guifg=#8abeb7
" }}}

" Help {{{
    highlight HelpDoc guibg=#698b69 guifg=#ffffff
    highlight HelpIgnore guifg=#99cc99
" }}}

" Git diff {{{
    highlight gitDiff guifg=#c7c7c7
    highlight DiffChange guibg=#2800
    highlight DiffText guibg=#8e00
    highlight DiffAdd guibg=#2800
    highlight DiffDelete guibg=#0
    highlight DiffRemoved guifg=#cc6666
    highlight DiffAdded guifg=#99cc99
" }}}

" Treesitter {{{
    highlight TSBoolean guifg=#3fffff
    highlight TSCharacter guifg=#99cc99
    highlight TSComment guifg=#969896 gui=italic
    highlight TSConditional guifg=#f8fe8a gui=bold
    highlight TSConstant guifg=#cc6666
    highlight TSInclude guifg=#8abeb7
    highlight TSConstant guifg=#81a2be
    highlight TSVariable guifg=#e0e0e0
    highlight TSFunction guifg=#f8fe7a gui=bold,italic
    highlight TSVariableBuiltin guifg=#f8fe7a
    highlight TSKeyword guifg=#b294bb gui=italic,bold
    highlight TSLabel guifg=#cc6666
" }}}

" VimWiki {{{
    highlight VimwikiBold guifg=#cc6666
" }}}

" Telescope {{{
    highlight TelescopeMatching guifg=#f2904b gui=bold
" }}}

" Startify {{{
    highlight StartifyBracket guifg=#cc6666
    highlight StartifyFile guifg=#c04040
    highlight StartifyNumber guifg=#81a2be
    highlight StartifyPath guifg=#77bb77
    highlight StartifySlash guifg=#8abeb7
    highlight StartifySection guifg=#fbffad
    highlight StartifySpecial guifg=#de935f
    highlight StartifyHeader guifg=#de935f
    highlight StartifyFooter guifg=#373b41
" }}}

" fold {{{
    highlight foldbraces guifg=#f2e5bc
" }}}

" markdown {{{
    highlight markdownH1 guifg=#81a2be
    highlight markdownH2 guifg=#a3bbd0
    highlight markdownH3 guifg=#c5d4e1
" }}}

" vimscript {{{
    highlight vimNotFunc guifg=#81a2be
    highlight vimCommand guifg=#81a2be
    highlight vimLet guifg=#aa92cd
    highlight vimFuncVar guifg=#8e6fbd
    highlight vimCommentTitle guifg=#cc6666
    highlight vimIsCommand guifg=#aa92cd
    highlight vimMapModKey guifg=#8abeb7
    highlight vimNotation guifg=#8abeb7
    highlight vimMapLHS guifg=#f8fe7a
    highlight vimNotation guifg=#8abeb7
    highlight vimBracket guifg=#96535c
    highlight vimMap guifg=#698b69
    highlight nvimMap guifg=#698b69
    highlight vimAutoloadFunction guifg=#f3fe14
" }}}

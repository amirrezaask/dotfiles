local ls = require"luasnip"

local parse = ls.parser.parse_snippet

vim.cmd [[
  imap <silent><expr> <c-k> luasnip#expand_or_jumpable() ? '<Plug>luasnip-expand-or-jump' : '<c-k>'
  inoremap <silent> <c-j> <cmd>lua require'luasnip'.jump(-1)<Cr>

  snoremap <silent> <c-k> <cmd>lua require('luasnip').jump(1)<Cr>
  snoremap <silent> <c-j> <cmd>lua require('luasnip').jump(-1)<Cr>

]]


ls.snippets = {
  all = {

  },
  go = {
    parse("iferr", "if ${1:err} != nil {\n\t$2\n}")
  }
}


vim.map {
  ['n <CR>'] = function()
    if vim.v.hlsearch == 1 then
      vim.c.nohl()
    end
    vim.fn.feedkeys("<CR>")
  end,

  -- Disable arrows in insert :)
  ['i <Up>']    = '<C-o>:echom "You dumb? use k"<CR>',
  ['i <Down>']  = '<C-o>:echom "You dumb? use j"<CR>',
  ['i <Right>'] = '<C-o>:echom "You dumb? use l"<CR>',
  ['i <Left>']  = '<C-o>:echom "You dumb? use h"<CR>',

  -- Splits movement
  ['<C-j>'] = '<C-w>j',
  ['<C-k>'] = '<C-w>k',
  ['<C-l>'] = '<C-w>l',
  ['<C-h>'] = '<C-w>h',

  Q = '<nop>',
  [';'] = ':',

  -- Resizes
  ['<Left>']  = ':vertical resize -5<CR>',
  ['<Right>'] = ':vertical resize +5<CR>',
  ['<Up>']    = ':resize +5<CR>',
  ['<Down>']  = ':resize -5<CR>',

  -- ??? why not working ?
  ['<C-.>'] = ':vertical resize +5<CR>',
  ['<C-,>'] = ':vertical resize -5<CR>',
  ["<C-'>"] = ':resize +5<CR>',
  ['<C-;>'] = ':resize -5<CR>',

  -- Tabs stuff
  ['<M-l>'] = ':tabprevious<CR>',
  ['<M-h>'] = ':tabnext<CR>',
  ['<M-q>'] = ':tabclose<CR>',
  ['<M-t>'] = ':tabnew<CR>',

  ['n j'] = 'gj',
  ['n k'] = 'gk',

  -- Terminal mode
  ['t <Esc>'] = '<C-\\><C-n>',
  ['t jk'] = '<C-\\><C-n>',
  ['t kj'] = '<C-\\><C-n>',

  -- Fast <ESC>
  ['i jk'] = '<esc>',
  ['i kj'] = '<esc>',

  -- Move lines jetbrains style -> Thanks to TJ again
  ['n <M-j>'] = ":m .+1<CR>==",
  ['n <M-k>'] = ":m .-2<CR>==",
  ['i <M-j>'] = "<Esc>:m .+1<CR>==gi",
  ['i <M-k>'] = "<Esc>:m .-2<CR>==gi",
  ['v <M-j>'] = ":m '>+1<CR>gv=gv",
  ['v <M-k>'] = ":m '<-2<CR>gv=gv",
}

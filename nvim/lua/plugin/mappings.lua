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
  [';'] = {':', "Map semicolon to colon"},

  -- Resizes
  ['<Left>']  = { ':vertical resize -5<CR>', "Decreases window width", "Window" },
  ['<Right>'] = { ':vertical resize +5<CR>', "Increases window width", "Window" },
  ['<Up>']    = { ':resize +5<CR>', "Decreases window height", "Window" },
  ['<Down>']  = { ':resize -5<CR>', "Increases window height", "Window" },

  -- Window resizes
  ['<M-.>'] = { ':vertical resize +5<CR>', "Increases window width", "Window" },
  ['<M-,>'] = { ':vertical resize -5<CR>', "Decreases window width", "Window" },
  ["<M-'>"] = { ':resize +5<CR>', "Decreases window height", "Window" },
  ['<M-;>'] = { ':resize -5<CR>', "Decreases window height", "Window" },

  -- Tabs stuff
  ['<M-l>'] = { ':tabprevious<CR>', "Previous Tab", "Tabs" },
  ['<M-h>'] = { ':tabnext<CR>', "Next tab", "Tabs" },
  ['<M-t>'] = { ':tabnew<CR>', "New Tab", "Tabs" },

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
  ['n <M-j>'] = { ":m .+1<CR>==", "Move current line down", "Editing" },
  ['n <M-k>'] = { ":m .-2<CR>==", "Move current line up", "Editing" },
  ['i <M-j>'] = { "<Esc>:m .+1<CR>==gi", "Move current line down", "Editing" },
  ['i <M-k>'] = { "<Esc>:m .-2<CR>==gi", "Move current line up", "Editing" },
  ['v <M-j>'] = { ":m '>+1<CR>gv=gv", "Move selected lines down", "Editing" },
  ['v <M-k>'] = { ":m '<-2<CR>gv=gv", "Move selected lines up", "Editing" },
}

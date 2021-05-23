-- vim.g.startify_disable_at_vimenter = 1
vim.g.startify_change_to_dir = 1

vim.g.startify_lists = {
  { ['type'] = 'files',     ['header'] = {'   MRU'}            },
  -- { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
  { ['type'] = 'sessions',  ['header'] = {'   Sessions'}       },
  -- { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
  { ['type'] = 'commands',  ['header'] = {'   Commands'}       },
}

vim.g.startify_skiplist = {
  'COMMIT_EDITMSG',
}

vim.map {
  ['<M-s>'] = function()
    vim.cmd(':SSave ' .. vim.fn.input('Session Name: '))
  end,
  ['<M-l>'] = ':SLoad '
}

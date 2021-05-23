-- vim.g.startify_disable_at_vimenter = 1
vim.g.startify_change_to_dir = 1

vim.g.startify_lists = {
  { ['type'] = 'files',     ['header'] = {'   MRU'}            },
  -- { 'type': 'dir',       'header': ['   MRU '. getcwd()] },
  { ['type'] = 'sessions',  ['header'] = {'   Sessions'}       },
  -- { 'type': 'bookmarks', 'header': ['   Bookmarks']      },
  -- { 'type': 'commands',  'header': ['   Commands']       },
}

vim.g.startify_skiplist = {
  'COMMIT_EDITMSG',
}

vim.g.startify_custom_footer = {'', "Best thing since pink floyd", ''}

vim.map {
  ['<M-s>'] = function()
    vim.cmd(':SSave ' .. vim.fn.input('Session Name: '))
  end,
  ['<M-l>'] = ':SLoad '
}

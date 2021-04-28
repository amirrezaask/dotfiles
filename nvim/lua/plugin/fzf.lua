local normal_maps = {}
local nvim = require('amirrezaask.nvim')

vim.g.fzf_layout = { down = '40%' }
normal_maps['<leader><leader>'] = '<cmd>Files<CR>'
normal_maps['<leader>ec'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles<CR>'
normal_maps['<leader>en'] = '<cmd>Files /home/amirreza/.config/nvim<CR>'
normal_maps['<leader>ez'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles/zsh<CR>'
normal_maps['<leader>fp'] = '<cmd>Files /home/amirreza/.local/share/nvim/site/pack/packer/start<CR>'
normal_maps['<leader>gf'] = '<cmd>GFiles<CR>'
normal_maps['<C-p>'] = '<cmd>GFiles<CR>'
normal_maps['<leader>fr'] = '<cmd>MRU<CR>'
normal_maps['<leader>pf'] = '<cmd>lua Projects({"/home/amirreza/src"})<CR>'
normal_maps['??'] = '<cmd>Rg<CR>'
normal_maps['<leader>b'] = '<cmd>Buffers<CR>'
normal_maps['<leader>c'] = '<cmd>Commands<CR>'
normal_maps['<leader>fh'] = '<cmd>History<CR>'
normal_maps['<leader>h'] = '<cmd>Helptags<CR>'
normal_maps['<leader>gc'] = '<cmd>Commits<CR>'
normal_maps['<leader>gb'] = '<cmd>BCommits<CR>'
normal_maps['<leader>gs'] = '<cmd>GitFiles?<CR>'

function FZF(opts)
  opts = opts or {}
  opts.down = opts.down or '40%'
  vim.fn.call('fzf#run', { opts })
end

function MRU()
  FZF({
    source = vim.split(vim.fn.execute('oldfiles'), '\n'),
    sink = function(file)
      vim.cmd(string.format('e %s', vim.split(file, ':')[2]))
    end,
  })
end

nvim.command('MRU', MRU)

require('amirrezaask.nvim').mode_map({
  n = normal_maps,
})

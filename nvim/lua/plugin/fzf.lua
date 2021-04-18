local normal_maps = {}
local nvim = require('amirrezaask.nvim')

vim.g.fzf_layout = { down = '40%' }
normal_maps['<Space><Space>'] = '<cmd>Files<CR>'
normal_maps['<Space>ec'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles<CR>'
normal_maps['<Space>en'] = '<cmd>Files /home/amirreza/.config/nvim<CR>'
normal_maps['<Space>ez'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles/zsh<CR>'
normal_maps['<Space>fp'] = '<cmd>Files /home/amirreza/.local/share/nvim/site/pack/packer/start<CR>'
normal_maps['<Space>gf'] = '<cmd>GFiles<CR>'
normal_maps['<C-p>'] = '<cmd>GFiles<CR>'
normal_maps['<Space>fr'] = '<cmd>MRU<CR>'
normal_maps['<Space>pf'] = '<cmd>lua Projects({"/home/amirreza/src"})<CR>'
normal_maps['??'] = '<cmd>Rg<CR>'
normal_maps['<Space>b'] = '<cmd>Buffers<CR>'
normal_maps['<Space>c'] = '<cmd>Commands<CR>'
normal_maps['<Space>fh'] = '<cmd>History<CR>'
normal_maps['<Space>h'] = '<cmd>Helptags<CR>'
normal_maps['<Space>gc'] = '<cmd>Commits<CR>'
normal_maps['<Space>gb'] = '<cmd>BCommits<CR>'
normal_maps['<Space>gs'] = '<cmd>GitFiles?<CR>'

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

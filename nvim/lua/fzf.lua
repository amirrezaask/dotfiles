local normal_maps = {}

normal_maps['<Space><Space>'] = '<cmd>Files<CR>'
normal_maps['<Space>ec'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles<CR>'
normal_maps['<Space>en'] = '<cmd>Files /home/amirreza/.config/nvim<CR>'
normal_maps['<Space>ez'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles/zsh<CR>'
normal_maps['<Space>fp'] = '<cmd>Files /home/amirreza/.local/share/nvim/site/pack/packer/start<CR>'
normal_maps['<Space>gf'] = '<cmd>GFiles<CR>'
normal_maps['<C-p>'] = '<cmd>GFiles<CR>'
-- normal_maps['<Space>fr'] = '<cmd>lua require"fuzzy".mru{}<CR>'
-- normal_maps['<Space>pf'] = '<cmd>lua require("fuzzy").projects{locations={"/home/amirreza/src"}}<CR>'
normal_maps['??'] = '<cmd>Rg<CR>'
normal_maps['<Space>b'] = '<cmd>Buffers<CR>'
normal_maps['<Space>c'] = '<cmd>Commands<CR>'
normal_maps['<Space>h'] = '<cmd>History<CR>'
normal_maps['<Space>h'] = '<cmd>Helptags<CR>'
normal_maps['<Space>gc'] = '<cmd>Commits<CR>'
normal_maps['<Space>gb'] = '<cmd>BCommits<CR>'
vim.g.fzf_layout = { window = { width = 0.9, height = 0.6 } }

vim.g.fzf_layout = { down = '40%' }
require'nvim'.mode_map({
  n = normal_maps
})


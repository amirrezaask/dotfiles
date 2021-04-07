local normal_maps = {}

-- Fuzzy Searching
vim.g.fuzzy_options = {
  width = 40,
  height = 100,
  blacklist = {
    "vendor"
  },
  prompt = '> '
}

-- Fuzzy.nvim
normal_maps['<Space><Space>'] = '<cmd>lua require("fuzzy").find_files{}<CR>'
normal_maps['<Space>fb'] = '<cmd>lua require("fuzzy").interactive_finder{}<CR>'
normal_maps['<Space>ec'] = '<cmd>lua require("fuzzy").find_files{path="/home/amirreza/src/github.com/amirrezaask/dotfiles"}<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("fuzzy").find_files{path="/home/amirreza/.config/nvim"}<CR>'
normal_maps['<Space>fp'] = '<cmd>lua require("fuzzy").find_files{path="/home/amirreza/.local/share/nvim/site/pack/packer/start"}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("fuzzy.git").git_files{}<CR>'
normal_maps['<Space>fr'] = '<cmd>MRU<CR>'
normal_maps['<Space>pf'] = '<cmd>lua require("fuzzy").projects{locations={"/home/amirreza/src"}}<CR>'
normal_maps['??'] = '<cmd>lua require("fuzzy").grep{}<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("fuzzy").buffers{}<CR>'
normal_maps['<Space>gg'] = '<cmd>lua require("fuzzy.git").git_grep{}<CR>'
normal_maps['<Space>c'] = '<cmd>lua require("fuzzy").commands{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").history{}<CR>'
normal_maps['<Space>s'] = '<cmd>lua require("sidetree").open_side_file_browser()<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("fuzzy").help{}<CR>'
normal_maps['<Space>gc'] = '<cmd>lua require("fuzzy.git").git_commits{}<CR>'
normal_maps['<Space>gb'] = '<cmd>lua require("fuzzy.git").git_bcommits{}<CR>'
normal_maps['<Space>gco'] = '<cmd>lua require("fuzzy.git").git_checkout{}<CR>'

require'nvim'.mode_map({
  n = normal_maps
})

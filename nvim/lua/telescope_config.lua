local normal_maps = {}

require('telescope').setup {
  defaults = {
    prompt_prefix = '❯ ',
    selection_caret = '❯ ',
    winblend = 0,
    preview_cutoff = 120,
    layout_strategy = "horizontal"
  }
}
local M = {}
require('telescope').load_extension('fzy_native')
require('telescope').load_extension('dap')
function M.edit_configs()
  require'telescope.builtin'.find_files {
    prompt_title = "> Edit Configs <",
    cwd = "~/src/github.com/amirrezaask/dotfiles"
  }
end

function M.edit_neovim()
  require'telescope.builtin'.find_files {
    prompt_title = "> Edit Neovim Config <",
    cwd = "~/src/github.com/amirrezaask/dotfiles/nvim"
  }
end

function M.edit_zsh()
  require'telescope.builtin'.find_files {
    prompt_title = "> Edit ZSH Config <",
    cwd = "~/src/github.com/amirrezaask/dotfiles/zsh"
  }
end


normal_maps['<Space><Space>'] = '<cmd>lua require("telescope.builtin").find_files{}<CR>'
normal_maps['<Space>fb'] = '<cmd>lua require("telescope.builtin").file_browser{}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("telescope.builtin").git_files{}<CR>'
normal_maps['<C-p>'] = '<cmd>lua require("telescope.builtin").git_files{}<CR>'
normal_maps['??'] = '<cmd>lua require("telescope.builtin").live_grep{}<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("telescope.builtin").buffers{}<CR>'
normal_maps['<Space>ec'] = '<cmd>lua require("telescope_config").edit_configs()<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("telescope_config").edit_neovim()<CR>'
normal_maps['<Space>ez'] = '<cmd>lua require("telescope_config").edit_zsh()<CR>'
normal_maps['<Space>c'] = '<cmd>lua require("telescope.builtin").commands{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("telescope.builtin").history{}<CR>'
normal_maps['<Space>fr'] = '<cmd>lua require("telescope.builtin").oldfiles{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("telescope.builtin").help_tags{}<CR>'
normal_maps['<Space>gc'] = '<cmd>lua require("telescope.builtin").git_commits{}<CR>'
normal_maps['<Space>gb'] = '<cmd>lua require("telescope.builtin").git_bcommits{}<CR>'
normal_maps['<Space>go'] = '<cmd>lua require("telescope.builtin").git_checkout{}<CR>'
normal_maps['<Space>gs'] = '<cmd>lua require("telescope.builtin").git_status{}<CR>'
normal_maps['<Space>tf'] = '<cmd>lua require("telescope.builtin").treesitter{}<CR>'
normal_maps['<Space>lr'] = '<cmd>lua require("telescope.builtin").lsp_references{}<CR>'
normal_maps['<Space>ld'] = '<cmd>lua require("telescope.builtin").lsp_document_symbols{}<CR>'
normal_maps['<Space>lw'] = '<cmd>lua require("telescope.builtin").lsp_workspace_symbols{}<CR>'
normal_maps['<Space>lc'] = '<cmd>lua require("telescope.builtin").lsp_code_actions{}<CR>'


require'nvim'.mode_map({
  n = normal_maps
})

return M

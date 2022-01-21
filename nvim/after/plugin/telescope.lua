local telescope_actions = require "telescope.actions"

-- Telescope
require("telescope").setup {
  defaults = {
    layout_strategy = "flex",
    layout_config = {
      width = 0.9,
      height = 0.8,

      horizontal = {
        width = { padding = 0.15 },
      },
      vertical = {
        preview_height = 0.75,
      },
    },
    file_ignore_patterns = {
      "__pycache__",
    },
    mappings = {
      n = {
        ["<ESC>"] = telescope_actions.close,
        ["<C-c>"] = telescope_actions.close,
        ["jk"] = telescope_actions.close,
        ["kj"] = telescope_actions.close,
      },
      i = {
        ["<C-c>"] = telescope_actions.close,
        ["<C-q>"] = telescope_actions.send_to_qflist,
        ["<C-j>"] = telescope_actions.move_selection_next,
        ["<C-k>"] = telescope_actions.move_selection_previous,
      },
    },
  },
}

require("telescope").load_extension "harpoon"

vim.cmd [[
  nnoremap <leader><leader> <cmd>Telescope find_files theme=ivy<CR>

  nnoremap <leader>fp <cmd>Telescope find_files theme=ivy hidden=true cwd=~/.local/share/nvim/site/pack/packer<CR>

  nnoremap <leader>ps <cmd>Telescope find_files theme=ivy hidden=true cwd=~/src/gitlab.snapp.ir<CR>

  nnoremap <C-q> <cmd>Telescope quickfix theme=ivy<CR>

  nnoremap ?? <cmd>Telescope live_grep theme=ivy<CR>

  nnoremap <leader>en <cmd>Telescope find_files theme=ivy<CR>

  nnoremap <leader><leader> <cmd>Telescope find_files theme=ivy<CR>
]]


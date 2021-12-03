local telescope_actions = require "telescope.actions"
-- Telescope
require("telescope").setup {
  defaults = {
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

local ivy = require("telescope.themes").get_ivy
local current_theme = ivy { layout_config = { height = 0.4 } }
local theme = function(opts)
  return vim.tbl_extend("force", current_theme, opts)
end

function telescope_wrap(fn, opts)
  opts = opts or {}
  if type(fn) == "function" then
    fn(theme(opts))
  elseif type(fn) == "string" then
    require("telescope.builtin")[fn](theme(opts))
  end
end
-- some mappings for searching using telescope
vim.api.nvim_set_keymap(
  "n",
  "<leader><leader>",
  '<cmd>lua telescope_wrap("find_files", { hidden = false })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fp",
  '<cmd>lua telescope_wrap("find_files", {hidden = true, cwd = "~/.local/share/nvim/site/pack/packer"})<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>ps",
  '<cmd>lua telescope_wrap("find_files", {hidden = true, cwd = "~/src/gitlab.snapp.ir/" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<C-q>",
  '<cmd>lua telescope_wrap("quickfix", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "??",
  '<cmd>lua telescope_wrap("live_grep", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>en",
  '<cmd>lua telescope_wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "gd",
  '<cmd>lua telescope_wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)

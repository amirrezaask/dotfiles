local M = {}
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
local current_theme = {}
-- local current_theme = ivy { layout_config = { height = 0.4 } }
local theme = function(opts)
  return vim.tbl_extend("force", current_theme, opts)
end

function M.wrap(fn, opts)
  opts = opts or {}
  if type(fn) == "function" then
    fn(theme(opts))
  elseif type(fn) == "string" then
    require("telescope.builtin")[fn](theme(opts))
  end
end

function M.wallpaper()
  local path = os.getenv "WALLPAPERS_PATH" or "~/src/github.com/amirrezaask/dotfiles/wallpapers/"
  require("telescope.builtin").find_files {
    cwd = path,
    prompt_title = "Set Wallpaper",
    previewer = false,
    attach_mappings = function(prompt_bufnr, map)
      local apply = function()
        local selected = require("telescope.actions.state").get_selected_entry()
        vim.fn.system(string.format("feh --bg-fill %s", selected.cwd .. selected.value))
      end
      map("i", "<CR>", apply)
      map("n", "<CR>", apply)
      return true
    end,
  }
end
vim.cmd [[ command! Wallpaper lua require"amirrezaask.telescope".wallpaper() ]]

-- some mappings for searching using telescope
vim.api.nvim_set_keymap(
  "n",
  "<leader><leader>",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", { hidden = false })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fp",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", {hidden = true, cwd = "~/.local/share/nvim/site/pack/packer"})<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>ps",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", {hidden = true, cwd = "~/src/gitlab.snapp.ir/" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<C-q>",
  '<cmd>lua require"amirrezaask.telescope".wrap("quickfix", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "??",
  '<cmd>lua require"amirrezaask.telescope".wrap("live_grep", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>en",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "gd",
  '<cmd>lua require"amirrezaask.telescope".wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)

function M.make_command_picker(pattern, opts)
  local pickers = require "telescope.pickers"
  local finders = require "telescope.finders"
  local make_entry = require "telescope.make_entry"
  local conf = require("telescope.config").values
  local action_state = require "telescope.actions.state"
  local actions = require "telescope.actions"
  opts = opts or require("telescope.themes").get_dropdown()
  return function()
    pickers.new(opts, {
      prompt_title = "Go Commands",
      finder = finders.new_table {
        results = (function()
          local commands_iter = vim.api.nvim_get_commands { builtin = false }
          local go_commands = {}
          for _, cmd in pairs(commands_iter) do
            if string.find(cmd.name, pattern) then
              table.insert(go_commands, cmd)
            end
          end
          return go_commands
        end)(),
        entry_maker = make_entry.gen_from_commands(),
      },
      sorter = conf.generic_sorter(opts),
      attach_mappings = function(prompt_bufnr)
        actions.select_default:replace(function()
          local selection = action_state.get_selected_entry()
          if selection == nil then
            print "[telescope] Nothing currently selected"
            return
          end

          actions.close(prompt_bufnr)
          local val = selection.value
          local cmd = string.format([[:%s ]], val.name)

          if val.nargs == "0" then
            vim.cmd(cmd)
          else
            vim.cmd [[stopinsert]]
            vim.fn.feedkeys(cmd)
          end
        end)

        return true
      end,
    }):find()
  end
end
return M

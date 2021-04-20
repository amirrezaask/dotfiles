local actions = require('telescope.actions')
local normal_maps = {}
local finders = require('telescope.finders')
local make_entry = require('telescope.make_entry')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values

require('telescope').setup({
  defaults = {
    prompt_prefix = '❯ ',
    selection_caret = '❯ ',
    layout_strategy = 'flex',
    previewer = false,
    prompt_position = 'top',
    sorting_strategy = 'descending',
    layout_defaults = {
      horizontal = {
        width_padding = 0.1,
        height_padding = 0.1,
        preview_width = 0.5,
      },
    },
    mappings = {
      i = {
        ['<C-c>'] = actions.close,
        ['<C-q>'] = actions.send_to_qflist,
      },
    },
  },
})

local M = {}

require('telescope').load_extension('fzy_native')
require('telescope').load_extension('dap')

function M.base16_theme_selector()
  local base16 = require('base16')
  local theme_names = {}
  for k, _ in pairs(base16.themes) do
    table.insert(theme_names, k)
  end
  pickers.new({}, {
    finder = finders.new_table({
      results = theme_names,
    }),
    sorter = conf.generic_sorter(),
    -- source = theme_names,
    -- handler = function(theme)
    --   for k, v in pairs(base16.themes) do
    --     if k == theme then
    --       base16(v)
    --     end
    --   end
    -- end,
  }):find()
end

function M.buffer_git_files()
  require('telescope.builtin').git_files({
    cwd = vim.fn.expand('%:p:h'),
  })
end

function M.installed_plugins()
  require('telescope.builtin').find_files({
    cwd = vim.fn.stdpath('data') .. '/site/pack/packer/start/',
  })
end

function M.edit_configs()
  require('telescope.builtin').find_files({
    prompt_title = '> Edit Configs <',
    cwd = '~/src/github.com/amirrezaask/dotfiles',
  })
end

function M.edit_neovim()
  require('telescope.builtin').find_files({
    prompt_title = '> Edit Neovim Config <',
    cwd = '~/src/github.com/amirrezaask/dotfiles/nvim',
  })
end

function M.edit_zsh()
  require('telescope.builtin').find_files({
    prompt_title = '> Edit ZSH Config <',
    cwd = '~/src/github.com/amirrezaask/dotfiles/zsh',
  })
end
function M.lsp_implementations(opts)
  opts = opts or {}

  local params = vim.lsp.util.make_position_params()
  local result = vim.lsp.buf_request_sync(0, 'textDocument/implementation', params, opts.timeout or 10000)
  local flattened_results = {}
  for _, server_results in pairs(result) do
    if server_results.result then
      vim.list_extend(flattened_results, server_results.result)
    end
  end

  if #flattened_results == 0 then
    return
  elseif #flattened_results == 1 then
    vim.lsp.util.jump_to_location(flattened_results[1])
  else
    local locations = vim.lsp.util.locations_to_items(flattened_results)
    pickers.new(opts, {
      prompt_title = 'LSP Implementations',
      finder = finders.new_table({
        results = locations,
        entry_maker = opts.entry_maker or make_entry.gen_from_quickfix(opts),
      }),
      previewer = conf.qflist_previewer(opts),
      sorter = conf.generic_sorter(opts),
    }):find()
  end
end

normal_maps['<Space><Space>'] = '<cmd>lua require("telescope.builtin").find_files{}<CR>'
normal_maps['<Space>fb'] = '<cmd>lua require("telescope.builtin").file_browser{}<CR>'
normal_maps['<Space>fp'] = '<cmd>lua require("plugin.telescope").installed_plugins{}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("telescope.builtin").git_files{}<CR>'
normal_maps['<C-p>'] = '<cmd>lua require("telescope.builtin").git_files{}<CR>'
normal_maps['??'] = '<cmd>lua require("telescope.builtin").live_grep{}<CR>'
normal_maps['<Space>b'] = '<cmd>lua require("telescope.builtin").buffers{}<CR>'
normal_maps['<Space>ec'] = '<cmd>lua require("plugin.telescope").edit_configs()<CR>'
normal_maps['<Space>en'] = '<cmd>lua require("plugin.telescope").edit_neovim()<CR>'
normal_maps['<Space>ez'] = '<cmd>lua require("plugin.telescope").edit_zsh()<CR>'
normal_maps['<Space>c'] = '<cmd>lua require("telescope.builtin").commands{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("telescope.builtin").history{}<CR>'
normal_maps['<Space>fr'] = '<cmd>lua require("telescope.builtin").oldfiles{}<CR>'
normal_maps['<Space>h'] = '<cmd>lua require("telescope.builtin").help_tags{}<CR>'
normal_maps['<Space>gc'] = '<cmd>lua require("telescope.builtin").git_commits{}<CR>'
normal_maps['<Space>gb'] = '<cmd>lua require("telescope.builtin").git_bcommits{}<CR>'
normal_maps['<Space>go'] = '<cmd>lua require("telescope.builtin").git_checkout{}<CR>'
normal_maps['<Space>gf'] = '<cmd>lua require("plugin.telescope").buffer_git_files{}<CR>'
normal_maps['<Space>gs'] = '<cmd>lua require("telescope.builtin").git_status{}<CR>'
normal_maps['<Space>tf'] = '<cmd>lua require("telescope.builtin").treesitter{}<CR>'
require('amirrezaask.nvim').mode_map({
  n = normal_maps,
})

return M

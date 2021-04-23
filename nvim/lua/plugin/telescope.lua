local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values
local repos = require('amirrezaask.repos')
local telescope = require('telescope')

telescope.setup({
  defaults = {
    prompt_prefix = ' ',
    selection_caret = ' ',
    layout_strategy = 'flex',
    prompt_position = 'bottom',
    sorting_strategy = 'descending',
    borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
    layout_defaults = {
      horizontal = {
        width_padding = 0.1,
        height_padding = 0.1,
        preview_width = 0.6,
      },
      vertical = {
        width_padding = 0.1,
        height_padding = 0.1,
        preview_height = 0.3,
      },
    },
    file_previewer = require('telescope.previewers').vim_buffer_cat.new,
    grep_previewer = require('telescope.previewers').vim_buffer_vimgrep.new,
    qflist_previewer = require('telescope.previewers').vim_buffer_qflist.new,
    mappings = {
      i = {
        ['<C-c>'] = actions.close,
        ['<ESC>'] = actions.close,
        ['<C-q>'] = actions.send_to_qflist,
        ['<C-j>'] = actions.move_selection_next,
        ['<C-k>'] = actions.move_selection_previous,
      },
    },
  },
})

local M = {}

telescope.load_extension('fzy_native')
telescope.load_extension('dap')
telescope.load_extension('media_files')
telescope.load_extension('git_worktree')
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
    attach_mappings = function(_)
      actions.select_default:replace(function()
        local theme = action_state.get_selected_entry()[1]
        -- actions.close(prompt_bufnr)
        for k, v in pairs(base16.themes) do
          if k == theme then
            base16(v)
          end
        end
      end)
      return true
    end,
  }):find()
end

function M.buffer_git_files()
  require('telescope.builtin').git_files({
    cwd = vim.fn.expand('%:p:h'),
  })
end

function M.projects()
  pickers.new({}, {
    finder = finders.new_table({
      results = repos.list_projects({ '~/src/github.com/amirrezaask' }),
    }),
    sorter = conf.generic_sorter(),
    attach_mappings = function(_)
      actions.select_default:replace(function()
        local dir = action_state.get_selected_entry()[1]
        vim.cmd([[ cd ]] .. dir)
      end)
      return true
    end,
  }):find()
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
function M.lsp_workspace_symbols()
  local q = vim.fn.input('Symbol: ')
  require('telescope.builtin').lsp_workspace_symbols({
    layout_strategy = 'vertical',
    query = q,
  })
end

M.vertical_opts = {
  layout_strategy = 'vertical',
}

require('amirrezaask.nvim').mode_map({
  n = {
    ['<Space><Space>'] = '<cmd>lua require("telescope.builtin").find_files{}<CR>',
    ['<Space>fb'] = '<cmd>lua require("telescope.builtin").file_browser{}<CR>',
    ['<Space>fp'] = '<cmd>lua require("plugin.telescope").installed_plugins{}<CR>',
    ['<Space>pf'] = '<cmd>lua require("plugin.telescope").projects{}<CR>',
    ['<C-p>'] = '<cmd>lua require("telescope.builtin").git_files{}<CR>',
    ['??'] = '<cmd>lua require("telescope.builtin").live_grep{layout_strategy="vertical"}<CR>',
    ['<Space>b'] = '<cmd>lua require("telescope.builtin").buffers{}<CR>',
    ['<Space>ec'] = '<cmd>lua require("plugin.telescope").edit_configs()<CR>',
    ['<Space>tc'] = '<cmd>lua require("plugin.telescope").base16_theme_selector()<CR>',
    ['<Space>en'] = '<cmd>lua require("plugin.telescope").edit_neovim()<CR>',
    ['<Space>ez'] = '<cmd>lua require("plugin.telescope").edit_zsh()<CR>',
    ['<Space>c'] = '<cmd>lua require("telescope.builtin").commands{}<CR>',
    ['<Space>fr'] = '<cmd>lua require("telescope.builtin").oldfiles{}<CR>',
    ['<Space>h'] = '<cmd>lua require("telescope.builtin").help_tags{}<CR>',
    -- Git
    ['<Space>gc'] = '<cmd>lua require("telescope.builtin").git_commits{}<CR>',
    ['<Space>gb'] = '<cmd>lua require("telescope.builtin").git_bcommits{}<CR>',
    ['<Space>go'] = '<cmd>lua require("telescope.builtin").git_checkout{}<CR>',
    ['<Space>gf'] = '<cmd>lua require("plugin.telescope").buffer_git_files{}<CR>',
    ['<Space>gs'] = '<cmd>lua require("telescope.builtin").git_status{}<CR>',
    ['<Space>gwc'] = '<cmd>lua require("telescope").extensions.git_worktree.create_git_worktree()<CR>',
    ['<Space>gwl'] = '<cmd>lua require("telescope").extensions.git_worktree.git_worktrees()<CR>',
    ['<Space>tf'] = '<cmd>lua require("telescope.builtin").treesitter{}<CR>',
    ['<C-q>'] = '<cmd>lua require("telescope.builtin").quickfix{}<CR>',
    -- LSP
    ['gd'] = '<cmd>lua require"telescope.builtin".lsp_defenitions(require"plugin.telescope".vertical_opts)<CR>',
    ['K'] = '<cmd>lua vim.lsp.buf.hover()<CR>',
    ['gI'] = '<cmd>lua require"telescope.builtin".lsp_implementations(require"plugin.telescope".vertical_opts)<CR>',
    ['gR'] = '<cmd>lua require"telescope.builtin".lsp_references(require"plugin.telescope".vertical_opts)<CR>',
    ['<Space>lr'] = '<cmd>lua require"telescope.builtin".lsp_references(require"plugin.telescope".vertical_opts)<CR>',
    ['<Space>li'] = '<cmd>lua require"telescope.builtin".lsp_implementations(require"plugin.telescope".vertical_opts)<CR>',
    ['<Space>ld'] = '<cmd>lua require"telescope.builtin".lsp_document_symbols(require"plugin.telescope".vertical_opts)<CR>',
    ['<Space>lw'] = '<cmd>lua require"plugin.telescope".lsp_workspace_symbols()<CR>',
    ['<Space>lc'] = '<cmd>lua require"telescope.builtin".lsp_code_actions()<CR>',
    ['<Space>d?'] = '<cmd>lua require"telescope.builtin".lsp_document_diagnostics()<CR>',
    ['<Space>w?'] = '<cmd>lua require"telescope.builtin".lsp_workspace_diagnostics()<CR>',
  },
})

return M

local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values
local repos = require('amirrezaask.repos')
local telescope = require('telescope')

vim.cmd([[ 
  hi TelescopeSelection guifg=#f04c75
  hi TelescopePreviewLine guifg=#01ff3c
  hi TelescopePreviewMatch guifg=#f04c75
  hi TelescopeMatching guifg=#ffff1e
]])

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
    layout_strategy = 'vertical',
    prompt_title = '> Edit Neovim Config <',
    cwd = '~/src/github.com/amirrezaask/dotfiles/nvim',
    previewer = false,
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
    ['<Space><Space>'] = require('telescope.builtin').find_files,
    ['<Space>fb'] = require('telescope.builtin').file_browser,
    ['<Space>fp'] = M.installed_plugins,
    ['<Space>pf'] = M.projects,
    ['<C-p>'] = require('telescope.builtin').git_files,
    ['??'] = function()
      require('telescope.builtin').live_grep({ layout_strategy = 'vertical' })
    end,
    ['<Space>b'] = require('telescope.builtin').buffers,
    ['<Space>ec'] = M.edit_configs,
    ['<Space>tc'] = M.base16_theme_selector,
    ['<Space>en'] = M.edit_neovim,
    ['<Space>ez'] = M.edit_zsh,
    ['<Space>c'] = require('telescope.builtin').commands,
    ['<Space>fr'] = require('telescope.builtin').oldfiles,
    ['<Space>h'] = require('telescope.builtin').help_tags,
    -- Git
    ['<Space>gc'] = require('telescope.builtin').git_commits,
    ['<Space>gb'] = require('telescope.builtin').git_bcommits,
    ['<Space>go'] = require('telescope.builtin').git_checkout,
    ['<Space>gf'] = M.buffer_git_files,
    ['<Space>gs'] = require('telescope.builtin').git_status,
    ['<Space>gwc'] = require('telescope').extensions.git_worktree.create_git_worktree,
    ['<Space>gwl'] = require('telescope').extensions.git_worktree.git_worktrees,
    ['<Space>tf'] = require('telescope.builtin').treesitter,
    ['<C-q>'] = require('telescope.builtin').quickfix,
  },
})

return M

local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values
local repos = require('amirrezaask.repos')
local telescope = require('telescope')
local themes = require('telescope.themes')

telescope.setup {
  defaults = {
    prompt_prefix = '> ',
    selection_caret = ' ',
    layout_strategy = 'flex',
    prompt_position = 'top',
    sorting_strategy = 'ascending',
    borderchars = { '─', '│', '─', '│', '┌', '┐', '┘', '└' },
    layout_defaults = {
      horizontal = {
        width_padding = 0.1,
        height_padding = 0.1,
        preview_width = 0.5,
      },
      vertical = {
        width_padding = 0.1,
        height_padding = 0.1,
        preview_height = 0.3,
      },
    },
    file_ignore_patterns = { 'node_modules/.*', '.git/.*', '_site/.*' },
    file_previewer = require('telescope.previewers').vim_buffer_cat.new,
    grep_previewer = require('telescope.previewers').vim_buffer_vimgrep.new,
    qflist_previewer = require('telescope.previewers').vim_buffer_qflist.new,
    mappings = {
      n = {
        ['<ESC>'] = actions.close,
        ['<C-c>'] = actions.close,
        ['jk'] = actions.close,
        ['jj'] = actions.close,
        ['kk'] = actions.close,
        ['kj'] = actions.close,
      },
      i = {
        ['<C-c>'] = actions.close,
        -- ['<ESC>'] = actions.close,
        ['<C-q>'] = actions.send_to_qflist,
        ['<C-j>'] = actions.preview_scrolling_up,
        ['<C-k>'] = actions.preview_scrolling_down,
      },
    },
  },
}
require('amirrezaask.nvim').highlight('TelescopeMatching', '#f2904b')
local M = {}

telescope.load_extension('fzy_native')
telescope.load_extension('dap')
telescope.load_extension('media_files')
telescope.load_extension('git_worktree')
telescope.load_extension('gh')
telescope.load_extension('snippets')
-- telescope.load_extension('fzf')

function M.base16_theme_selector()
  local theme_names = require('base16.themes'):names() 
  pickers.new({}, {
    finder = finders.new_table({
      results = theme_names,
    }),
    sorter = conf.generic_sorter(),
    attach_mappings = function(_)
      actions.select_default:replace(function()
        local theme = action_state.get_selected_entry()[1]
        for k, v in pairs(require('base16.themes')) do
          if k == theme then
            v:apply()
          end
        end
      end)
      return true
    end,
  }):find()
end

function M.find_src()
  require('telescope.builtin').find_files {
    cwd = '~/src'
  }
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

function M.git_files()
  require('telescope.builtin').git_files(themes.get_ivy()) 
end

M.vertical_opts = {
  layout_strategy = 'flex',
}

function M.on_attach(_)
  require('amirrezaask.nvim').map {
      ['n gd'] = function()
        require('telescope.builtin').lsp_definitions(require('plugin.telescope').vertical_opts)
      end,
      ['n K'] = vim.lsp.buf.hover,
      ['n gI'] = function()
        require('telescope.builtin').lsp_implementations(require('plugin.telescope').vertical_opts)
      end,
      ['n gR'] = function()
        require('telescope.builtin').lsp_references(require('plugin.telescope').vertical_opts)
      end,
      ['n <leader>lR'] = function()
        vim.lsp.buf.rename()
      end,
      ['n <leader>lr'] = function()
        require('telescope.builtin').lsp_references(require('plugin.telescope').vertical_opts)
      end,
      ['n <leader>li'] = function()
        require('telescope.builtin').lsp_implementations(require('plugin.telescope').vertical_opts)
      end,
      ['n <leader>ld'] = function()
        require('telescope.builtin').lsp_document_symbols(require('plugin.telescope').vertical_opts)
      end,
      ['n <leader>lw'] = function()
        require('plugin.telescope').lsp_workspace_symbols()
      end,
      ['n <leader>lc'] = function()
        require('telescope.builtin').lsp_code_actions()
      end,
      ['n <leader>d?'] = function()
        require('telescope.builtin').lsp_document_diagnostics()
      end,
      ['n <leader>w?'] = function()
        require('telescope.builtin').lsp_workspace_diagnostics()
      end,
  }
end


require('amirrezaask.nvim').map {
    ['n <leader><leader>'] = require('telescope.builtin').find_files,
    ['n <leader>fb'] = require('telescope.builtin').file_browser,
    ['n <leader>fp'] = M.installed_plugins,
    ['n <leader>pf'] = M.projects,
    ['n <C-p>'] = M.git_files,
    ['n <M-q>'] = require('telescope.builtin').quickfix,
    ['n ??'] = require('telescope.builtin').live_grep,
    ['n <leader>b'] = require('telescope.builtin').buffers,
    ['n <leader>ec'] = M.edit_configs,
    ['n <leader>tc'] = M.base16_theme_selector,
    ['n <leader>en'] = M.edit_neovim,
    ['n <leader>ez'] = M.edit_zsh,
    ['n <leader>fs'] = M.find_src,
    ['n <leader>c'] = require('telescope.builtin').commands,
    ['n <leader>fr'] = require('telescope.builtin').oldfiles,
    ['n <leader>h'] = require('telescope.builtin').help_tags,
    -- Git
    ['n <leader>gc'] = require('telescope.builtin').git_commits,
    ['n <leader>gb'] = require('telescope.builtin').git_bcommits,
    ['n <leader>go'] = require('telescope.builtin').git_checkout,
    ['n <leader>gf'] = M.buffer_git_files,
    ['n <leader>gs'] = require('telescope.builtin').git_status,
    ['n <leader>gwc'] = require('telescope').extensions.git_worktree.create_git_worktree,
    ['n <leader>gwl'] = require('telescope').extensions.git_worktree.git_worktrees,
    ['n <leader>tf'] = require('telescope.builtin').treesitter,
}

return M

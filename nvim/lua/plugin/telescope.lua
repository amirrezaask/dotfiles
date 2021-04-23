local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values
local repos = require('amirrezaask.repos')
local telescope = require('telescope')
local builtin = require('telescope.builtin')

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

local nvim = require('amirrezaask.nvim')

nvim.command('LSPDefinitions', builtin.lsp_definitions)
nvim.command('LSPHover', vim.lsp.buf.hover)
nvim.command('LSPSignatureHelp', vim.lsp.buf.signature_help)
nvim.command('LSPTypeDefinition', vim.lsp.buf.type_definition)
nvim.command('LSPRename', require('lspsaga.rename').rename)
nvim.command('LSPReferences', function()
  builtin.lsp_references({ layout_strategy = 'vertical' })
end)
nvim.command('LSPDocumentSymbols', function()
  builtin.lsp_document_symbols({ layout_strategy = 'vertical' })
end)
nvim.command('LSPWorkspaceSymbols', function()
  builtin.lsp_workspace_symbols({ layout_strategy = 'vertical' })
end)
nvim.command('LSPCodeActions', telescope.lsp_code_actions)
nvim.command('LSPImplementations', function()
  builtin.lsp_implementations({ layout_strategy = 'vertical' })
end)
nvim.command('LSPDocumentDiagnostics', function()
  builtin.lsp_document_diagnostics({ layout_strategy = 'vertical' })
end)
nvim.command('LSPWorkspaceDiagnostics', function()
  builtin.lsp_workspace_diagnostics({ layout_strategy = 'vertical' })
end)
nvim.mode_map({
  n = {
    ['gd'] = '<cmd>LSPDefinitions<CR>',
    ['K'] = '<cmd>LSPHover<CR>',
    ['gI'] = '<cmd>LSPImplementations<CR>',
    ['1gD'] = '<cmd>LSPTypeDefinition<CR>',
    ['gR'] = '<cmd>LSPReferences<CR>',
    ['<Space>lr'] = '<cmd>LSPReferences<CR>',
    ['<Space>li'] = '<cmd>LSPImplementations<CR>',
    ['<Space>ld'] = '<cmd>LSPDocumentSymbols<CR>',
    ['<Space>lw'] = '<cmd>LSPWorkspaceSymbols<CR>',
    ['<Space>lc'] = '<cmd>LSPCodeActions<CR>',
    ['<Space>d?'] = '<cmd>LSPDocumentDiagnostics<CR>',
    ['<Space>w?'] = '<cmd>LSPWorkspaceDiagnostics<CR>',
  },
})
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
    ['<Space>gc'] = '<cmd>lua require("telescope.builtin").git_commits{}<CR>',
    ['<Space>gb'] = '<cmd>lua require("telescope.builtin").git_bcommits{}<CR>',
    ['<Space>go'] = '<cmd>lua require("telescope.builtin").git_checkout{}<CR>',
    ['<Space>gf'] = '<cmd>lua require("plugin.telescope").buffer_git_files{}<CR>',
    ['<Space>gs'] = '<cmd>lua require("telescope.builtin").git_status{}<CR>',
    ['<Space>tf'] = '<cmd>lua require("telescope.builtin").treesitter{}<CR>',
    ['<C-q>'] = '<cmd>lua require("telescope.builtin").quickfix{}<CR>',
  },
})

return M

local has_telescope, _ = pcall(require, 'telescope')
if not has_telescope then return end

local actions = require('telescope.actions')
local action_state = require('telescope.actions.state')
local finders = require('telescope.finders')
local pickers = require('telescope.pickers')
local conf = require('telescope.config').values
local repos = require('repos')
local telescope = require('telescope')
local wallpaper = require('plugin.wallpaper')
local ivy = require('telescope.themes').get_ivy
local notheme = function(opts)
  return opts
end
local current_theme = notheme

local function wrap(fn, opts)
  return function() fn(current_theme(opts)) end
end

telescope.setup {
  defaults = {
    prompt_prefix = '> ',
    selection_caret = '> ',
    layout_strategy = 'flex',
    prompt_position = 'bottom',
    sorting_strategy = 'descending',
    borderchars = { '─', '│', '─', '│', '╭', '╮', '╯', '╰' },
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
        ['kj'] = actions.close,
      },
      i = {
        ['<C-c>'] = actions.close,
        -- ['<ESC>'] = actions.close,
        ['<C-q>'] = actions.send_to_qflist,
        ['<C-j>'] = actions.move_selection_next,
        ['<C-k>'] = actions.move_selection_previous,
      },
    },
  },
}

local M = {}

-- telescope.load_extension('fzy_native')
telescope.load_extension('dap')
telescope.load_extension('media_files')
telescope.load_extension('git_worktree')
telescope.load_extension('gh')
telescope.load_extension('snippets')
telescope.load_extension('fzf')
--TODO: write telescope function for nvim doc things

function M.docs()
  local docs = {}
  for group, group_keys in pairs(__MAPS_DOCS) do
    for k, doc in pairs(group_keys) do
      table.insert(docs, string.format('%s [%s] -> %s', group, k, doc))
    end
  end
  for cmd, doc in pairs(__CMDS_DOCS) do
    table.insert(docs, string.format('%s -> %s', cmd, doc))
  end
  pickers.new(current_theme(), {
    prompt_title = "Docs :)",
    finder = finders.new_table {
      results = docs,
    },
    sorter = conf.generic_sorter(),
    previewer = false,

  }):find()
end

function M.grep_string()
  require('floating'):prompt('> Grep String < ', function(word)
    require('telescope.builtin').grep_string {
        shorten_path = true,
        search = word,
    }
  end)
end

function M.base16_theme_selector()
  local theme_names = require('base16.themes'):names() 
  pickers.new(current_theme(), {
    finder = finders.new_table({
      results = theme_names,
    }),
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local apply = function()
        local theme = action_state.get_selected_entry(prompt_bufnr)[1]
        for k, v in pairs(require('base16.themes')) do
          if k == theme then
            v:apply()
          end
        end
      end
      map('i', '<CR>', apply)
      map('n', '<CR>', apply)
      return true
    end,
  }):find()
end

function M.find_src()
  require('telescope.builtin').find_files(current_theme {
    cwd = '~/src'
  })
end

function M.buffer_git_files()
  require('telescope.builtin').git_files(current_theme {
    cwd = vim.fn.expand('%:p:h'),
  })
end

function M.projects()
  pickers.new(current_theme(), {
    previewer = false,
    finder = finders.new_table({
      results = repos.list_projects({ '~/src/' }),
    }),
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local cd_into = function()
        local dir = action_state.get_selected_entry(prompt_bufnr)[1]
        vim.c.cd(dir)
        actions.close(prompt_bufnr)
      end
      map('i', '<CR>', cd_into)
      map('n', '<CR>', cd_into)
      return true
    end,
  }):find()
end

function M.installed_plugins()
  require('telescope.builtin').find_files(current_theme {
    cwd = vim.fn.stdpath('data') .. '/site/pack/packer/start/',
  })
end

function M.edit_configs()
  require('telescope.builtin').find_files(current_theme {
    prompt_title = '> Edit Configs <',
    cwd = '~/src/github.com/amirrezaask/dotfiles',
  })
end

function M.edit_neovim()
  require('telescope.builtin').find_files(current_theme {
    layout_strategy = 'vertical',
    prompt_title = '> Edit Neovim Config <',
    cwd = '~/.config/nvim',
    previewer = false,
  })
end

function M.edit_awesome()
  require('telescope.builtin').find_files(current_theme {
    layout_strategy = 'vertical',
    prompt_title = '> Edit Neovim Config <',
    cwd = '~/.config/awesome',
    previewer = false,
  })
end


function M.edit_zsh()
  require('telescope.builtin').find_files(current_theme {
    prompt_title = '> Edit ZSH Config <',
    cwd = '~/src/github.com/amirrezaask/dotfiles/zsh',
  })
end

function M.lsp_workspace_symbols()
  require('floating'):prompt('> LSP Workspace: ', function(word)
    require('telescope.builtin').lsp_workspace_symbols({
      query = word,
    })
  end)
end

function M.git_files()
  require('telescope.builtin').git_files(current_theme()) 
end

function M.buffer_grep()
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  pickers.new(current_theme(), {
    prompt_title = "> Current File Grep <",
    -- TODO: add previewer
    previewer = false,
    finder = finders.new_table({
      results = lines,
    }),
    sorter = conf.generic_sorter(),
    attach_mappings = function(prompt_bufnr, map)
      local jump_to = function()
        local line = action_state.get_selected_entry(prompt_bufnr)[1]
        local prompt = vim.api.nvim_buf_get_lines(prompt_bufnr, 0, -1, false)[1]
        local current_picker = action_state.get_current_picker(prompt_bufnr)
        local col = line:find(prompt:sub(#current_picker.prompt_prefix+1, -1)) or 0
        actions.close(prompt_bufnr)
        for i, l in ipairs(lines) do
          if l == line then
            vim.api.nvim_win_set_cursor(0, {i, col})
          end
        end
      end
      map('i', '<CR>', jump_to)
      map('n', '<CR>', jump_to)
      return true
    end,
  }):find()

end

function M.quickfix()
  if _G.quickfix_state == 'open' then
    vim.c.cclose()
  else
    require('telescope.builtin').quickfix()
  end
end

function M.on_attach(_)
  vim.nmap {
    ['gd'] = { wrap(require('telescope.builtin').lsp_definitions), "Goto defenition", "IDE" },
    ['gI'] = { wrap(require('telescope.builtin').lsp_implementations), "Goto implementations", "IDE" },
    ['gR'] = { wrap(require('telescope.builtin').lsp_references), "Goto references", "IDE" },
    ['<leader>lr'] = { wrap(require('telescope.builtin').lsp_references), "Goto references", "IDE" },
    ['<leader>li'] = { wrap(require('telescope.builtin').lsp_implementations), "Goto implementations", "IDE" },
    ['<leader>ld'] = { wrap(require('telescope.builtin').lsp_document_symbols), "Search through document symbols", "IDE" },
    ['<leader>lw'] = { wrap(require('plugin.telescope').lsp_workspace_symbols), "Search through workspace symbols", "IDE" },
    ['<leader>lc'] = { wrap(require('telescope.builtin').lsp_code_actions), "Show code actions", "IDE" },
    ['<leader>d?'] = { wrap(require('telescope.builtin').lsp_document_diagnostics), "Search through document diagnostic", "IDE" },
    ['<leader>w?'] = { wrap(require('telescope.builtin').lsp_workspace_diagnostics), "Search through workspace diagnostics", "IDE" }
  }
end

vim.nmap {
    ['<leader><leader>'] = wrap(require('telescope.builtin').find_files, {hidden=true}),
    ['<leader>fb'] = wrap(require('telescope.builtin').file_browser),
    ['<leader>fp'] = M.installed_plugins,
    ['<leader>pp'] = M.projects,
    ['<C-p>'] = M.git_files,
    ['<C-q>'] = M.quickfix,
    ['<M-q>'] = require('telescope.builtin').quickfix,
    ['\\\\'] = wrap(M.buffer_grep),
    ['??'] = wrap(require('telescope.builtin').live_grep),
    [',f'] = wrap(M.grep_string),
    [',s'] = wrap(require('telescope.builtin').grep_string),
    ['<leader>b'] = wrap(require('telescope.builtin').buffers),
    ['<leader>ec'] = M.edit_configs,
    ['<leader>tc'] = M.base16_theme_selector,
    ['<leader>en'] = M.edit_neovim,
    ['<leader>ez'] = M.edit_zsh,
    ['<leader>ea'] = M.edit_awesome,
    ['<leader>fs'] = M.find_src,
    [',w'] = wallpaper.set_wallpaper,
    ['<leader>c'] = wrap(require('telescope.builtin').commands),
    ['<leader>fr'] = wrap(require('telescope.builtin').oldfiles),
    ['<leader>h'] = wrap(require('telescope.builtin').help_tags),
    -- Git
    ['<leader>gc'] = wrap(require('telescope.builtin').git_commits),
    ['<leader>gb'] = wrap(require('telescope.builtin').git_bcommits),
    ['<leader>go'] = wrap(require('telescope.builtin').git_checkout),
    ['<leader>gf'] = M.buffer_git_files,
    ['<leader>gs'] = wrap(require('telescope.builtin').git_status),
    ['<leader>gwc'] = require('telescope').extensions.git_worktree.create_git_worktree,
    ['<leader>gwl'] = require('telescope').extensions.git_worktree.git_worktrees,
    ['<leader>tf'] = require('telescope.builtin').treesitter,
}
return setmetatable(M, {
  __index = function(tbl, k)
    R('plugin.telescope')
    if tbl[k] then
      return tbl[k]
    else
      return require('telescope.builtin')[k]
    end
  end
})

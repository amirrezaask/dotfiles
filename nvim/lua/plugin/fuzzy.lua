local loc = require('fuzzy.lib.location')
local repos = require('amirrezaask.repos')
local nvim = require('amirrezaask.nvim')

local function base16_theme_selector()
  local base16 = require('base16')
  local theme_names = {}
  for k, _ in pairs(base16.themes) do
    table.insert(theme_names, k)
  end
  require('fuzzy').new({
    source = theme_names,
    mappings = {
      ['<CR>'] = function()
        local theme = require('fuzzy').CurrentFuzzy():get_output()
        for k, v in pairs(base16.themes) do
          if k == theme then
            base16(v)
          end
        end
      end,
    },
  })
end

local function find_src()
  require('fuzzy').new {
    source = repos.list_projects({ '~/src/github.com' }),
    mappings = {
      ['<CR>'] = function ()
        local dir =  require('fuzzy').CurrentFuzzy():get_output()
        require('fuzzy').CurrentFuzzy():close()
        vim.cmd(string.format([[ cd %s]], dir))
      end
    }
  }
end

local fuzzy = {}

local remove_icon = require('fuzzy.lib.helpers').remove_icon

local parser = function(line)
      local parts = vim.split(line, ':')
      return remove_icon(parts[1]), parts[2]
    end
local function preview()
  local preview_window = function (line)
    local floating_buffer = require('fuzzy.lib.floating').floating_buffer
    local buf, win, _ = floating_buffer {
      height = 50,
      width = 50,
      location = loc.center,
      border = 'no',
    }
    local file, lnum = parser(line)
    require('fuzzy.lib.helpers').open_file_at(file, lnum)
    vim.cmd([[ call feedkeys("\<C-c>") ]])
    vim.api.nvim_buf_set_option(0, 'modifiable', false)
  end
  local line = require('fuzzy').CurrentFuzzy():get_output()
  preview_window(line)
end

-- Fuzzy.nvim
require('fuzzy').setup({
  width = 40,
  height = 100,
  blacklist = {
    'vendor',
    '.git',
    'target',
  },
  location = loc.bottom_center,
  highlight_matches = 'no',
  mappings = {
    i = {
      ['P'] = preview
    },
    n = {
      ['p'] = preview,
      v = function()
        local line = require('fuzzy').CurrentFuzzy():get_output()
        vim.cmd([[vnew]])
        local filename, lnum = parser(line)
        require('fuzzy.lib.helpers').open_file_at(filename, lnum)
      end,
      s = function()
        local line = require('fuzzy').CurrentFuzzy():get_output()
        vim.cmd([[new]])
        local filename, lnum = parser(line)
        require('fuzzy.lib.helpers').open_file_at(filename, lnum)
      end
    },
  },
  sorter = require('fuzzy.lib.sorter').fzf_native,
  prompt = '> ',
  register = {
    base16_theme_selector = base16_theme_selector,
    find_src = find_src,
  },
  border = 'no',
})

vim.cmd [[ hi default link FuzzyNormal CursorLine ]]

function fuzzy.on_attach()
  local fuzzy_lsp = require('fuzzy.lsp')
  nvim.mode_map({
    n = {
      ['gd'] = fuzzy_lsp.definitions,
      ['K'] = vim.lsp.buf.hover,
      ['gI'] = fuzzy_lsp.implementation,
      ['gR'] = fuzzy_lsp.references,
      ['g0'] = fuzzy_lsp.document_symbols,
      ['gW'] = fuzzy_lsp.workspace_symbols,
      ['<leader>A'] = fuzzy_lsp.code_actions,
      ['<leader>lR'] = vim.lsp.buf.rename,
      ['<leader>lr'] = fuzzy_lsp.references,
      ['<leader>li'] = fuzzy_lsp.implementation,
      ['<leader>ld'] = fuzzy_lsp.document_symbols,
      ['<leader>lw'] = fuzzy_lsp.workspace_symbols,
      ['<leader>lc'] = fuzzy_lsp.code_actions,
    },
  })
end 

nvim.mode_map({
  n = {
    ['<leader><leader>'] = require('fuzzy').find_files,
    ['<leader>fb'] = require('fuzzy').interactive_finder,
    ['<leader>ec'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles', prompt = 'Edit dotfiles> ' })
    end,
    ['<leader>en'] = function()
      require('fuzzy').find_files({ path = '~/.config/nvim', prompt = 'Edit Neovim> ' })
    end,
    ['<leader>ez'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles/zsh', prompt = 'Edit ZSH> ' })
    end,
    ['<leader>fp'] = function()
      require('fuzzy').find_repo({ locations = { '~/.local/share/nvim/site/pack/packer' }, prompt = 'Find plugins> ' })
    end,
    ['<leader>gf'] = function()
      require('fuzzy').git_files({})
    end,
    ['<C-p>'] = require('fuzzy').git_files,
    ['<leader>fr'] = require('fuzzy').recent_files,
    ['<leader>pf'] = function()
      require('fuzzy').find_repo({ locations = { '~/src' } })
    end,
    ['??'] = function()
      require('fuzzy').grep({ height = 90, width = 70 })
    end,
    ['<leader>b'] = require('fuzzy').buffers,
    ['<leader>gg'] = require('fuzzy').git_grep,
    ['<leader>c'] = require('fuzzy').commands,
    ['<leader>h'] = require('fuzzy').help,
    ['<leader>gc'] = require('fuzzy').git_commits,
    ['<leader>gb'] = require('fuzzy').git_bcommits,
    ['<leader>gco'] = require('fuzzy').git_checkout,
  },
})

return fuzzy

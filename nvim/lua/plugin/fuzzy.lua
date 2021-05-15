local repos = require('amirrezaask.repos')
local nvim = require('amirrezaask.nvim')
local fuzzy = {}

function fuzzy.base16_theme_selector()
  local theme_names = require('base16.themes'):names() 
  require('fuzzy') {
    source = theme_names,
    handler = function(theme)
        for k, v in pairs(require('base16.themes')) do
          if k == theme then
            v:apply()
          end
        end
      end
    }
end

function fuzzy.find_src()
  require('fuzzy') {
    source = repos.list_projects({ '~/src/github.com' }),
    handler = function(line)
      vim.cmd(string.format([[ cd %s]], line))
    end
    -- mappings = {
    --   ['<CR>'] = function ()
    --     local dir =  require('fuzzy').CurrentFuzzy():get_output()
    --     require('fuzzy').CurrentFuzzy():close()
    --     vim.cmd(string.format([[ cd %s]], dir))
    --   end
    -- }
  }
end


-- local remove_icon = require('fuzzy.lib.helpers').remove_icon

-- local parser = function(line)
--   local parts = vim.split(line, ':')
--   return remove_icon(parts[1]), parts[2]
-- end

-- __AMIRREZAASK_FUZZY_PREVIEW_CLOSER = nil

-- local function preview()
--   local preview_window = function (line)
--     local floating_buffer = require('fuzzy.lib.floating').floating_buffer
--     local buf
--     buf, _, __AMIRREZAASK_FUZZY_PREVIEW_CLOSER = floating_buffer {
--       height = 50,
--       width = 50,
--       location = loc.center,
--       border = 'no',
--     }
--     local file, lnum = parser(line)
--     require('fuzzy.lib.helpers').open_file_at(file, lnum)
--     vim.cmd([[ call feedkeys("\<C-c>") ]])
--     vim.api.nvim_buf_set_option(buf, 'modifiable', false)
--     vim.api.nvim_buf_set_keymap(buf, 'n', 'q', '<cmd>lua __AMIRREZAASK_FUZZY_PREVIEW_CLOSER()<CR>', {noremap=true})
--   end
--   local line = require('fuzzy').CurrentFuzzy():get_output()
--   preview_window(line)
-- end

-- Fuzzy.nvim
require('fuzzy').setup({
  window = {
    width = 70,
    height = 100,
  },
  icons = 'no',
  blacklist = {
    'vendor',
    '.git',
    'target',
  },
  highlight_matches = 'no',
  -- mappings = {
  --   i = {
  --     ['P'] = preview
  --   },
  --   n = {
  --     ['p'] = preview,
  --     v = function()
  --       local line = require('fuzzy').CurrentFuzzy():get_output()
  --       vim.cmd([[vnew]])
  --       local filename, lnum = parser(line)
  --       require('fuzzy.lib.helpers').open_file_at(filename, lnum)
  --     end,
  --     s = function()
  --       local line = require('fuzzy').CurrentFuzzy():get_output()
  --       vim.cmd([[new]])
  --       local filename, lnum = parser(line)
  --       require('fuzzy.lib.helpers').open_file_at(filename, lnum)
  --     end
  --   },
  -- },
  sorter = require('fuzzy.lib.sorter').fzf_native,
  prompt = '> ',
  register = {
    base16_theme_selector = fuzzy.base16_theme_selector,
    find_src = fuzzy.find_src,
  },
  border = 'no',
})

-- vim.cmd [[ hi default link FuzzyNormal CursorLine ]]

function fuzzy.on_attach()
  local fuzzy_lsp = require('fuzzy.lsp')
  nvim.map {
    n = {
      ['n gd'] = fuzzy_lsp.definitions,
      ['n K'] = vim.lsp.buf.hover,
      ['n gI'] = fuzzy_lsp.implementation,
      ['n gR'] = fuzzy_lsp.references,
      ['n g0'] = fuzzy_lsp.document_symbols,
      ['n gW'] = fuzzy_lsp.workspace_symbols,
      ['n <leader>A'] = fuzzy_lsp.code_actions,
      ['n <leader>lR'] = vim.lsp.buf.rename,
      ['n <leader>lr'] = fuzzy_lsp.references,
      ['n <leader>li'] = fuzzy_lsp.implementation,
      ['n <leader>ld'] = fuzzy_lsp.document_symbols,
      ['n <leader>lw'] = fuzzy_lsp.workspace_symbols,
      ['n <leader>lc'] = fuzzy_lsp.code_actions,
    },
  }
end 

nvim.map {
    ['n <leader><leader>'] = require('fuzzy').find_files,
    -- ['<leader>fb'] = require('fuzzy').interactive_finder,
    ['n <leader>ec'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles', prompt = 'Edit dotfiles> ' })
    end,
    ['n <leader>en'] = function()
      require('fuzzy').find_files({ path = '~/.config/nvim', prompt = 'Edit Neovim> ' })
    end,
    ['n <leader>ez'] = function()
      require('fuzzy').find_files({ path = '~/src/github.com/amirrezaask/dotfiles/zsh', prompt = 'Edit ZSH> ' })
    end,
    ['n <leader>fp'] = function()
      require('fuzzy').find_repo({ locations = { '~/.local/share/nvim/site/pack/packer' }, prompt = 'Find plugins> ' })
    end,
    ['n <leader>gf'] = function()
      require('fuzzy').git_files({})
    end,
    ['n <C-p>'] = require('fuzzy').git_files,
    ['n <leader>fr'] = require('fuzzy').recent_files,
    ['n <leader>pf'] = function()
      require('fuzzy').find_repo({ locations = { '~/src' } })
    end,
    ['n ??'] = function()
      require('fuzzy').grep({ height = 90, width = 70 })
    end,
    ['n <leader>b'] = require('fuzzy').buffers,
    ['n <leader>gg'] = require('fuzzy').git_grep,
    ['n <leader>c'] = require('fuzzy').commands,
    ['n <leader>h'] = require('fuzzy').help,
    ['n <leader>gc'] = require('fuzzy').git_commits,
    ['n <leader>gb'] = require('fuzzy').git_bcommits,
    ['n <leader>gco'] = require('fuzzy').git_checkout,
}

return fuzzy

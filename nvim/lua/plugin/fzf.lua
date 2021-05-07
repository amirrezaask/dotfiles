local normal_maps = {}
local nvim = require('amirrezaask.nvim')

local repos = require('amirrezaask.repos')
local fzf = {}

vim.g.fzf_layout = { down = '40%' }
normal_maps['<leader><leader>'] = '<cmd>Files<CR>'
normal_maps['<leader>ec'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles<CR>'
normal_maps['<leader>en'] = '<cmd>Files /home/amirreza/.config/nvim<CR>'
normal_maps['<leader>ez'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles/zsh<CR>'
normal_maps['<leader>fp'] = '<cmd>Files /home/amirreza/.local/share/nvim/site/pack/packer/start<CR>'
normal_maps['<leader>gf'] = '<cmd>GFiles<CR>'
normal_maps['<C-p>'] = '<cmd>GFiles<CR>'
normal_maps['<C-q>'] = '<cmd>Quickfix<CR>'
normal_maps['<leader>fr'] = '<cmd>MRU<CR>'
normal_maps['<leader>pf'] = '<cmd>lua Projects({"/home/amirreza/src"})<CR>'
normal_maps['??'] = '<cmd>Rg<CR>'
normal_maps['<leader>b'] = '<cmd>Buffers<CR>'
normal_maps['<leader>c'] = '<cmd>Commands<CR>'
normal_maps['<leader>fh'] = '<cmd>History<CR>'
normal_maps['<leader>h'] = '<cmd>Helptags<CR>'
normal_maps['<leader>gc'] = '<cmd>Commits<CR>'
normal_maps['<leader>gb'] = '<cmd>BCommits<CR>'
normal_maps['<leader>gs'] = '<cmd>GitFiles?<CR>'

-- function M.buffer_git_files()
--   require('telescope.builtin').git_files({
--     cwd = vim.fn.expand('%:p:h'),
--   })
-- end

-- function M.find_files()
--   if vim.fn.isdirectory('.git') ~= 0 then
--     return M.git_files()
--   end
--   return require('telescope.builtin').find_files()
-- end

-- function M.projects()
--   pickers.new({}, {
--     finder = finders.new_table({
--       results = repos.list_projects({ '~/src/github.com/amirrezaask' }),
--     }),
--     sorter = conf.generic_sorter(),
--     attach_mappings = function(_)
--       actions.select_default:replace(function()
--         local dir = action_state.get_selected_entry()[1]
--         vim.cmd([[ cd ]] .. dir)
--       end)
--       return true
--     end,
--   }):find()
-- end
-- function M.installed_plugins()
--   require('telescope.builtin').find_files({
--     cwd = vim.fn.stdpath('data') .. '/site/pack/packer/start/',
--   })
-- end

-- function M.edit_configs()
--   require('telescope.builtin').find_files({
--     prompt_title = '> Edit Configs <',
--     cwd = '~/src/github.com/amirrezaask/dotfiles',
--   })
-- end

-- function M.edit_neovim()
--   require('telescope.builtin').find_files({
--     layout_strategy = 'vertical',
--     prompt_title = '> Edit Neovim Config <',
--     cwd = '~/src/github.com/amirrezaask/dotfiles/nvim',
--     previewer = false,
--   })
-- end

-- function M.edit_zsh()
--   require('telescope.builtin').find_files({
--     prompt_title = '> Edit ZSH Config <',
--     cwd = '~/src/github.com/amirrezaask/dotfiles/zsh',
--   })
-- end
-- function M.lsp_workspace_symbols()
--   local q = vim.fn.input('Symbol: ')
--   require('telescope.builtin').lsp_workspace_symbols({
--     layout_strategy = 'vertical',
--     query = q,
--   })
-- end

-- function M.git_files()
--   require('telescope.builtin').git_files(themes.get_dropdown()) 
-- end

function fzf.lsp_on_attach()
  local buf = vim.lsp.buf
  require('amirrezaask.nvim').mode_map {
    n = {
      ['gd'] = function()
        buf.definition()
      end,
      ['K'] = buf.hover,
      ['gI'] = Quickfix(buf.implementation),
      ['gR'] = Quickfix(buf.references),
      ['<leader>lR'] = buf.rename,
      ['<leader>lr'] = Quickfix(buf.references),
      ['<leader>li'] = Quickfix(buf.implementation),
      ['<leader>ld'] = Quickfix(buf.document_symbol),
      ['<leader>lw'] = Quickfix(buf.workspace_symbol),
      ['<leader>lc'] = buf.code_action,
      -- TODO(amirreza): fix these :)
      -- ['<leader>d?'] = ,
      -- ['<leader>w?'] = function()
      --   require('telescope.builtin').lsp_workspace_diagnostics()
      -- end,
    },

  }
end

function FZF(opts)
  opts = opts or {}
  opts.down = opts.down or '40%'
  vim.fn.call('fzf#run', { opts })
end

function Quickfix(callback)
  return function()
    vim.fn.setqflist({})
    if callback then callback() end
    local list
    vim.wait(2000, function()
      list = vim.fn.getqflist()
      return #list ~= 0
    end, 20, false)
    local source = {}
    for _, l in ipairs(list) do
      table.insert(source, string.format('%s:%s:%s:%s', vim.fn.bufname(l.bufnr), l.lnum, l.col, l.text))
    end
    FZF {
      source = source,
      sink = function(entry)
        entry = vim.split(entry, ':')
        -- local filename = vim.fn.bufname(tonumber(entry[1]))
        local filename = entry[1]
        vim.api.nvim_command(string.format('e +%s %s', entry[2], filename))
      end
    }
    vim.cmd [[ cclose ]]
  end
end
nvim.command('Quickfix', Quickfix)
nvim.command('MRU', function()
  FZF({
    source = vim.split(vim.fn.execute('oldfiles'), '\n'),
    sink = function(file)
      vim.cmd(string.format('e %s', vim.split(file, ':')[2]))
    end,
  })
end)

require('amirrezaask.nvim').mode_map({
  n = normal_maps,
})
return fzf

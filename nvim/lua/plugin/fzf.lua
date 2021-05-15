local normal_maps = {}
local nvim = require('amirrezaask.nvim')

local repos = require('amirrezaask.repos')
local fzf = {}

-- vim.g.fzf_layout = { down = '40%' }
vim.g.fzf_layout = { window = { width = 0.8, height = 0.8 }}

vim.g.fzf_action = {
  ['ctrl-v'] = 'vsplit',
  ['ctrl-s'] = 'split'
}
normal_maps['i <leader><leader>'] = '<cmd>Files<CR>'
normal_maps['i <leader>ec'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles<CR>'
normal_maps['i <leader>en'] = '<cmd>Files /home/amirreza/.config/nvim<CR>'
normal_maps['i <leader>ez'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles/zsh<CR>'
normal_maps['i <leader>fp'] = '<cmd>Files /home/amirreza/.local/share/nvim/site/pack/packer/start<CR>'
normal_maps['i <leader>gf'] = '<cmd>GFiles<CR>'
normal_maps['i <C-p>'] = '<cmd>GFiles<CR>'
normal_maps['i <leader>fr'] = '<cmd>MRU<CR>'
normal_maps['i <leader>pf'] = '<cmd>lua Projects({"/home/amirreza/src"})<CR>'
normal_maps['i ??'] = '<cmd>Rg<CR>'
normal_maps['i <leader>b'] = '<cmd>Buffers<CR>'
normal_maps['i <leader>c'] = '<cmd>Commands<CR>'
normal_maps['i <leader>fh'] = '<cmd>History<CR>'
normal_maps['i <leader>h'] = '<cmd>Helptags<CR>'
normal_maps['i <leader>gc'] = '<cmd>Commits<CR>'
normal_maps['i <leader>gb'] = '<cmd>BCommits<CR>'
normal_maps['i <leader>gs'] = '<cmd>GitFiles?<CR>'
normal_maps['i <leader>fb'] = function()
  vim.cmd(string.format('Files %s', vim.fn.expand('%:p:h')))
end
normal_maps['i <leader>sf'] = function()
  FZF {
    source = repos.list_projects({ '~/src/' }),
    sink = function(line)
      vim.cmd([[ cd ]] .. line)
    end
  }
end

normal_maps['i <leader>ef'] = function()
  FZF {
    source = repos.list_projects({ '~/src/gitlab.espadev.ir' }),
    sink = function(line)
      vim.cmd([[ cd ]] .. line)
    end
  }
end

normal_maps['i <leader>ep'] = function()
  FZF {
    source = repos.list_projects({ '~/src/github.com/amirrezaask' }),
    sink = function(line)
      vim.cmd([[ cd ]] .. line)
    end
  }
end


function fzf.lsp_on_attach()
  local buf = vim.lsp.buf
  require('amirrezaask.nvim').map {
      ['n gd'] = function()
        buf.definition()
      end,
      ['n K'] = buf.hover,
      ['n gI'] = WrapQuickfix(buf.implementation),
      ['n gR'] = WrapQuickfix(buf.references),
      ['n <leader>lR'] = buf.rename,
      ['n <leader>lr'] = WrapQuickfix(buf.references),
      ['n <leader>li'] = WrapQuickfix(buf.implementation),
      ['n <leader>ld'] = WrapQuickfix(buf.document_symbol),
      ['n <leader>lw'] = function()
        local query = vim.fn.input('Query: ')
        WrapQuickfix(function() buf.workspace_symbol(query) end)()
      end,
      ['n <leader>lc'] = buf.code_action,
      -- TODO(amirreza): fix these :)
      -- ['<leader>d?'] = ,
      -- ['<leader>w?'] = function()
      --   require('telescope.builtin').lsp_workspace_diagnostics()
      -- end,
  }
end

function FZF(opts)
  opts = opts or {}
  local fzf_args = vim.g.fzf_layout
  fzf_args.source = opts.source
  fzf_args.sink = opts.sink
  vim.fn.call('fzf#run', { fzf_args })
end


function fzf.qflist()
  local list = vim.fn.getqflist()
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
end

function WrapQuickfix(callback)
  return function()
    vim.fn.setqflist({})
    if callback then callback() end
    local list
    vim.wait(2000, function()
      list = vim.fn.getqflist()
      return #list ~= 0
    end, 20, false)
    fzf.qflist()
    vim.cmd [[ cclose ]]
  end
end

nvim.command('MRU', function()
  FZF({
    source = vim.split(vim.fn.execute('oldfiles'), '\n'),
    sink = function(file)
      vim.cmd(string.format('e %s', vim.split(file, ':')[2]))
    end,
  })
end)

require('amirrezaask.nvim').map(normal_maps)
return fzf

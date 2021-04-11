local normal_maps = {}

local function open_file_at(filename, line)
  vim.api.nvim_command(string.format('e +%s %s', line, filename))
end

vim.g.fzf_layout = { down = '40%' }
normal_maps['<Space><Space>'] = '<cmd>Files<CR>'
normal_maps['<Space>ec'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles<CR>'
normal_maps['<Space>en'] = '<cmd>Files /home/amirreza/.config/nvim<CR>'
normal_maps['<Space>ez'] = '<cmd>Files /home/amirreza/src/github.com/amirrezaask/dotfiles/zsh<CR>'
normal_maps['<Space>fp'] = '<cmd>Files /home/amirreza/.local/share/nvim/site/pack/packer/start<CR>'
normal_maps['<Space>gf'] = '<cmd>GFiles<CR>'
normal_maps['<C-p>'] = '<cmd>GFiles<CR>'
-- normal_maps['<Space>fr'] = '<cmd>lua require"fuzzy".mru{}<CR>'
-- normal_maps['<Space>pf'] = '<cmd>lua require("fuzzy").projects{locations={"/home/amirreza/src"}}<CR>'
normal_maps['??'] = '<cmd>Rg<CR>'
normal_maps['<Space>b'] = '<cmd>Buffers<CR>'
normal_maps['<Space>c'] = '<cmd>Commands<CR>'
normal_maps['<Space>h'] = '<cmd>History<CR>'
normal_maps['<Space>h'] = '<cmd>Helptags<CR>'
normal_maps['<Space>gc'] = '<cmd>Commits<CR>'
normal_maps['<Space>gb'] = '<cmd>BCommits<CR>'

function FZF(opts)
  opts = opts or {}
  opts.down = opts.down or '40%'
  vim.fn.call('fzf#run', {opts})
end

function LSP_Document_Symbols()
  local opts = {}
  local params = vim.lsp.util.make_position_params()
  params.context = { includeDeclaration = true }
  params.query = '' 
  local results_lsp = vim.lsp.buf_request_sync(0, "textDocument/documentSymbol", params, opts.timeout or 10000)
  local locations = {}
  for _, server_results in pairs(results_lsp) do
    if server_results.result then
      vim.list_extend(locations, vim.lsp.util.symbols_to_items(server_results.result) or {})
    end
  end
  local lines = {}
  for _, loc in ipairs(locations) do
    table.insert(lines, string.format('%s:%s:%s', loc.filename, loc.lnum, loc.text))
  end
  FZF {
    source = lines,
    sink = function(line)
      local segments = vim.split(line, ":")
      open_file_at(segments[1], segments[2])
    end
  }
end
function LSPWorkspace_Symbols()
  local opts = {}
  local params = vim.lsp.util.make_position_params()
  params.context = { includeDeclaration = true }
  params.query = ''
  local results_lsp = vim.lsp.buf_request_sync(0, "workspace/symbol", params, opts.timeout or 10000)
  local locations = {}
  for _, server_results in pairs(results_lsp) do
    if server_results.result then
      vim.list_extend(locations, vim.lsp.util.symbols_to_items(server_results.result) or {})
    end
  end
  local lines = {}
  for _, loc in ipairs(locations) do
    table.insert(lines, string.format('%s:%s:%s', loc.filename, loc.lnum, loc.text))
  end
  FZF {
    source = lines,
    sink = function(line)
      local segments = vim.split(line, ":")
      open_file_at(segments[1], segments[2])
    end,
  }
end

function LSPReferences()
  local opts = {}
  local params = vim.lsp.util.make_position_params()
  params.context = { includeDeclaration = true }
  local results_lsp = vim.lsp.buf_request_sync(0, "textDocument/references", params, opts.timeout or 10000)
  local locations = {}
  for _, server_results in pairs(results_lsp) do
    if server_results.result then
      vim.list_extend(locations, vim.lsp.util.locations_to_items(server_results.result) or {})
    end
  end
  local callback = function(line)
    local segments = vim.split(line, ":")
    open_file_at(segments[1], segments[2])
  end
  opts.callback = callback
  local lines = {}
  for _, loc in ipairs(locations) do
    table.insert(lines, string.format('%s:%s:%s', loc.filename, loc.lnum, loc.text))
  end
  if #lines == 1 then
    opts.callback(lines[1])
    return
  end
  FZF {
    source = lines,
    sink = function(line)
      local segments = vim.split(line, ":")
      open_file_at(segments[1], segments[2])
    end,
  }
end
function LSPImplementations()
  local opts = {}
  local params = vim.lsp.util.make_position_params()
  params.context = { includeDeclaration = true }
  local results_lsp = vim.lsp.buf_request_sync(0, "textDocument/implementations", params, opts.timeout or 10000)
  local locations = {}
  for _, server_results in pairs(results_lsp) do
    if server_results.result then
      vim.list_extend(locations, vim.lsp.util.locations_to_items(server_results.result) or {})
    end
  end
  local callback = function(line)
    local segments = vim.split(line, ":")
    open_file_at(segments[1], segments[2])
  end
  opts.callback = callback
  local lines = {}
  for _, loc in ipairs(locations) do
    table.insert(lines, string.format('%s:%s:%s', loc.filename, loc.lnum, loc.text))
  end
  if #lines == 1 then
    opts.callback(lines[1])
    return
  end
  FZF {
    source = lines,
    sink = function(line)
      local segments = vim.split(line, ":")
      open_file_at(segments[1], segments[2])
    end,
  }
end

local function do_lsp_code_action(code_action)
  if code_action.edit or type(code_action.command) == "table" then
    if code_action.edit then
      vim.lsp.util.apply_workspace_edit(code_action.edit)
    end
    if type(code_action.command) == "table" then
      vim.lsp.buf.execute_command(code_action.command)
    end
  else
    vim.lsp.buf.execute_command(code_action)
  end
end

function LSPCodeActions()
  local opts = {}
  local params = opts.params or vim.lsp.util.make_range_params()

  params.context = {
    diagnostics = vim.lsp.diagnostic.get_line_diagnostics()
  }

  local results_lsp, err = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, opts.timeout or 10000)

  if err then
    print("ERROR: " .. err)
    return
  end

  if not results_lsp or vim.tbl_isempty(results_lsp) then
    print("No results from textDocument/codeAction")
    return
  end

  local _, response = next(results_lsp)
  if not response then
    print("No code actions available")
    return
  end

  local results = response.result
  if not results or #results == 0 then
    print("No code actions available")
    return
  end
  if #results == 1 then
    do_lsp_code_action(results[1])
    return
  end
  local results_titles = {}

  for i,a in ipairs(results) do
    table.insert(results_titles, string.format('%d.%s', i, a.title))
  end
  FZF {
    source = results_titles,
    handler = function(code_action)
      code_action = results[tonumber(vim.split(code_action, '%.')[1])]
      do_lsp_code_action(code_action)
    end
  }
end

function LSPDefenitions()
  local opts = {}
  local params = vim.lsp.util.make_position_params()
  params.context = { includeDeclaration = true }
  local results_lsp = vim.lsp.buf_request_sync(0, "textDocument/definition", params, opts.timeout or 10000)
  local locations = {}
  for _, server_results in pairs(results_lsp) do
    if server_results.result then
      vim.list_extend(locations, vim.lsp.util.locations_to_items(server_results.result) or {})
    end
  end
  local callback = function(line)
    local segments = vim.split(line, ":")
    open_file_at(segments[1], segments[2])
  end
  opts.callback = callback
  local lines = {}
  for _, loc in ipairs(locations) do
    table.insert(lines, string.format('%s:%s:%s', loc.filename, loc.lnum, loc.text))
  end
  if #lines == 1 then
    opts.callback(lines[1])
    return
  end
  FZF {
    source = lines,
    sink = function(line)
      local segments = vim.split(line, ":")
      open_file_at(segments[1], segments[2])
    end,
  }
end


function Projects()
  local opts = {}
  local projects_list = require'projects'.list_projects(opts.locations)
  FZF {
    source = projects_list,
    sink = function (path)
      vim.cmd(string.format([[ cd %s ]], path))
    end
  }
end

function MRU()
  FZF {
    source = vim.split(vim.fn.execute('oldfiles'), '\n'),
    sink = function(file)
      vim.cmd (string.format('e %s', vim.split(file, ':')[2]))
    end,
  }
end


require'nvim'.mode_map({
  n = normal_maps
})


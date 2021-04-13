local lspconfig = require('lspconfig')

local function open_file_at(filename, line)
  vim.api.nvim_command(string.format('e +%s %s', line, filename))
end
local nvim = require'nvim'
if package.loaded['fzf'] then
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

  function LSPDefinitions()
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

  function LSPDeclaration()
    local opts = {}
    local params = vim.lsp.util.make_position_params()
    params.context = { includeDeclaration = true }
    local results_lsp = vim.lsp.buf_request_sync(0, "textDocument/declaration", params, opts.timeout or 10000)
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
end
-- function Projects(locations)
--   local projects_list = require'projects'.list_projects(locations)
--   FZF {
--     source = projects_list,
--     sink = function (path)
--       vim.cmd(string.format([[ cd %s ]], path))
--     end
--   }
-- end


local function get_lua_runtime()
    local result = {};
    for _, path in pairs(vim.api.nvim_list_runtime_paths()) do
        local lua_path = path .. "/lua/";
        if vim.fn.isdirectory(lua_path) then
            result[lua_path] = true
        end
    end

    result[vim.fn.expand("$VIMRUNTIME/lua")] = true

    result[vim.fn.expand("~/build/neovim/src/nvim/lua")] = true

    return result;
end

lspconfig.gopls.setup{}
lspconfig.rust_analyzer.setup{
  on_attach = function()
    require'lsp_extensions'.inlay_hints{}
  end
}

local sumneko_root = "/home/amirreza/.local/lua-language-server"
local sumneko_binary = sumneko_root .. "/bin/Linux/lua-language-server" 

lspconfig.sumneko_lua.setup{
  cmd = {sumneko_binary, "-E", sumneko_root .. "/main.lua"};
  settings = {
    Lua = {
      runtime = {
        version = "LuaJIT"
      },
      diagnostics = {
        enable = true,
        disable = {
          "trailing-space",
        },
        globals = {"vim"}
      },
      workspace = {
        library = vim.list_extend(get_lua_runtime(), {}),
        maxPreload = 1000,
        preloadFileSize = 1000,
      },
    }
  }
}

lspconfig.pyls_ms.setup{}

require'compe'.setup {
  enabled = true;
  autocomplete = true;
  debug = false;
  min_length = 1;
  preselect = 'enable';
  throttle_time = 80;
  source_timeout = 200;
  incomplete_delay = 400;
  max_abbr_width = 100;
  max_kind_width = 100;
  max_menu_width = 100;
  documentation = true;

  source = {
    path = true;
    buffer = true;
    calc = true;
    nvim_lsp = true;
    nvim_lua = true;
    vsnip = true;
  };
}
-- Commands

-- if package.loaded['fzf'] then
--   nvim.command('LSPDefinitions', LSPDefinitions)
--   nvim.command('LSPHover', vim.lsp.buf.hover)
--   nvim.command('LSPSignatureHelp', vim.lsp.buf.signature_help)
--   nvim.command('LSPTypeDefinition', vim.lsp.buf.type_definition)
--   nvim.command('LSPRename', require'lspsaga.rename'.rename)
--   nvim.command('LSPWorkspaceSymbols', LSPWorkspace_Symbols)
--   nvim.command('LSPDocumentSymbols', LSP_Document_Symbols)
--   nvim.command('LSPReferences', LSPReferences)
--   nvim.command('LSPImplementations', LSPImplementations)
--   nvim.command('LSPCodeActions', LSPCodeActions)
--   nvim.command('LSPDeclaration', LSPDeclaration)
-- end

local fuzzy_lsp = require'fuzzy.lsp'
nvim.command('LSPDefinitions', fuzzy_lsp.definitions)
nvim.command('LSPHover', vim.lsp.buf.hover)
nvim.command('LSPSignatureHelp', vim.lsp.buf.signature_help)
nvim.command('LSPTypeDefinition', vim.lsp.buf.type_definition)
nvim.command('LSPRename', require'lspsaga.rename'.rename)
nvim.command('LSPWorkspaceSymbols', fuzzy_lsp.workspace_symbols)
nvim.command('LSPDocumentSymbols', fuzzy_lsp.document_symbols)
nvim.command('LSPReferences', fuzzy_lsp.references)
nvim.command('LSPImplementations', fuzzy_lsp.implementation)
nvim.command('LSPCodeActions', fuzzy_lsp.code_actions)
nvim.command('LSPDeclaration', fuzzy_lsp.declaration)

-- Keybindings
vim.cmd [[ nnoremap <silent> gd    <cmd>LSPDefinitions<CR> ]]
vim.cmd [[ nnoremap <silent> K     <cmd>LSPHover<CR> ]]
vim.cmd [[ nnoremap <silent> gI    <cmd>LSPImplementations<CR> ]]
vim.cmd [[ nnoremap <silent> <c-k> <cmd>LSpSignatureHelp<CR> ]]
vim.cmd [[ nnoremap <silent> 1gD   <cmd>LSPTypeDefinition<CR> ]]
vim.cmd [[ nnoremap <silent> gR    <cmd>LSPReferences<CR> ]]
vim.cmd [[ nnoremap <silent> g0    <cmd>LSPDocumentSymbols<CR> ]]
vim.cmd [[ nnoremap <silent> gW    <cmd>LSPWorkspaceSymbols<CR> ]]
vim.cmd [[ nnoremap <silent> gD    <cmd>LSPDeclaration<CR> ]]
vim.cmd [[ nnoremap <silent> <Space>A    <cmd>LSPCodeActions<CR> ]]
vim.cmd [[ nnoremap <silent> <Space>R     <cmd>LSpRename<CR> ]]

vim.cmd [[ inoremap <silent><expr> <C-Space> compe#complete() ]]
vim.cmd [[ inoremap <silent><expr> <CR>      compe#confirm('<CR>') ]]
vim.cmd [[ inoremap <silent><expr> <C-e>     compe#close('<C-e>') ]]
vim.cmd [[ inoremap <silent><expr> <C-f>     compe#scroll({ 'delta': +4 }) ]]
vim.cmd [[ inoremap <silent><expr> <C-d>     compe#scroll({ 'delta': -4 }) ]]


-- Use <Tab> and <S-Tab> to navigate through popup menu
vim.cmd [[inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"]]
vim.cmd [[inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"]]

-- Set completeopt to have a better completion experience
vim.cmd [[set completeopt=menuone,noselect]]

-- Avoid showing message extra message when using completion
vim.cmd [[set shortmess+=c]]




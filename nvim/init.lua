--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|
--
--------------------------------------------------------------------------------
-- Setting options
--------------------------------------------------------------------------------
vim.opt.smartcase = true -- care about case of chars when we have capital ones in search.
vim.opt.equalalways = false -- don't change windows size after closing one
vim.opt.modeline = true
vim.opt.autoread = true
vim.opt.compatible = false -- no compatibility with vim.
vim.opt.encoding = "utf-8" -- default encoding
vim.opt.hlsearch = true -- highlight matched when searching
vim.opt.history = 700
vim.opt.tabpagemax = 100
vim.opt.ruler = true -- show line/col in statusbar
vim.opt.mouse = "a" -- enable mouse
vim.opt.wrap = true
vim.opt.autoindent = true -- use same indent as previous line
vim.opt.termguicolors = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false
vim.opt.writebackup = false -- no vim backup file
vim.opt.swapfile = false -- disable vim swap files
vim.opt.splitright = true -- always split window to right
vim.opt.splitbelow = true -- always split to below
vim.opt.cursorline = true -- highlight current line
vim.opt.relativenumber = true -- relative line numbers
vim.opt.number = true -- show current line number
vim.opt.pumblend = 5
vim.opt.showmode = false
vim.opt.clipboard = "unnamedplus"
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.wildmode = { "longest", "list", "full" }
vim.opt.wildmode = vim.opt.wildmode - "list"
vim.opt.wildmode = vim.opt.wildmode + { "longest", "full" }
--------------------------------------------------------------------------------
-- Installing plugins
--------------------------------------------------------------------------------

local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
local is_wsl = (function()
  return string.find(vim.fn.systemlist("uname -r")[1] or "", "WSL")
end)()
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "dracula/vim" } -- Theme
    use { 'gruvbox-community/gruvbox' } -- Theme
    use {
      "jghauser/mkdir.nvim",
      config = function()
        require "mkdir"
      end,
    }
    use { "amirrezaask/nline.nvim" } -- Statusline plugin
    use { "L3MON4D3/LuaSnip" } -- Snippets plugin
    use { "tpope/vim-fugitive" }
    use { "windwp/nvim-spectre", requires = { "nvim-lua/plenary.nvim" } }
    use { "nvim-telescope/telescope.nvim", requires = { "nvim-lua/plenary.nvim" } } -- UI to search for things
    use { "tpope/vim-surround" } -- Vim surround objects
    use { "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } } -- Gitsigns
    use { "tpope/vim-commentary" } -- Comment codes at ease
    use { "neovim/nvim-lspconfig" } -- LSP configurations
    use { "norcalli/nvim-colorizer.lua", branch = "color-editor" }
    use { "honza/dockerfile.vim" } -- Dockerfile
    use { "hashivim/vim-terraform" } -- Terraform
    use { "LnL7/vim-nix" } -- Nix
    use { "dag/vim-fish" } -- Fish
    use { "cespare/vim-toml" } -- Toml
    use { "elixir-editors/vim-elixir" } -- Elixir
    use { "pearofducks/ansible-vim" } -- Ansible
    use { "Glench/Vim-Jinja2-Syntax" } -- Jinja2
    use { "amirrezaask/actions.nvim" } -- Define IDE like actions.
    use { "purescript-contrib/purescript-vim" }
    use { "ziglang/zig.vim" }
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "saadparwaiz1/cmp_luasnip" }
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "mfussenegger/nvim-dap" } -- debug adapter protocol
    use { "theHamsta/nvim-dap-virtual-text" } -- debug adapter protocol virtual text
    use { "folke/todo-comments.nvim", requires = "nvim-lua/plenary.nvim" } -- Highlight todo and etc...
    use { "godlygeek/tabular" } -- beautify text
    use { "milisims/nvim-luaref" } -- lua reference as vim help
    use { "nanotee/luv-vimdocs" } -- luv reference as vim help
    use { "lukas-reineke/indent-blankline.nvim" } -- Show indent highlights
    use { "ThePrimeagen/harpoon", requires = { "nvim-lua/plenary.nvim" } }
    use { "fatih/vim-go" }
  end,
}

vim.g.gruvbox_contrast_dark = "hard"
vim.g.gruvbox_contrast_light = "light"
vim.cmd [[ colorscheme gruvbox ]]

--------------------------------------------------------------------------------
-- Keymaps
--------------------------------------------------------------------------------
vim.g.mapleader = " "
vim.cmd [[
nnoremap Q <NOP>
nnoremap ; :
nnoremap q; q:

" Window resizes
nnoremap <Left> :vertical resize -5<CR>
nnoremap <Right> :vertical resize +5<CR>
nnoremap <Up> :resize +5<CR>
nnoremap <Down> :resize -5<CR>

nnoremap j gj
nnoremap k gk

tnoremap <Esc> <C-\><C-n>
tnoremap jk <C-\><C-n>
tnoremap kj <C-\><C-n>

inoremap jk <esc>
inoremap kj <esc>

" Move lines jetbrains style -> Thanks to TJ again
nnoremap <M-j> :m .+1<CR>==
nnoremap <M-k> :m .-2<CR>==

inoremap <M-j> <Esc>:m .+1<CR>==gi
inoremap <M-k> <Esc>:m .-2<CR>==gi

vnoremap <M-j> :m '>+1<CR>gv=gv
vnoremap <M-k> :m '<-2<CR>gv=gv

nnoremap Y y$
nnoremap n nzz
nnoremap N "Nzz

nnoremap { :cprev<CR>
nnoremap } :cnext<CR>

" Thanks to TJ again
nnoremap <expr><CR> {-> v:hlsearch ? ":nohl<CR>" : "<CR>"}()
]]

--------------------------------------------------------------------------------
-- Netrw
--------------------------------------------------------------------------------
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

--------------------------------------------------------------------------------
-- Treesitter
--------------------------------------------------------------------------------
require("nvim-treesitter.configs").setup {
  ensure_installed = {'php', 'go', 'lua', 'python' },
  highlight = {
    enable = true, -- false will disable the whole extension
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-a>",
      node_incremental = "<C-a>",
      scope_incremental = "grc",
      node_decremental = "<M-a>",
    },
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
}

--------------------------------------------------------------------------------
-- Telescope
--------------------------------------------------------------------------------
local telescope_actions = require "telescope.actions"

require("telescope").setup {
  defaults = {
    layout_strategy = "flex",
    layout_config = {
      width = 0.9,
      height = 0.8,

      horizontal = {
        width = { padding = 0.15 },
      },
      vertical = {
        preview_height = 0.75,
      },
    },
    file_ignore_patterns = {
      "__pycache__",
    },
    mappings = {
      n = {
        ["<ESC>"] = telescope_actions.close,
        ["<C-c>"] = telescope_actions.close,
        ["jk"] = telescope_actions.close,
        ["kj"] = telescope_actions.close,
      },
      i = {
        ["<C-c>"] = telescope_actions.close,
        ["<C-q>"] = telescope_actions.send_to_qflist,
        ["<C-j>"] = telescope_actions.move_selection_next,
        ["<C-k>"] = telescope_actions.move_selection_previous,
      },
    },
  },
}

require("telescope").load_extension "harpoon"

vim.cmd [[
  nnoremap <leader><leader> <cmd>Telescope find_files <CR>

  nnoremap <leader>fp <cmd>Telescope find_files hidden=true cwd=~/.local/share/nvim/site/pack/packer<CR>

  nnoremap <leader>ps <cmd>Telescope find_files hidden=true cwd=~/src/gitlab.snapp.ir<CR>

  nnoremap <C-q> <cmd>Telescope quickfix <CR>

  nnoremap ?? <cmd>Telescope live_grep <CR>

  nnoremap <leader>en <cmd>Telescope find_files cwd=~/.config/nvim<CR>
]]


vim.cmd [[
    autocmd BufEnter *.lua setlocal ts=2 | setlocal sts=2 | setlocal expandtab | setlocal shiftwidth=2
]]

--------------------------------------------------------------------------------
-- Actions.nvim
--------------------------------------------------------------------------------
local actions = require "actions"
local utils = require "actions.utils"

actions:setup {
  mappings = {
    ["n ,ab"] = "build",
    ["n ,at"] = "test_all",
    ["n ,tt"] = "test_this",
    ["n ,ar"] = "run",
    ["n ,af"] = "format",
  },
  {
    predicate = utils.make_language_predicate "vim",
    actions = {
      run = function(_)
        vim.cmd [[ so % ]]
      end,
    },
  },
  {
    predicate = utils.compose(utils.make_language_predicate "lua", utils.make_path_predicate "plugins.lua"),
    actions = {
      run = function(bufnr)
        vim.cmd [[ luafile % ]]
        vim.cmd [[ PackerInstall ]]
      end,
    },
  },
  {
    predicate = utils.make_language_predicate "lua",
    actions = {
      run = function(_)
        vim.cmd [[ so % ]]
      end,
      format = function(bufnr)
        stylua_format(bufnr) 
      end,
    },
  },
  {
    predicate = utils.make_language_predicate "rust",
    actions = {
      format = function()
        vim.lsp.buf.formatting_sync()
      end,
      run = function(_)
        vim.cmd [[ vnew | term cargo run ]]
      end,
      build = function(_)
        vim.cmd [[ vnew | term cargo check ]]
      end,
      test_all = function(_)
        vim.cmd [[ RustTest! ]]
      end,
      test_this = function(_)
        vim.cmd [[ RustTest ]]
      end,
    },
  },
  {
    predicate = utils.make_path_predicate "gitlab.snapp.ir",
    actions = {
      format = function() end,
    },
  },
  {
    predicate = utils.make_language_predicate "go",
    actions = {
      format = function(_)
        GoFormat()
      end,
      build = function(_)
        vim.cmd [[ vnew | term go build ]]
      end,
      run = function()
        vim.cmd [[ vnew | term go run *.go ]]
      end,
      test_all = function(_)
        vim.cmd [[ vnew | term go test -v ./... ]]
      end,
      test_this = function(_)
        local function go_current_test()
          local linenr = vim.fn.search("func \\(Test\\|Example\\)", "bcnW")
          if linenr == 0 then
            return
          end
          local linetext = vim.fn.getline(linenr)
          local test_name = vim.split(linetext, " ")[2]
          local start, _ = string.find(test_name, "%(")
          test_name = string.sub(test_name, 1, start - 1)
          return test_name
        end
        local current_test = go_current_test()
        vim.cmd(string.format([[ vnew | term go test -v -run %s ]], current_test))
      end,
    },
  },
}

vim.cmd [[ autocmd BufWritePre *.rs lua Actions:exec(0, 'format') ]]
vim.cmd [[ autocmd BufWritePre *.go lua Actions:exec(0, 'format') ]]

--------------------------------------------------------------------------------
-- Git signs and popups
--------------------------------------------------------------------------------
require("gitsigns").setup {
  signs = {
    add = { text = "|", numhl = "GitSignsAddNr" },
    change = { text = "|", numhl = "GitSignsChangeNr" },
    delete = { text = "_", numhl = "GitSignsDeleteNr" },
    topdelete = { text = "‾", numhl = "GitSignsDeleteNr" },
    changedelete = { text = "~-", numhl = "GitSignsChangeNr" },
  },
  numhl = false,
  current_line_blame = false,
  current_line_blame_opts = {
    delay = 800,
    virt_text_pos = "eol",
  },
}

vim.cmd [[ 
  nnoremap <leader>gb :Gitsigns blame_line<CR>
  nnoremap <C-k> :G<CR>
  nnoremap <leader>gc :Telescope git_branches<CR>
]]

--------------------------------------------------------------------------------
-- Harpoon
--------------------------------------------------------------------------------
vim.cmd [[
  nnoremap <leader>mp <cmd>lua require"harpoon.ui".nav_prev()<CR>
  nnoremap <leader>mn <cmd>lua require"harpoon.ui".nav_next()<CR>
  nnoremap <leader>m1 <cmd>lua require"harpoon.ui".nav_file(1)<CR>
  nnoremap <leader>m2 <cmd>lua require"harpoon.ui".nav_file(2)<CR>
  nnoremap <leader>m3 <cmd>lua require"harpoon.ui".nav_file(3)<CR>
  nnoremap <leader>ma <cmd>lua require"harpoon.mark".add_file()<CR>
  nnoremap <leader>mm <cmd>lua require"harpoon.ui".toggle_quick_menu()<CR>
]]

--------------------------------------------------------------------------------
-- Golang
--------------------------------------------------------------------------------
function GoFormat(bufnr)
  bufnr = bufnr or 0
  vim.cmd [[ write ]]
  local job = require("plenary.job"):new {
    "goimports",
    vim.api.nvim_buf_get_name(0),
  }

  local output = job:sync()

  if job.code ~= 0 then
    return
  end

  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, output)
end
vim.g.go_fmt_autosave = 0
vim.g.go_imports_autosave = 0


--------------------------------------------------------------------------------
-- Indent lines
--------------------------------------------------------------------------------
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { "help", "packer" }
vim.g.indent_blankline_buftype_exclude = { "terminal", "nofile" }
vim.g.indent_blankline_show_trailing_blankline_indent = false
vim.g.indent_blankline_show_current_context = false
vim.g.indent_blankline_use_treesitter = true
vim.g.indent_blankline_filetype = {"yaml", "json"}

--------------------------------------------------------------------------------
-- highlight on yank
--------------------------------------------------------------------------------
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]]


--------------------------------------------------------------------------------
-- LSP
--------------------------------------------------------------------------------
local lspconfig = require "lspconfig"
local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", { silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", { silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", { silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n","?d",'<cmd>Telescope lsp_document_symbols<CR>',{ silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n","?w",'<cmd>Telescope lsp_workspace_symbols<CR>',{ silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n","?c",'<cmd>Telescope lsp_code_actions<CR>',{ silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "R", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "i", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-d>", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)

  vim.cmd [[ command! Format execute '<cmd>lua vim.lsp.buf.formatting()' ]]

  if client.resolved_capabilities.document_highlight then -- highlight current symbol usages in code
    vim.cmd [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
  ]]
  end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local servers = { "clangd", "rust_analyzer", "gopls", "intelephense", "jedi_language_server", "hls", "purescriptls" }

for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end

local sumneko_root = string.format("%s/.local/lua-language-server", os.getenv("HOME"))
local sumneko_binary = sumneko_root .. "/bin/lua-language-server"

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

require("lspconfig").sumneko_lua.setup {
  cmd = { sumneko_binary, "-E", sumneko_root .. "/main.lua" },
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
        -- Setup your lua path
        path = runtime_path,
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}

require"lspconfig".elixirls.setup {
  cmd = { os.getenv("HOME") .. "/.local/elixir-ls/language_server.sh" }

}


--------------------------------------------------------------------------------
-- Autocomplete menu
--------------------------------------------------------------------------------
vim.opt.completeopt = { "menuone", "noselect" }

-- Don't show the dumb matching stuff.
vim.opt.shortmess:append "c"

local cmp = require "cmp"
local luasnip = require "luasnip"
cmp.setup {
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },

  -- You can set mapping if you want.
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Insert,
      select = false,
    },
    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif luasnip.expand_or_jumpable() then
        luasnip.expand_or_jump()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif luasnip.jumpable(-1) then
        luasnip.jump(-1)
      else
        fallback()
      end
    end,
  },

  -- You should specify your *installed* sources.
  sources = {
    { name = "buffer" },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}

--------------------------------------------------------------------------------
-- Stylua, Lua autoformat
--------------------------------------------------------------------------------
local Job = require "plenary.job"
local Path = require "plenary.path"

local Stylua = {}

local cached_configs = {}

local lspconfig_util = require "lspconfig.util"
local root_finder = lspconfig_util.root_pattern ".git"

--@param bufnr: number
local function stylua_get_config_path(bufnr)
  local path = vim.api.nvim_buf_get_name(bufnr)
  if cached_configs[path] == nil then
    local file_path = Path:new(path)
    local root_path = Path:new(root_finder(path))

    local file_parents = file_path:parents()
    local root_parents = root_path:parents()

    local relative_diff = #file_parents - #root_parents
    for index, dir in ipairs(file_parents) do
      if index > relative_diff then
        break
      end

      local stylua_path = Path:new { dir, "stylua.toml" }
      if stylua_path:exists() then
        cached_configs[path] = stylua_path:absolute()
        break
      end

      stylua_path = Path:new { dir, ".stylua.toml" }
      if stylua_path:exists() then
        cached_configs[path] = stylua_path:absolute()
        break
      end
      stylua_path = Path:new { os.getenv "HOME", ".stylua.toml" }
      if stylua_path:exists() then
        cached_configs[path] = stylua_path:absolute()
        break
      end
    end
  end

  return cached_configs[path]
end

--@param bufnr: number
function stylua_format(bufnr)
  local config_path = stylua_get_config_path(bufnr)
  local job = Job:new {
    "stylua",
    "--config-path",
    config_path,
    "-",
    writer = vim.api.nvim_buf_get_lines(0, 0, -1, false),
  }
  local output = job:sync()

  if job.code ~= 0 then
    print "cannot format"
    return
  end
  vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, output)
end


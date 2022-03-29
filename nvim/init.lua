--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|
--
-- Colorscheme stuff
vim.cmd [[ colorscheme sitruuna ]]

-- Basic Keymaps
vim.g.mapleader = " "
local map = function(mode, lhs, rhs) vim.api.nvim_set_keymap(mode, lhs, rhs, {silent=true, noremap=true}) end
map('n', "Q", "<NOP>")
map('n', ';', ':')
map('n', 'q;', 'q:')

map('n', '<Left>', ':vertical resize -5<CR>')
map('n', '<Right>', ':vertical resize +5<CR>')
map('n', '<Up>', ':resize +5<CR>')
map('n', '<Down>', ':resize -5<CR>')

map('n', 'j', 'gj')
map('n', 'k', 'gk')

map('t', '<Esc>', '<C-\\><C-n>')
map('t', 'jk', '<C-\\><C-n>')
map('t', 'kj', '<C-\\><C-n>')

map('i', 'jk', '<esc>')
map('i', 'kj', '<esc>')

map('n', 'Y', 'y$')
map('n', 'n', 'nzz')
map('n', 'N', '"Nzz')

map('n', '<M-t>', ":tabnew<CR>")
map('n', '<M-p>', ":tabprev<CR>")
map('n', '<M-n>', ":tabnext<CR>")

vim.cmd [[ 
  nnoremap <M-j> :m .+1<CR>==
  nnoremap <M-k> :m .-2<CR>==
  inoremap <M-j> <Esc>:m .+1<CR>==gi
  inoremap <M-k> <Esc>:m .-2<CR>==gi
  vnoremap <M-j> :m '>+1<CR>gv=gv
  vnoremap <M-k> :m '<-2<CR>gv=gv
]]

map('n', '{', ':cprev<CR>')
map('n', '}', ':cnext<CR>')

vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ":nohl<CR>" : "<CR>"}() ]]

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
vim.opt.showmode = false
vim.opt.clipboard = "unnamedplus"
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.wildmode = { "longest", "list", "full" }
vim.opt.wildmode = vim.opt.wildmode - "list"
vim.opt.wildmode = vim.opt.wildmode + { "longest", "full" }

-- statusline
vim.opt.laststatus=3 -- global statusline -- needs neovim HEAD
vim.opt.statusline='%{FugitiveStatusline()} %= %q%w%h%r%m%f %= %l:%c:%L'


-- Plugins
require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "jghauser/mkdir.nvim", config = function() require "mkdir" end } -- Mkdir
    use { "windwp/nvim-spectre", requires = { "nvim-lua/plenary.nvim" } }
    use { "tpope/vim-surround" } -- Vim surround objects
    use { "neovim/nvim-lspconfig" } -- LSP configurations
    use { "honza/dockerfile.vim" } -- Dockerfile
    use { "hashivim/vim-terraform" } -- Terraform
    use { "LnL7/vim-nix" } -- Nix
    use { "dag/vim-fish" } -- Fish
    use { "cespare/vim-toml" } -- Toml
    use { "elixir-editors/vim-elixir" } -- Elixir
    use { "pearofducks/ansible-vim" } -- Ansible
    use { "Glench/Vim-Jinja2-Syntax" } -- Jinja2
    use { "ziglang/zig.vim" } -- Zig language Support
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "lukas-reineke/indent-blankline.nvim" } -- Show indent highlights
    use { "fatih/vim-go" } -- Golang IDE
    use { 'fladson/vim-kitty' }
    use { 'vim-erlang/vim-erlang-runtime' }
    use { 'junegunn/fzf' }
    use { 'junegunn/fzf.vim' }
    use { 'eemed/sitruuna.vim' }
    use { 'luisiacc/gruvbox-baby' }
    use { 'ap/vim-buftabline' }
    use { 'tpope/vim-commentary' }
  end,
}


-- Treesitter
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

-- FZF
vim.g.fzf_layout = { ['down'] = '40%' }
vim.g.fzf_preview_window = {}
vim.cmd [[
  nnoremap <leader><leader> <cmd>Files<CR>
  nnoremap <leader>fp <cmd>Files ~/.local/share/nvim/site/pack/packer<CR>
  nnoremap <leader>ps <cmd>Files ~/src/gitlab.snapp.ir<CR>
  nnoremap <leader>en <cmd>Files ~/.config/nvim<CR>
  nnoremap ?? <cmd>Rg<CR>
]]

-- Autocomplete
vim.opt.completeopt = { "menuone", "noselect" }

-- Don't show the dumb matching stuff.
vim.opt.shortmess:append "c"


local cmp = require "cmp"
cmp.setup {
  snippet = {
      expand = function(args)
        require('luasnip').lsp_expand(args.body)
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
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },

  -- You should specify your *installed* sources.
  sources = {
    { name = "buffer" },
    -- { name = 'luasnip' },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}


local lspconfig = require "lspconfig"
local on_attach = function(_, bufnr)
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
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local servers = { "clangd", "rust_analyzer", "gopls", "intelephense", "jedi_language_server", "hls", "purescriptls", "zls" }

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
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
      workspace = {
        library = vim.api.nvim_get_runtime_file("", true),
      },
      telemetry = {
        enable = false,
      },
    },
  },
}

require"lspconfig".elixirls.setup {
  cmd = { os.getenv("HOME") .. "/.local/elixir-ls/language_server.sh" }
}

local function make_listchars_str(tbl)
  local tbl_pairs = {}
  for name, value in pairs(tbl) do
    table.insert(tbl_pairs, string.format("%s:%s", name, value))
  end

  return table.concat(tbl_pairs, ",")
end
local str = make_listchars_str({
  eol = '↲',
  tab = '» ',
  trail = '·',
  extends= '<',
  precedes= '>',
  conceal= '┊',
  nbsp= '␣',
})

vim.opt.list = true
vim.opt.listchars = str

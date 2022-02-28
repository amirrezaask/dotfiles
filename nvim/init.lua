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
require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "dracula/vim" } -- dracula Theme
    use { 'gruvbox-community/gruvbox' } -- gruvbox Theme
    use { "jghauser/mkdir.nvim", config = function() require "mkdir" end } -- Mkdir
    use { 'chriskempson/base16-vim' } -- Colorschemes
    use { "itchyny/lightline.vim" } -- Statusline
    use { 'shinchu/lightline-gruvbox.vim' } -- Statusline theme
    use { "tpope/vim-fugitive" } -- Vim Git bindings
    use { "windwp/nvim-spectre", requires = { "nvim-lua/plenary.nvim" } }
    use { "nvim-telescope/telescope.nvim", requires = { "nvim-lua/plenary.nvim" } } -- UI to search for things
    use { 'junegunn/fzf' }
    use { 'junegunn/fzf.vim' }
    use { "tpope/vim-surround" } -- Vim surround objects
    use { "tpope/vim-commentary" } -- Comment codes at ease
    use { "neovim/nvim-lspconfig" } -- LSP configurations
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
    use { "ziglang/zig.vim" } -- Zig language Support
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "godlygeek/tabular" } -- Beautify text
    use { "lukas-reineke/indent-blankline.nvim" } -- Show indent highlights
    use { "fatih/vim-go" } -- Golang IDE
    use { 'hrsh7th/vim-vsnip' } -- Snippet plugin
    use { 'hrsh7th/vim-vsnip-integ' }
    use { 'kyazdani42/nvim-web-devicons' }
    use { 'yamatsum/nvim-web-nonicons' }
  end,
}
--------------------------------------------------------------------------------
-- Colorscheme
--------------------------------------------------------------------------------

vim.g.gruvbox_contrast_dark = "hard"
vim.g.gruvbox_contrast_light = "light"
vim.cmd [[ colorscheme gruvbox ]]
vim.g.lightline = {
  colorscheme = 'gruvbox',
}
--------------------------------------------------------------------------------
-- Keymaps
--------------------------------------------------------------------------------
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

--------------------------------------------------------------------------------
-- Netrw
--------------------------------------------------------------------------------
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25

--------------------------------------------------------------------------------
-- FileTypes
--------------------------------------------------------------------------------

vim.cmd [[
augroup FileTypes
  autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o 
  autocmd FileType yaml setlocal cursorcolumn
augroup END
]]
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
----------------------------------------------------------------------
-- Telescope
----------------------------------------------------------------------
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

vim.cmd [[
  nnoremap <leader><leader> <cmd>Telescope find_files<CR>
  nnoremap <leader>fp <cmd>Telescope find_files cwd=~/.local/share/nvim/site/pack/packer<CR>
  nnoremap <leader>ps <cmd>Telsccope find_files cwd=~/src/gitlab.snapp.ir<CR>
  nnoremap <leader>en <cmd>Telescope find_files cwd=~/.config/nvim<CR>
  nnoremap ?? <cmd>Telescope live_grep<CR>
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
      run = function(_)
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
        vim.cmd([[GoFmt]])
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
-- Git
--------------------------------------------------------------------------------
vim.cmd [[ 
  nnoremap <leader>gb :Git blame<CR>
  nnoremap <leader>gc :Telescope git_branches<CR>
]]

--------------------------------------------------------------------------------
-- Golang
--------------------------------------------------------------------------------
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
-- Highlight on yank
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
  settings = {
    Lua = {
      diagnostics = {
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
cmp.setup {
  snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
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
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}


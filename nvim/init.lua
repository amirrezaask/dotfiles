-- Install packages
require('packer').startup(function(use)
    use 'folke/tokyonight.nvim'
    use 'wbthomason/packer.nvim'
    use 'jnurmine/Zenburn'
    use 'eemed/sitruuna.vim'                              -- Best Minimal Colorscheme if you like black,yellow and green colors
    use 'joshdick/onedark.vim'
    use 'gruvbox-community/gruvbox'                       -- Popular gruvbox
    use 'sainnhe/sonokai'
    use 'windwp/nvim-spectre'                         -- Search/Replace project wide
    use 'nvim-treesitter/nvim-treesitter'             -- Treesitter syntax highlighting
    use 'nvim-treesitter/nvim-treesitter-textobjects' -- Treesitter text objects
    use 'hrsh7th/nvim-cmp'                            -- Neovim auto complete menu
    use 'hrsh7th/cmp-buffer'                          -- auto complete buffer source
    use 'hrsh7th/cmp-nvim-lua'                        -- auto complete nvim lua stuff source
    use 'hrsh7th/cmp-nvim-lsp'                        -- auto complete lsp source
    use 'hrsh7th/cmp-path'                            -- auto complete os path source
    use 'neovim/nvim-lspconfig'                       -- LSP client configurations
    use 'nvim-telescope/telescope.nvim'               -- Telescope fuzzy finder by great TJDevries
    use 'nvim-lua/plenary.nvim'                       -- Neovim stdlib lua by TJDevries
    use 'kyazdani42/nvim-web-devicons'
    use 'hrsh7th/vim-vsnip'                               -- Snippets
    use 'junegunn/fzf'                                    -- Google of the command line
    use 'junegunn/fzf.vim'                                -- Integrate fzf into vim as commands
    use 'sheerun/vim-polyglot'                            -- Basic vim support for multiple languages see https://github.com/sheerun/vim-polyglot for the full list.
    use 'Glench/Vim-Jinja2-Syntax'                        -- Jinja2 syntax
    use 'ziglang/zig.vim'                                 -- Best language ever ?
    use 'rust-lang/rust.vim'                              -- Haskell on LLVM ?
    use 'fladson/vim-kitty'                               -- Best Terminal Emulator config syntax
    use 'pbrisbin/vim-mkdir'                              -- Save files and create not existing directories
    use 'tpope/vim-commentary'                            -- Best commenting plugin ever
    use 'tpope/vim-surround'                              -- Now you can command your surroundings
    use 'tpope/vim-fugitive'                              -- Best Vim Git client
    use 'junegunn/gv.vim'                                 -- Git diff split
    use 'cohama/agit.vim'                                 -- Git log viewer
    use 'junegunn/vim-easy-align'                         -- Align text with ease
end)

-- bind(modes, key, fn)
local bind = vim.keymap.set

local function nnoremap(key, fn)
    bind('n', key, fn)
end

local function inoremap(key, fn)
    bind('i', key, fn)
end

local function tnoremap(key, fn)
    bind('t', key, fn)
end

local function vnoremap(key, fn)
    bind('v', key, fn)
end



-- Set <Space> as <leader>
vim.g.mapleader = ' '
-- basic keybindings
nnoremap("Q", '<NOP>')
nnoremap(";,", ':')
nnoremap('q;', 'q:')

nnoremap('<Left>', ':vertical resize -5<CR>')
nnoremap('<Right>', ':vertical resize +5<CR>')
nnoremap('<Up>', ':resize +5<CR>')
nnoremap('<Down>', ':resize -5<CR>')

nnoremap('j', 'gj')
nnoremap('k', 'gk')

tnoremap('<Esc>', '<C-\\><C-n>')
tnoremap('jk','<C-\\><C-n>')
tnoremap('kj','<C-\\><C-n>')

inoremap('jk', '<esc>')
inoremap('kj', '<esc>')

nnoremap('Y', 'y$')
nnoremap('n', 'nzz')
nnoremap('N', 'Nzz')

nnoremap('<M-p>', ':bprev<CR>')
nnoremap('<M-n>', ':bnext<CR>')

nnoremap('<M-j>', ':m .+1<CR>==')
nnoremap('<M-k>', ':m .-2<CR>==')

inoremap('<M-j>', '<Esc>:m .+1<CR>==gi')
inoremap('<M-k>', '<Esc>:m .-2<CR>==gi')

vnoremap('<M-k>', ':m >+1<CR>gv=gv')
vnoremap('<M-k>', '<Esc>:m .-2<CR>==gi')

nnoremap('{', ':cprev<CR>')
nnoremap('}', ':cnext<CR>')

nnoremap("<C-h>", ":tabprev<CR>")
nnoremap("<C-l>", ":tabnext<CR>")
nnoremap("<C-n>", ":tabnew<CR>")


vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]]


-- options
vim.opt.ignorecase = true -- ignore case when searching
vim.opt.smartcase = true --don't ignore case in search when there is uppercase letter
vim.opt.equalalways = false -- don't resize windows on changing window state ( closing or splitting )
vim.opt.modeline = true --
vim.opt.autoread = true -- If you detect a file change read it automatically
vim.opt.compatible = false -- no need legacy VI compatibility
vim.opt.encoding = 'utf-8' -- Default file encoding
vim.opt.hlsearch = true -- Highlight search matches
vim.opt.history= 700 -- Number of stored history items
vim.opt.tabpagemax = 100 -- Max number of tabs
vim.opt.ruler = true -- Show line and column in statusline
vim.opt.mouse='a' -- enable mouse support for all modes
vim.opt.wrap = true
vim.opt.autoindent = true
vim.opt.cindent = true
vim.opt.wrap = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false 
vim.opt.writebackup = false 
vim.opt.swapfile = false 
vim.opt.splitright = true
vim.opt.splitbelow = true
vim.opt.cursorline = true    -- highlight current line
vim.opt.relativenumber = true -- show relative line numbers
vim.opt.number = true        -- show current line number
vim.opt.showmode = true      -- show current vim mode down of the screen
vim.opt.showcmd = true       -- show commands as they are being typed
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.incsearch = true      -- Continue search as I type characters
vim.opt.guioptions ='egmrti'
vim.opt.backspace = 'indent,eol,start'
vim.opt.complete = vim.opt.complete + 'i'    -- don't search for all included files
vim.opt.wildmenu = true
vim.opt.wildoptions = 'tagfile'
vim.opt.updatetime = 300
vim.opt.pumheight = 10   -- Completion window max size
vim.opt.conceallevel = 2 -- Concealed text is completely hidden
vim.opt.shortmess = vim.opt.shortmess + 'c'   -- Shut off completion messages
vim.opt.belloff = vim.opt.belloff + 'ctrlg' -- If Vim beeps during completion
vim.opt.lazyredraw = true
vim.opt.termguicolors = true
vim.cmd [[ set clipboard^=unnamedplus ]]

vim.g.tokyonight_style = "night"
vim.cmd [[ colorscheme tokyonight ]]

-- Telescope
local telescope_builtin = require "telescope.builtin"
nnoremap('<leader><leader>', function() telescope_builtin.find_files() end)
nnoremap('??', '<cmd>Telescope live_grep<CR>')
nnoremap('?c', '<cmd>Telescope lsp_code_actions<CR>')

-- LSP
local lspconfig = require "lspconfig"
local on_attach = function(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", { silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gi", "<cmd>lua vim.lsp.buf.implementation()<CR>", { silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gr", "<cmd>lua vim.lsp.buf.references()<CR>", { silent = true, noremap = true })
  vim.api.nvim_buf_set_keymap(bufnr, "n", "R", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "i", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-s>", "<cmd>lua vim.lsp.buf.signature_help()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-d>", "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "[d", "<cmd>lua vim.diagnostic.goto_prev()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "]d", "<cmd>lua vim.diagnostic.goto_next()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<leader>ca", "<cmd>Telescope lsp_code_actions<CR>", opts)

  vim.cmd [[ command! Format execute '<cmd>lua vim.lsp.buf.formatting()' ]]
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

require("lspconfig").clangd.setup {
   on_attach = on_attach,
}
require("lspconfig").rust_analyzer.setup {
   on_attach = on_attach,
}
require("lspconfig").gopls.setup {
   on_attach = on_attach,
}
require("lspconfig").intelephense.setup {
   on_attach = on_attach,
}
require("lspconfig").jedi_language_server.setup {
   on_attach = on_attach,
}
require("lspconfig").hls.setup {
   on_attach = on_attach,
}
require("lspconfig").purescriptls.setup {
   on_attach = on_attach,
}
require("lspconfig").zls.setup {
   on_attach = on_attach,
}

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
   on_attach = on_attach,
   cmd = { os.getenv("HOME") .. "/.local/elixir-ls/language_server.sh" }

}
vim.opt.completeopt = { "menuone", "noselect" }

vim.opt.shortmess:append "c"

-- AutoComplete
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

require'nvim-treesitter.configs'.setup {
  textobjects = {
    move = {
          enable = true,
          set_jumps = true, -- whether to set jumps in the jumplist
          goto_next_start = {
            ["]m"] = "@function.outer",
            ["]]"] = "@class.outer",
          },
          goto_next_end = {
            ["]M"] = "@function.outer",
            ["]["] = "@class.outer",
          },
          goto_previous_start = {
            ["[m"] = "@function.outer",
            ["[["] = "@class.outer",
          },
          goto_previous_end = {
            ["[M"] = "@function.outer",
            ["[]"] = "@class.outer",
          },
        },
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

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
vim.cmd [[
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

let g:netrw_browse_split = 0
let g:netrw_banner = 0
let g:netrw_winsize = 25

" Golang
let g:go_fmt_autosave = 1
let g:go_imports_autosave = 1

" Yaml
autocmd FileType yaml setlocal cursorcolumn

" Zig

" Easy Align
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign) 
]]

vim.g.zig_fmt_autosave = 1

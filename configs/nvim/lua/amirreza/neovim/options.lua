vim.o.undofile = true
vim.o.swapfile = false
vim.o.number = true
vim.o.relativenumber = true
vim.o.signcolumn = "yes"
vim.o.cursorline = true
vim.o.scrolloff = 10
vim.o.linebreak = true
vim.o.winborder = "rounded"
vim.o.laststatus = 3
vim.o.showcmd = false
vim.o.title = true
vim.o.titlestring = "%{fnamemodify(getcwd(), ':~')}"
vim.o.mouse = "a"
vim.o.autoread = true
vim.o.shortmess = vim.o.shortmess .. "I" -- No Intro screen

vim.g.loaded_node_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_ruby_provider = 0

vim.o.foldmethod = "expr"
vim.o.foldexpr = "v:lua.vim.treesitter.foldexpr()"
vim.o.foldcolumn = "0"
vim.o.foldtext = ""
vim.o.foldlevel = 99
vim.o.foldlevelstart = 99

vim.o.shiftround = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.expandtab = true

vim.o.ignorecase = true
vim.o.smartcase = true
vim.o.inccommand = "split"

vim.o.formatoptions = "jcql"
vim.o.completeopt = "menuone,noselect,noinsert,fuzzy"

vim.o.splitbelow = true
vim.o.splitright = true
vim.o.splitkeep = "topline"

vim.o.clipboard = "unnamedplus"

vim.opt.wildoptions:append("fuzzy")

-- vim.diagnostic.config {
--   virtual_text = { prefix = "●" },
--   float = { border = "rounded" },
--   signs = true,
--   update_in_insert = false,
-- }

vim.o.updatetime = 300
vim.o.pumheight = 10
vim.o.pumblend = 10

local ok, ui2 = pcall(require, "vim._core.ui2")
if ok then ui2.enable { enable = true } end

vim.g.dotfiles_location = "~/dev/dotfiles"

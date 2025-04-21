--
--
--   ___            _                           ___      _
--  / _ \          (_)                         / _ \    | |
-- / /_\ \_ __ ___  _ _ __ _ __ ___ ______ _  / /_\ \___| | __
-- |  _  | '_ ` _ \| | '__| '__/ _ \_  / _` | |  _  / __| |/ /
-- | | | | | | | | | | |  | | |  __// / (_| | | | | \__ \   <
-- \_| |_/_| |_| |_|_|_|  |_|  \___/___\__,_| \_| |_/___/_|\_\
--
--
--
local g, o, keymap = vim.g, vim.o, vim.keymap.set

g.mapleader = " "
o.wrap = true
o.breakindent = true
o.signcolumn = "yes"
o.swapfile = false
o.backup = false
o.undofile = true
o.splitbelow = true
o.splitright = true
o.showmode = false
o.shiftwidth = 4
o.tabstop = 4
o.expandtab = true
o.timeoutlen = 300
o.updatetime = 250
o.clipboard = "unnamedplus"
o.ignorecase = true
o.smartcase = true
o.cursorline = true -- Highlight current line
o.guicursor = o.guicursor .. ",t:ver25"
o.laststatus = 3 -- Single Statusline for all windows
o.number = true -- Line numbers
o.winblend = 15 -- Floating Windows Transparency
o.termguicolors = true
o.winborder = "single"
o.inccommand = "split"
o.more = true
o.relativenumber = true
o.list = true
o.listchars = "tab:  ,trail:␣,eol:↲"

keymap("n", "Y", "^v$y", { desc = "Copy whole line" })
keymap("t", "<esc>", [[<C-\><C-n>]])
keymap("i", "<C-c>", "<esc>")
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")
keymap("n", "n", "nzz")
keymap("n", "N", "Nzz")
keymap("i", "jk", "<ESC>")
keymap("i", "kj", "<ESC>")
keymap("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
keymap("n", "j", "gj")
keymap("n", "k", "gk")
keymap("n", "{", "<cmd>cprev<CR>")
keymap("n", "}", "<cmd>cnext<CR>")
keymap("n", "<M-k>", ":bwipe!<CR>")

keymap("n", "<C-q>", function()
  local wins = vim.api.nvim_list_wins()
  for _, win in ipairs(wins) do
    local buf = vim.api.nvim_win_get_buf(win)
    if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "quickfix" then
      vim.cmd.cclose()
      return
    end
  end
  vim.cmd.copen()
end)
local terminal = require("terminal")

vim.keymap.set({ "n", "t" }, "<C-j>", function()
  terminal.toggle_floating({ height_ratio = 1, width_ratio = 0.95 })
end)

require("statusline") -- Loads lua/statusline/init.lua which is a simple script to create a beautiful statusline

local statusline = require("statusline")
local sections = statusline.sections
statusline.setup {
  sections.ModeSection,
  " ",
  sections.GitBranchSection,
  sections.SeperatorSection,
  sections.FileTypeIcon(),
  "  ",
  sections.FileSection { shorten_style = "elipsis" },
  sections.ModifiedSection,
  sections.SeperatorSection,
  sections.FileTypeSection,
  "[",
  sections.LineSection,
  ":",
  sections.ColumnSection,
  "]",
}

vim.lsp.enable({ "lua_ls", "gopls", "intelephense" })

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "[[", function()
      vim.diagnostic.jump({ count = -1 })
    end, { buffer = args.buf })
    keymap("n", "]]", function()
      vim.diagnostic.jump({ count = 1 })
    end, { buffer = args.buf })
    keymap("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gD", vim.lsp.buf.declaration, { buffer = args.buf })
    keymap("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
    keymap("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
    keymap("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
    keymap("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
    keymap("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
    keymap({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, { buffer = args.buf })
    keymap("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
    keymap("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
  end,
})

-- Initialize Lazy.nvim package manager
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

o.rtp = o.rtp .. "," .. lazypath

require("lazy").setup({ import = "plugins.spec" }, {
  change_detection = {
    notify = false,
  },
})

function Transparent()
  vim.cmd [[
        hi Normal guibg=none
        hi NormalFloat guibg=none
        hi LineNr guibg=none
        hi SignColumn guibg=none
        hi WinBorder guibg=none
    ]]
end

vim.cmd.colorscheme(vim.env.NVIM_COLORSCHEME or "gruvbuddy")

vim.api.nvim_create_user_command("Transparent", Transparent, {})

if vim.env.NVIM_TRANSPARENT then
  Transparent()
end

-- require("startscreen")

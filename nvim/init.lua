---@enum FuzzyFinders
local fuzzy_finders = {
  FZF_lua = 1,
  Snacks = 2,
}

-- local fuzzy_finder = fuzzy_finders.FZF_lua
local fuzzy_finder = fuzzy_finders.Snacks

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
local o = vim.o

o.rtp = o.rtp .. "," .. lazypath

require("lazy").setup({
  "amirrezaask/nvim-gruvbuddy.lua", -- Colorscheme, inspired by great @tjdevries.
  { "rose-pine/neovim", name = "rose-pine" }, -- Colorscheme.
  { "folke/tokyonight.nvim" }, -- Colorscheme.
  { "catppuccin/nvim", name = "catppuccin" }, -- Colorscheme.
  { "ellisonleao/gruvbox.nvim" },
  { "datsfilipe/vesper.nvim" },

  { "nvim-tree/nvim-web-devicons" }, -- Icons in terminal, nice.

  { "supermaven-inc/supermaven-nvim" }, -- AI Apocalypse
  { "MagicDuck/grug-far.nvim" }, -- Find/Replace project wide.
  { "stevearc/oil.nvim" }, -- File manager done right.
  { "williamboman/mason.nvim" }, -- Package manager for your system inside neovim.
  { "saghen/blink.cmp", tag = "v1.1.1" }, -- Blazingly fast autocomplete
  { "stevearc/conform.nvim" }, -- Autoformat/fixes
  { "folke/snacks.nvim", enabled = fuzzy_finder == fuzzy_finders.Snacks }, -- Plugin collection but i use the picker only.
  { "ibhagwan/fzf-lua", enabled = fuzzy_finder == fuzzy_finders.FZF_lua }, -- as a fallback for snacks picker.

  { "lewis6991/gitsigns.nvim" }, -- Git signs
  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
  },

  "folke/lazydev.nvim", -- Better neovim development support.
})

--- :h runtimepath
--- First this file $MYVIMRC is sourced.
--- Then the /plugin files are loaded.
--- Then the /after/plugin files are loaded.
--- On Opening a file with Filetype lua, the corresponding /after/ftplugin file is loaded.
--- In this file we just install our plugins, all plugins have a corresponding after/plugin/ file.

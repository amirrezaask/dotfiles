local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({
    "git",
    "clone",
    "--filter=blob:none",
    "https://github.com/folke/lazy.nvim.git",
    "--branch=stable", -- latest stable release
    lazypath,
  })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup {
	"wbthomason/packer.nvim",
	{ "rose-pine/neovim", name = "rose-pine" },
	{ "folke/tokyonight.nvim" },
	{ "ellisonleao/gruvbox.nvim" },
	{ "catppuccin/nvim", name = "catppuccin" },
	{ "goolord/alpha-nvim", dependencies = { "nvim-tree/nvim-web-devicons" } },
	{ "nvim-lualine/lualine.nvim", dependencies = { "nvim-tree/nvim-web-devicons" } },
	-- Treesitter syntax highlighting and text objects.
	{
		"nvim-treesitter/nvim-treesitter",
		dependencies = {
			"nvim-treesitter/nvim-treesitter-textobjects",
			"nvim-treesitter/playground",
			"nvim-treesitter/nvim-treesitter-context",
		},
	},
	-- Editor stuff
	"tpope/vim-surround", -- surrounding text objects
	"tpope/vim-abolish", -- useful text stuff
	"numToStr/Comment.nvim", -- Comment stuff like a boss
	"tpope/vim-fugitive",
	"fladson/vim-kitty", -- Support Kitty terminal config syntax
	"towolf/vim-helm", -- Support for helm template syntax
	"jansedivy/jai.vim", -- Jai from Jonathan Blow
	"tpope/vim-sleuth", -- Heuristically set buffer options
	"lewis6991/gitsigns.nvim",
	{
		"hrsh7th/nvim-cmp",
		dependencies = {
			"hrsh7th/cmp-nvim-lsp",
			"hrsh7th/cmp-vsnip",
			"hrsh7th/vim-vsnip",
			"hrsh7th/cmp-path",
			"hrsh7th/cmp-buffer",
		},
	},
	{
		"neovim/nvim-lspconfig",
		dependencies = { "williamboman/mason.nvim" },
	},
	{
		"mfussenegger/nvim-dap",
		dependencies = {
			"rcarriga/nvim-dap-ui",
			"leoluz/nvim-dap-go",
			"theHamsta/nvim-dap-virtual-text",
		},
	},
	"pbrisbin/vim-mkdir", -- Automatically create directory if not exists
	"tpope/vim-eunuch", -- Helper commands like :Rename, :Move, :Delete, :Remove, ...
	{
		"nvim-telescope/telescope.nvim",
		dependencies = {
			"nvim-lua/plenary.nvim",
			{ "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
			"nvim-telescope/telescope-ui-select.nvim",
		},
	},
	"fatih/vim-go",
}

function get_path_sep()
	if vim.fn.has("win32") == 1 then
		return "\\"
	else
		return "/"
	end
end

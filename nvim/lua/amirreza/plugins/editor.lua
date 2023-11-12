return {
	"tpope/vim-abolish",                               -- useful text stuff
	{ "numToStr/Comment.nvim", opts = {} },            -- Comment stuff like a boss
	"fladson/vim-kitty",                               -- Support Kitty terminal config syntax
	"towolf/vim-helm",                                 -- Support for helm template syntax
	"jansedivy/jai.vim",                               -- Jai from Jonathan Blow
	"tpope/vim-sleuth",
	{ 'kevinhwang91/nvim-bqf' },                       -- Peek location on quick fix list items
	{
		"lukas-reineke/indent-blankline.nvim",
		main = "ibl",
		opts = {
			scope = { enabled = false }
		}
	},
	{
		'stevearc/dressing.nvim',
		opts = {},
	}
}

return {
	"saghen/blink.cmp",
	version = "v1.6.0",
	opts = {
		sources = {
			default = { "lsp", "path", "buffer", "snippets" },
		},
		completion = { list = { selection = { preselect = false } } },
		keymap = {
			preset = "default",
			["<Tab>"] = { "accept", "fallback" },
			["<CR>"] = { "accept", "fallback" },
		},
	},
}


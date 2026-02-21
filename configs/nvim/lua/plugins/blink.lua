return {
	"saghen/blink.cmp",
	version = "v1.6.0",
	opts = {
		completion = { list = { selection = { preselect = false } } },
		keymap = {
			preset = "default",
			["<Tab>"] = { "accept", "fallback" },
			["<CR>"] = { "accept", "fallback" },
		},
	},
}
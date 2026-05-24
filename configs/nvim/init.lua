local start_time = vim.uv.hrtime()

-- Global configuration variables
vim.g.nvim_colorscheme = os.getenv("NVIM_THEME") or "catppuccin-macchiato"
vim.g.nvim_picker = "snacks"

require("amirreza.vim")

require("amirreza.colorscheme")
require("amirreza.sleuth")
require("amirreza.which-key")
require("amirreza.picker")
require("amirreza.statusline")
require("amirreza.git")
require("amirreza.lsp")
require("amirreza.outline")
require("amirreza.format")
require("amirreza.cloak")
require("amirreza.treesitter")
require("amirreza.lint")
require("amirreza.completion")
require("amirreza.highlight-colors")

local end_time = vim.uv.hrtime()
print(string.format("Neovim startup took %.2f ms", (end_time - start_time) / 1e6))

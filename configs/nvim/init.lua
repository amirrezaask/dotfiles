local start_time = vim.uv.hrtime()
vim.g.colorscheme = os.getenv("NVIM_THEME") or "everforest"

require("amirreza.neovim")
require("amirreza.colors")
require("amirreza.editor")
require("amirreza.blink")
require("amirreza.which-key")
require("amirreza.snacks")
require("amirreza.oil")
require("amirreza.lualine")
require("amirreza.git")
require("amirreza.lsp")
require("amirreza.conform")
require("amirreza.cloak")
require("amirreza.treesitter")
require("amirreza.lint")

print(string.format("Neovim startup took %.2f ms", (vim.uv.hrtime() - start_time) / 1e6))

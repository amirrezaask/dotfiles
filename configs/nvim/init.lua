local start_time = vim.uv.hrtime()
vim.g.colorscheme = os.getenv("NVIM_THEME") or "everforest"

pcall(require,"amirreza.neovim")
pcall(require,"amirreza.colors")
pcall(require,"amirreza.editor")
pcall(require,"amirreza.blink")
pcall(require,"amirreza.which-key")
pcall(require,"amirreza.snacks")
pcall(require,"amirreza.oil")
pcall(require,"amirreza.lualine")
pcall(require,"amirreza.git")
pcall(require,"amirreza.lsp")
pcall(require,"amirreza.conform")
pcall(require,"amirreza.cloak")
pcall(require,"amirreza.treesitter")
pcall(require,"amirreza.lint")

print(string.format("Neovim startup took %.2f ms", (vim.uv.hrtime() - start_time) / 1e6))

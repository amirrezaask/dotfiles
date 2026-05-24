local start_time = vim.uv.hrtime()
vim.g.nvim_colorscheme = os.getenv("NVIM_THEME") or "catppuccin-macchiato"
vim.g.nvim_picker = "snacks"

require("amirreza")

print(string.format("Neovim startup took %.2f ms", (vim.uv.hrtime() - start_time) / 1e6))

require("core.sets")
require("core.keymaps")
require("lazy_init")

vim.cmd.colorscheme(os.getenv("NVIM_COLORSCHEME") or "rose-pine-moon")
-- if true then
-- 	vim.cmd([[
--         hi! Normal guibg=none
--     ]])
-- end

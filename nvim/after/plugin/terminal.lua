vim.cmd(string.format([[ command! Term %s new | term]], math.ceil(vim.api.nvim_get_option "lines" * 0.3)))

function Transparent()
    vim.cmd [[
        hi Normal guibg=none
        hi NormalFloat guibg=none
        hi LineNr guibg=none
        hi SignColumn guibg=none
        hi WinBorder guibg=none
    ]]
end

vim.cmd.colorscheme(vim.env.NVIM_COLORSCHEME or "gruvbuddy")

vim.api.nvim_create_user_command("Transparent", Transparent, {})

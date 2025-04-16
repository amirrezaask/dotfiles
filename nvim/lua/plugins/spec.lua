local function config(name)
    return function()
        require("plugins." .. name)
    end
end

return {
    { 'rose-pine/neovim', name = 'rose-pine' },
    { 'catppuccin/nvim',  name = 'catppuccin' },
    'folke/tokyonight.nvim',

    {
        "saghen/blink.cmp",
        tag = "v1.1.1",
        config = config("blinkcmp")
    },

    {
        'stevearc/conform.nvim',
        config = config("conform"),
    },

    { "j-hui/fidget.nvim",     opts = {} },

    {
        "williamboman/mason.nvim",
        opts = {}
    },

    { 'kevinhwang91/nvim-bqf', opts = {} },

    {
        "ibhagwan/fzf-lua",
        enabled = vim.fn.executable('fzf') == 1,
        dependencies = {
            'nvim-tree/nvim-web-devicons',
            { "junegunn/fzf", build = "./install --all" }, -- This is not really a dependency, it just makes sure that fzf is laready installed into my system.
        },
        config = config("fzf"),
    },

    {
        "nvim-treesitter/nvim-treesitter",
        config = config("nvim-treesitter")
    },

    {
        "stevearc/oil.nvim",
        dependencies = { "nvim-tree/nvim-web-devicons" },
        config = config("oil"),
    },

    { 'lewis6991/gitsigns.nvim', config = config("gitsigns") },

    { "folke/which-key.nvim",    config = config('which-key') },

    { "folke/snacks.nvim",       config = config("snacks") },

    {

        "supermaven-inc/supermaven-nvim",
        opts = {},
    },
}

use {
  "folke/trouble.nvim",
  requires = "kyazdani42/nvim-web-devicons",
  config = function()
    require("trouble").setup {}
    require("core.keymaps").nnoremap("<leader>lt", "<cmd>TroubleToggle<CR>")
  end,
}

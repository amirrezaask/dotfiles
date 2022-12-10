if not has_plugins "nvim-tree" then
  return
end

require("nvim-tree").setup()

nnoremap("<leader>e", "<cmd>NvimTreeToggle<CR>")

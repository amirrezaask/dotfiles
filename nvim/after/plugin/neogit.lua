local ok, _ = pcall(require, "neogit")
if not ok then
  return
end

require("neogit").setup()

require("amirrezaask.keymaps").nnoremap("<leader>g", "<cmd>Neogit<cr>")

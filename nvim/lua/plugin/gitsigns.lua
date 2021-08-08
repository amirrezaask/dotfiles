local has_gitsigns, gitsigns = pcall(require, "gitsigns")
if not has_gitsigns then
  return
end

gitsigns.setup {
  signs = {
    add = { text = "|", numhl = "GitSignsAddNr" },
    change = { text = "|", numhl = "GitSignsChangeNr" },
    delete = { text = "_", numhl = "GitSignsDeleteNr" },
    topdelete = { text = "‾", numhl = "GitSignsDeleteNr" },
    changedelete = { text = "~-", numhl = "GitSignsChangeNr" },
  },
  numhl = false,
}

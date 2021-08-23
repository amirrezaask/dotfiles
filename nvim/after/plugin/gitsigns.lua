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
  current_line_blame = false,
  current_line_blame_opts = {
    delay = 800,
    virt_text_pos = "eol",
  },
}

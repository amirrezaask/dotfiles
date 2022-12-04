local ok, _ = pcall(require, "lspsaga")
if not ok then
  return
end

require("lspsaga").init_lsp_saga {
  code_action_lightbulb = {
    enable = false,
  },
  symbol_in_winbar = {
    enable = true,
  },
}

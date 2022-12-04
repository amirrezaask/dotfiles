local ok, rt = pcall(require, "rust-tools")
if ok then
  -- rt.setup {
  --   server = {
  --     on_attach = function(_, bufnr)
  --       lsp.on_attach(_, bufnr)
  --       require("amirrezaask.keymaps").bind {
  --         n = {
  --           C = { rt.hover_actions.hover_actions, desc = "Hover code actions", buffer = bufnr },
  --           ga = { rt.code_action_group.code_action_group, desc = "Code actions", buffer = bufnr },
  --         },
  --       }
  --     end,
  --   },
  -- }
end

vim.api.nvim_create_autocmd("BufWritePost", {
  pattern = "*.rs",
  callback = function(_)
    vim.lsp.buf.format()
  end,
})

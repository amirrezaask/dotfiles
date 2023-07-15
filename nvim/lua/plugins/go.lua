local function config()
  vim.g["go_gopls_enabled"] = 0
  vim.g["go_code_completion_enabled"] = 0
  vim.g["go_fmt_autosave"] = 0
  vim.g["go_imports_autosave"] = 0
  vim.g["go_mod_fmt_autosave"] = 0
  vim.g["go_doc_keywordprg_enabled"] = 0
  vim.g["go_def_mapping_enabled"] = 0
  vim.g["go_textobj_enabled"] = 0
  vim.g["go_list_type"] = "quickfix"
  vim.g["go_template_autocreate"] = false
  vim.api.nvim_create_autocmd("BufWritePre", {
    pattern = { "*.go" },
    callback = function()
      local params = vim.lsp.util.make_range_params()
      params.context = { only = { "source.organizeImports" } }
      local result = vim.lsp.buf_request_sync(0, "textDocument/codeAction", params, 2000)
      for _, res in pairs(result or {}) do
        for _, r in pairs(res.result or {}) do
          if r.edit then
            vim.lsp.util.apply_workspace_edit(r.edit, "utf-16")
          else
            vim.lsp.buf.execute_command(r.command)
          end
        end
      end

      vim.lsp.buf.format()
    end,
  })
  require("dap-go").setup {
    dap_configurations = {
      {
        type = "go",
        name = "Mabna: API",
        request = "launch",
        program = "./cmd/api",
      },
      {
        type = "go",
        name = "Mabna: Service",
        request = "launch",
        program = "./cmd/service",
      },
    },
  }
end

return {
  "fatih/vim-go",
}

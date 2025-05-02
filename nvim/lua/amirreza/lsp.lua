local keymap = vim.keymap.set

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "[[", function()
      vim.diagnostic.jump({ count = -1 })
    end, { buffer = args.buf })
    keymap("n", "]]", function()
      vim.diagnostic.jump({ count = 1 })
    end, { buffer = args.buf })
    keymap("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gd", vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gD", vim.lsp.buf.declaration, { buffer = args.buf })
    keymap("n", "gr", vim.lsp.buf.references, { buffer = args.buf })
    keymap("n", "gi", vim.lsp.buf.implementation, { buffer = args.buf })
    keymap("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
    keymap("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
    keymap("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
    keymap("n", "<leader>s", vim.lsp.buf.signature_help, { buffer = args.buf })
    keymap("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
    keymap("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
  end,
})

vim.lsp.enable({ "gopls", "intelephense", "lua_ls", "ocamllsp" })

vim.diagnostic.config({ virtual_text = true })

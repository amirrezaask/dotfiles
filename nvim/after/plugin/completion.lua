vim.opt.completeopt = { "menuone", "noselect" }

-- Don't show the dumb matching stuff.
vim.opt.shortmess:append "c"


local cmp = require "cmp"
cmp.setup {
  snippet = {
      expand = function(args)
        require('luasnip').lsp_expand(args.body)
      end,
    },
  documentation = {
    border = { "╭", "─", "╮", "│", "╯", "─", "╰", "│" },
  },
  -- You can set mapping if you want.
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Insert,
      select = false,
    },
    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },

  -- You should specify your *installed* sources.
  sources = {
    { name = "buffer" },
    { name = 'luasnip' },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}


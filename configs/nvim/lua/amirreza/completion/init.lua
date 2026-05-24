vim.pack.add {
  "https://github.com/saghen/blink.cmp",
}

require("blink.cmp").setup {
  cmdline = {
    enabled = true,
    keymap = {
      preset = "cmdline",
      ["<Right>"] = false,
      ["<Left>"] = false,
    },
    completion = {
      list = { selection = { preselect = false } },
      menu = {
        auto_show = function(_) return vim.fn.getcmdtype() == ":" end,
      },
      ghost_text = { enabled = true },
    },
  },
  keymap = {
    preset = "super-tab",
    ["<C-y>"] = { "select_and_accept" },
    ["<enter>"] = { "select_and_accept", "fallback" },
    ["<tab>"] = { "select_and_accept", "fallback" },
  },
  sources = {
    default = { "lsp", "path", "buffer" },
    providers = {
      lsp = {
        score_offset = 1000, -- Extreme priority to override fuzzy matching
      },
      path = {
        score_offset = 3, -- File paths moderate priority
      },
      buffer = {
        score_offset = -150, -- Lowest priority
        min_keyword_length = 3, -- Only show after 3 chars
      },
    },
  },
  completion = {
    accept = { auto_brackets = { enabled = true } },
    menu = {
      border = "rounded",
      max_height = 10,
      draw = {
        columns = {
          { "kind_icon" },
          { "label", "label_description", gap = 1 },
          { "source_name" },
        },
        components = {
          -- Native icon support (no lspkind needed)
          source_name = {
            text = function(ctx)
              local source_names = {
                lsp = "[LSP]",
                buffer = "[Buffer]",
                path = "[Path]",
                snippets = "[Snippet]",
              }
              return (source_names[ctx.source_name] or "[") .. ctx.source_name .. "]"
            end,
            highlight = "CmpItemMenu",
          },
        },
      },
      auto_show = true,
    },
    documentation = { auto_show = true, auto_show_delay_ms = 200 },
    ghost_text = { enabled = true },
  },
}

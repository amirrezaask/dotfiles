local function my_colors()
  local hl = function(name, opts)
    vim.api.nvim_set_hl(0, name, opts)
  end
  local colors = {
    blue1 = "#121b2b",
    blue2 = "#213554",
    blue3 = "#1d3872",
    blue4 = "#80b3d6",
    blue5 = "#3aa3e9",
    blue6 = "#56b6c2",
    blue7 = "#01bfef",
    white0 = "#f2f2f2",
    white1 = "#abb2bf",
    white2 = "#b6bdca",
    white3 = "#c8ccd4",
    red = "#f04c75",
    yellow1 = "#d19a66",
    yellow2 = "#e5c07b",
    green  = "#98c379",
    pink = "#c678dd",
    orange = "#be5046",
    grey = "#585858"

  }

  local theme = {
    normal = {
      bg = colors.blue1,
      fg = colors.white3,
    },
    overlay = {
      bg = colors.blue2,
    },
    linenr = {
      bg = colors.blue1,
      fg = colors.blue5
    },
    visual = {
      bg = colors.blue3,
      fg = colors.white3,
    },
    cursorline = {
      bg = colors.blue2,
      fg = colors.white,
    },
    pmenu = {
      bg = colors.blue2,
      fg = colors.white,
      sel = {
        bg = colors.blue1,
      },
    },
    tabline = {
      bg = colors.blue1,
      fg = colors.white,
      sel = {
        bg = colors.blue3,
      },
    },

    statusline = {
      bg = colors.blue3,
      fg = colors.white,
    },

    comment = {
      fg = colors.grey
    },

    string = {
      fg = colors.green
    },

    keyword = {
      fg = colors.red
    },

    identifier = {
      fg = colors.white0,
    },

    number = {
      fg = colors.red,
    },
    char = {
      fg = colors.lightpurple
    },
    type = {
      fg = colors.blue7,
    },
    conditional = {
      fg = colors.pink
    }

  }

  hl("Statusline", { bg = theme.statusline.bg, fg = theme.statusline.fg })
  hl("Tabline", { bg = theme.tabline.bg, fg = theme.tabline.white })
  hl("TablineFill", { bg = theme.tabline.bg })
  hl("TablineSel", { bg = theme.tabline.sel.bg })
  hl("Normal", { bg = theme.normal.bg, fg = theme.normal.fg })
  hl("Visual", { bg = theme.visual.bg, fg = theme.visual.fg })
  hl("Cursorline", { bg = theme.cursorline.bg })
  hl("NormalFloat", { bg = theme.overlay.bg } )
  hl("Pmenu", { bg = theme.pmenu.bg, fg = theme.pmenu.fg })
  hl("PmenuSel", { bg = theme.pmenu.sel.bg, fg = theme.pmenu.fg })
  hl("SignColumn", { bg = theme.normal.bg, fg = theme.statusline.fg })
  hl("LineNr", { bg = theme.linenr.bg, fg = theme.linenr.fg })
  -- Syntax
  hl("Comment", { fg = theme.comment.fg })
  hl("String", { fg = theme.string.fg })
  hl("Keyword" , { fg = theme.keyword.fg })
  hl("Keywords",  { fg = theme.keyword.fg })
  hl("Identifier" , { fg = theme.identifier.fg })
  hl("Number", { fg = theme.number.fg })
  hl("Character", { fg = theme.char.fg })
  hl("Boolean", { fg = theme.keyword.fg })
  hl("Type", { fg = theme.type.fg })
  hl("Conditional", {fg = theme.conditional.fg})
  -- Fugitive
  hl('diffAdd', { bg = theme.statusline.bg, fg = theme.string.fg } )
  hl('diffChange', { bg = theme.statusline.bg, fg = theme.conditional.fg } )
  hl('diffRemove', { bg = theme.statusline.bg, fg = theme.keyword.fg } )
  hl("diffAdded", {link = "String"})
  hl("diffRemoved", { link = "Keyword"})
end
my_colors()

return {
  { 'navarasu/onedark.nvim', opts = { style = "darker" }},
  { 'marko-cerovac/material.nvim' },
  { 'rose-pine/neovim', name = "rose-pine" },
  { 'catppuccin/nvim', name = 'catppuccin' },
  { 'amirrezaask/themes', name = "amirreza-themes" }
}

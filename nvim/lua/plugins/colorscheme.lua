local function my_colors()
  local hl = function(name, opts)
    vim.api.nvim_set_hl(0, name, opts)
  end
  local colors = {
    orange = "#FF7509",
    black = "#000000",
    grey = "#585858",
    lightblack = "#001648",
    lightblue = "#137BD5",
    lightpurple = "#D24DD7",
    darkpurple = "#180075",
    darkblue = "#002375",
    darkblue2 = "#004CFF",
    darkblue3 = "#000C27",
    darkblue4 = "#00143F",
    white = "#E0E0E0",
    yellow = "#FFFB00",
    red = "#FE4C3E",
    cyan = "#32DFE8"

  }

  local theme = {
    normal = {
      bg = colors.darkblue3,
      fg = colors.white,
      float = {
        bg = colors.darkblue4,
      }
    },
    visual = {
      bg = colors.darkblue,
      fg = colors.white,
    },
    cursorline = {
      bg = colors.lightblack,
      fg = colors.white,
    },
    pmenu = {
      bg = colors.darkblue,
      fg = colors.white,
      sel = {
        bg = colors.darkblue2,
      },
    },
    tabline = {
      bg = colors.darkblue,
      fg = colors.white,
      sel = {
        bg = colors.darkblue2,
      },
    },

    statusline = {
      bg = colors.darkblue,
      fg = colors.white,
    },

    comment = {
      fg = colors.grey
    },

    string = {
      fg = colors.lightblue
    },

    keyword = {
      fg = colors.orange
    },
    identifier = {
      fg = colors.white,
    },
    number = {
      fg = colors.red,
    },
    char = {
      fg = colors.lightpurple
    },
    type = {
      fg = colors.cyan,
    }

  }

  hl("Statusline", { bg = theme.statusline.bg, fg = theme.statusline.fg })
  hl("Tabline", { bg = theme.tabline.bg, fg = theme.tabline.white })
  hl("TablineFill", { bg = theme.tabline.bg })
  hl("TablineSel", { bg = theme.tabline.sel.bg })
  hl("Normal", { bg = theme.normal.bg, fg = theme.normal.fg })
  hl("Visual", { bg = theme.visual.bg, fg = theme.visual.fg })
  hl("Cursorline", { bg = theme.cursorline.bg })
  hl("NormalFloat", { bg = theme.normal.float.bg } )
  hl("Pmenu", { bg = theme.pmenu.bg, fg = theme.pmenu.fg })
  hl("PmenuSel", { bg = theme.pmenu.sel.bg, fg = theme.pmenu.fg })
  hl("SignColumn", { bg = theme.statusline.bg, fg = theme.statusline.fg })
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
end
my_colors()

return {}

local has_nline, nline = pcall(require, "nline")
local has_lualine, lualine = pcall(require, "lualine")

if has_nline then
  local vim = require "nline.parts.vim"
  local git = require "nline.parts.git"
  local lsp = require "nline.parts.lsp"
  local icons = require "nline.parts.icons"
  local wrappers = require "nline.wrappers"

  -- Took after TJDevries configuration of express_line.nvim
  local tj = {
    vim.mode {
      texts = {
        normal = "Normal",
        visual = "Visual",
        visual_block = "VisualBlock",
        insert = "Insert",
        insert_complete = "IComplete",
        command = "Command",
        terminal = "Terminal",
      },
    },
    vim.space(),
    icons.git_branch,
    vim.space(),
    git.branch(),

    vim.seperator(),

    icons.file,
    vim.space(),
    vim.filename { shorten = true },
    vim.modified(),

    vim.seperator(),

    -- wrappers.square_brackets(lsp.current_function()),
    wrappers.square_brackets(git.changes()),
    wrappers.square_brackets(lsp.diagnostics()),
    -- lsp.progress(),
    wrappers.square_brackets(vim.line() .. vim.space() .. vim.colon() .. vim.col()),
    vim.filetype(),
  }

  local simple = {
    vim.modified,
    vim.filename { shorten = false },
    vim.space(),
    vim.filetype(),
    vim.space(),
    vim.seperator(),
    wrappers.square_brackets(git.branch()),
    vim.space(),
    wrappers.square_brackets(git.changes()),
  }
  -- nline.make(tj)
  nline.make(simple)
end

if has_lualine then
  lualine.setup {
    options = {
      icons_enabled = true,
      theme = "onedark",
      component_separators = { "", "" },
      section_separators = { "", "" },
      disabled_filetypes = {},
    },
    sections = {
      lualine_a = { "mode" },
      lualine_b = { "branch" },
      lualine_c = { "filename" },
      lualine_x = { "encoding", "fileformat", "filetype" },
      lualine_y = { "progress" },
      lualine_z = { "location" },
    },
    inactive_sections = {
      lualine_a = {},
      lualine_b = {},
      lualine_c = { "filename" },
      lualine_x = { "location" },
      lualine_y = {},
      lualine_z = {},
    },
    tabline = {},
    extensions = {},
  }
end

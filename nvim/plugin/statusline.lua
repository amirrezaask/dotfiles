local o = vim.o
_G._filetype_icon = function()
  local filetype = vim.bo.filetype or "Unknown"
  local icon
  pcall(function()
    icon = require("nvim-web-devicons").get_icon(filetype)
  end)
  return icon or ""
end

function _G._statusline_mode()
  local mode = vim.api.nvim_get_mode().mode
  local mode_map = {
    ["n"] = "Normal",
    ["i"] = "Insert",
    ["v"] = "Visual",
    ["V"] = "Visual Line",
    ["\22"] = "Visual Block", -- \22 is Ctrl-V
    ["c"] = "Command",
    ["R"] = "Replace",
    ["s"] = "Select",
    ["S"] = "Select Line",
    ["\19"] = "Select Block", -- \19 is Ctrl-S
    ["t"] = "Terminal",
    ["no"] = "Operator Pending",
    ["niI"] = "Normal (Insert)",
    ["niR"] = "Normal (Replace)",
    ["niV"] = "Normal (Virtual Replace)",
    ["nt"] = "Normal (Terminal)",
    ["rm"] = "More Prompt",
    ["r?"] = "Confirm",
    ["!"] = "Shell",
  }

  return mode_map[mode] or "Unknown"
end
local space = " "
local bracket = function(s)
  return "[" .. s .. "]"
end
local gitsigns_status = "[%{get(b:,'gitsigns_status','')}]"
-- local branch_icon = ""
local gitsigns_head = "%{get(b:,'gitsigns_head','')}"
local mode = "[%{v:lua._statusline_mode()}]"
-- local filetype_icon = "%{v:lua._filetype_icon()}"
local filetype = "%y"
local file_path = "%F"
local line = "%l"
local column = "%c"
local line_col = line .. ":" .. column
local modified = "%m"

local sections = {
  mode .. space .. gitsigns_head .. space .. gitsigns_status, -- Left
  file_path .. modified, -- Center
  bracket(line_col) .. filetype, -- Right
}

o.statusline = table.concat(sections, "%=")

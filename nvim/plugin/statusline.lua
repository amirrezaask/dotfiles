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

_G.statusline_git_status = function()
  if not vim.b.gitsigns_status or vim.b.gitsigns_status == "" then
    return ""
  else
    return "[" .. vim.b.gitsigns_status .. "]"
  end
end

_G.statusline_git_head = function()
  local branch_icon = ""
  if not vim.b.gitsigns_head or vim.b.gitsigns_head == "" then
    return ""
  else
    return branch_icon .. " " .. vim.b.gitsigns_head
  end
end
local git_head = "%{v:lua.statusline_git_head()}"
local git_status = "%{v:lua.statusline_git_status()}"
local mode = "[%{v:lua._statusline_mode()}]"
local filetype_icon = "%{v:lua._filetype_icon()}"
local filetype = "%y"
local filename = "%r%h%w%q%F"
local line = "%l"
local column = "%c"
local line_col = line .. " :" .. column
local modified = "%m"

local sections = {
  mode .. space .. git_head .. space .. git_status, -- Left
  filetype_icon .. "  " .. filename .. modified, -- Center
  bracket(line_col) .. filetype, -- Right
}

o.statusline = table.concat(sections, "%=")

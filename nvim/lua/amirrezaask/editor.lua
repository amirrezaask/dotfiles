-- Color Picker
function ColorPicker()
  _PICKER_ASHKAN_KIANI_COPYRIGHT_2020_LONG_NAME_HERE_ = nil
  require("colorizer").color_picker_on_cursor()
end
vim.cmd [[ autocmd BufEnter * ColorizerAttachToBuffer ]]
vim.cmd [[ command! ColorPicker lua ColorPicker ]] -- On an color code run ColorPicker command
-- Indent lines
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { "help", "packer" }
vim.g.indent_blankline_buftype_exclude = { "terminal", "nofile" }
vim.g.indent_blankline_show_trailing_blankline_indent = false
vim.g.indent_blankline_show_current_context = true

-- highlight on yank
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]]

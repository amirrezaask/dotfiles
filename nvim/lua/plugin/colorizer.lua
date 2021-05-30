local has_colorizer, colorizer = pcall(require, 'colorizer')
if not has_colorizer then return end

function ColorPicker()
  _PICKER_ASHKAN_KIANI_COPYRIGHT_2020_LONG_NAME_HERE_ = nil
  colorizer.color_picker_on_cursor()
end

vim.autocmd {
  "BufEnter",
  "*",
  "ColorizerAttachToBuffer"
}

vim.command('ColorPicker', ColorPicker)

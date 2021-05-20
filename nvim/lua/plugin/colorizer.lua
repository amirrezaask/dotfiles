local colorizer = require('colorizer')

function ColorPicker()
  _PICKER_ASHKAN_KIANI_COPYRIGHT_2020_LONG_NAME_HERE_ = nil
  colorizer.color_picker_on_cursor()
end

nvim_autocmd {
  "BufEnter",
  "*",
  "ColorizerAttachToBuffer"
}

nvim_command('ColorPicker', ColorPicker)

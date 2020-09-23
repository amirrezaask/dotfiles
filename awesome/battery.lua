local wibox = require("wibox")
local watch = require("awful.widget.watch")

battery_widget = wibox.widget.textbox()
battery_widget:set_font('monospace 9')

function string:split(delimiter)
  local result = { }
  local from  = 1
  local delim_from, delim_to = string.find( self, delimiter, from  )
  while delim_from do
    table.insert( result, string.sub( self, from , delim_from-1 ) )
    from  = delim_to + 1
    delim_from, delim_to = string.find( self, delimiter, from  )
  end
  table.insert( result, string.sub( self, from  ) )
  return result
end
watch(
    "acpi", 10,
    function(widget, stdout, stderr, exitreason, exitcode)
        battery_widget:set_text(stdout)
    end
)

return {
    widget = battery_widget
}

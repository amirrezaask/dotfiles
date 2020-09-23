local wibox = require("wibox")
local watch = require("awful.widget.watch")

battery_widget = wibox.widget.textbox()
battery_widget:set_font('monospace 9')

watch(
    "acpi", 10,
    function(widget, stdout, stderr, exitreason, exitcode)
        battery_widget:set_text(stdout)
    end
)

return {
    widget = battery_widget
}

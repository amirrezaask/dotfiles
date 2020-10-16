local wibox = require("wibox")
local watch = require("awful.widget.watch")

local widget = watch(
    "acpi", 10,
    function(widget, stdout, stderr, exitreason, exitcode)
        widget:set_text(stdout)
    end
)

return {
    widget = widget 
}

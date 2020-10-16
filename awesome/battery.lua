local watch = require("awful.widget.watch")

local widget = watch(
    "acpi", 10,
    function(widget, stdout, _, _, _)
        widget:set_text(stdout)
    end
)

return {
    widget = widget 
}

local wibox = require("wibox")
local watch = require("awful.widget.watch")
local awful = require('awful')

rhythmbox_widget = wibox.widget.textbox()
rhythmbox_widget:set_font('monospace 9')

local press_handler = function(_, _, _,button)
    if button == 1 then
        awful.spawn("rhythmbox-client --no-start --play-pause")
    elseif button == 2 then
        awful.spawn("rhythmbox-client --no-start --previous")
    elseif button == 3 then
        awful.spawn("rhythmbox-client --no-start --next")
    end
end

rhythmbox_widget:connect_signal("button::press", press_handler)

watch(
    "rhythmbox-client --no-start --print-playing-format %tt", 1,
    function(widget, stdout, stderr, exitreason, exitcode)
        rhythmbox_widget:set_text(stdout)
    end
)

return {
    widget = rhythmbox_widget
}

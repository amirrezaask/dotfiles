local watch = require("awful.widget.watch")
local awful = require('awful')

local press_handler = function(_, _, _,button)
    if button == 1 then
        awful.spawn("rhythmbox-client --no-start --play-pause")
    elseif button == 2 then
        awful.spawn("rhythmbox-client --no-start --previous")
    elseif button == 3 then
        awful.spawn("rhythmbox-client --no-start --next")
    end
end


local widget = watch(
    "rhythmbox-client --no-start --print-playing-format %tt", 1,
    function(widget, stdout, _, _, _)
        widget:set_text(stdout)
    end
)

widget:connect_signal("button::press", press_handler)

return {
    widget = widget 
}

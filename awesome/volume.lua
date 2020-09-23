local wibox = require("wibox")
local watch = require("awful.widget.watch")
local awful = require('awful')

local GET_VOLUME_CMD = 'amixer -D pulse sget Master'
local INC_VOLUME_CMD = 'amixer -D pulse sset Master 5%+'
local DEC_VOLUME_CMD = 'amixer -D pulse sset Master 5%-'
local TOG_VOLUME_CMD = 'amixer -D pulse sset Master toggle'


volume_widget = wibox.widget.textbox()
volume_widget:set_font('monospace 9')

local volume_callback = function(widget, stdout, _, _, _)
    local mute = string.match(stdout, "%[(o%D%D?)%]")
    local volume = string.match(stdout, "(%d?%d?%d)%%")
    volume = tonumber(string.format("% 3d", volume))
    widget:set_text(string.format("%s %s", volume, mute))
end

local press_handler = function(_, _, _, button)
    if button == 1 then
        awful.spawn(TOG_VOLUME_CMD)
    elseif button == 4 then
        awful.spawn(INC_VOLUME_CMD)
    elseif button == 5 then
        awful.spawn(DEC_VOLUME_CMD)
    end
end

volume_widget:connect_signal("button::press", press_handler)

watch(GET_VOLUME_CMD, 0.5, volume_callback, volume_widget)

return {
    widget = volume_widget 
}

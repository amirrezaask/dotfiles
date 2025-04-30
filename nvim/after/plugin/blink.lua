if Missing("blink") then
  return
end

require("blink").setup {
  keymap = { preset = "enter" },
  cmdline = { enabled = false },
}

if Missing("blink.cmp") then
  return
end

print("setting up blink")

require("blink.cmp").setup {
  keymap = { preset = "enter" },
  cmdline = { enabled = false },
}

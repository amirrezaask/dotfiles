if Missing("blink.cmp") then
  return
end

require("blink.cmp").setup {
  keymap = { preset = "enter" },
  cmdline = { enabled = false },
}

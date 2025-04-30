if Missing("lazydev") then
  return
end

---@diagnostic disable-next-line: missing-fields
require("lazydev").setup {
  library = {},
}

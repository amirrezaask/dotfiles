local ok, _ = pcall(require, "dressing")
if not ok then
  return
end

require("dressing").setup {
  input = {
    enabled = true,
  },
  select = {
    enabled = true,
  },
}

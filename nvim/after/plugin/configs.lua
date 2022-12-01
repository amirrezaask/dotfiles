for _, cfg in pairs(configs) do
  cfg()
end

local ok, _ = pcall(require, "which-key")
if ok then
  require("which-key").setup {}
end

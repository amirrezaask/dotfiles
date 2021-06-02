local has_blame, blame = pcall(require,'blame')
if not has_blame then
  return
end

blame.setup {
  always = false,
}

vim.nmap {
  ['-'] = require('blame').toggle,
}

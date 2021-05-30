local has_blame, blame = pcall(require,'blame')
if not has_blame then
  return
end

blame.setup {
  always = false,
  prefix = ''
}

vim.nmap {
  [',b'] = require('blame').blame,
  [',c'] = require('blame').clear
}

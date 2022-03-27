if true then
  return
end
vim.g.fzf_layout = { ['down'] = '30%' }
vim.g.fzf_preview_window = {}
vim.cmd [[
  nnoremap <leader><leader> <cmd>Files<CR>
  nnoremap <leader>fp <cmd>Files ~/.local/share/nvim/site/pack/packer<CR>
  nnoremap <leader>ps <cmd>Files ~/src/gitlab.snapp.ir<CR>
  nnoremap <leader>en <cmd>Files ~/.config/nvim<CR>
  nnoremap ?? <cmd>Rg<CR>
]]




command! FRun lua require'amirrezaask.floating':command()
command! VRun lua require'amirrezaask.floating':vnew()
nnoremap <leader>r <cmd>FRun<CR>

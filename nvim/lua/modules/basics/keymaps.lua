require"core.keymaps".bind {
    n = {
        ["Q"] = '<NOP>',
        [";,"] = ':',
        ['q;'] = 'q:',
        ['{'] = ':cprev<CR>',
        ['}'] = ':cnext<CR>',
        ["<C-h>"] = ":tabprev<CR>",
        ["<C-l>"] = ":tabnext<CR>",
        ["<C-n>"] = ":tabnew<CR>",
        ['<Left>'] = ':vertical resize -5<CR>',
        ['<Right>'] = ':vertical resize +5<CR>',
        ['<Up>'] = ':resize +5<CR>',
        ['<Down>'] = ':resize -5<CR>',
        ['Y'] = 'y$',
        ['n'] = 'nzz',
        ['N'] = 'Nzz',
        ['<M-p>'] = ':bprev<CR>',
        ['<M-n>'] = ':bnext<CR>',
        ['<M-j>'] = ':m .+1<CR>==',
        ['<M-k>'] = ':m .-2<CR>==',
        ['j'] = 'gj',
        ['k'] = 'gk',
    },
    t = {
        ['<Esc>'] = '<C-\\><C-n>',
        ['jk'] = '<C-\\><C-n>',
        ['kj'] = '<C-\\><C-n>',
    },

    i = {
        ['jk'] = '<esc>',
        ['kj'] = '<esc>',
    },
    v = {
        ['<M-j>'] = '<Esc>:m .+1<CR>==gi',
        -- ['<M-k>'] = '<Esc>:m .-2<CR>==gi',
        -- ['<M-k>'] = ':m >+1<CR>gv=gv',
        -- ['<M-k>'] = '<Esc>:m .-2<CR>==gi'
    }
}

vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]]

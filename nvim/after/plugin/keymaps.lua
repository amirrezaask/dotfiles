require("keymaps").bind {
  n = {
    ["Q"] = "<NOP>",
    [";,"] = ":",
    ["q;"] = "q:",
    ["{"] = ":cprev<CR>",
    ["}"] = ":cnext<CR>",
    ["<C-h>"] = ":tabprev<CR>",
    ["<C-l>"] = ":tabnext<CR>",
    ["Y"] = "y$",
    ["n"] = "nzz",
    ["N"] = "Nzz",
    ["<M-p>"] = ":bprev<CR>",
    ["<M-n>"] = ":bnext<CR>",
    ["<M-j>"] = ":m .+1<CR>==",
    ["<M-k>"] = ":m .-2<CR>==",
    ["j"] = "gj",
    ["k"] = "gk",
  },
  t = {
    ["<Esc>"] = "<C-\\><C-n>",
    ["jk"] = "<C-\\><C-n>",
    ["kj"] = "<C-\\><C-n>",
  },

  i = {
    ["jk"] = "<esc>",
    ["kj"] = "<esc>",
  },
}

vim.cmd [[ nnoremap <expr><CR> {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]]

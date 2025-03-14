vim.g.mapleader = " " -- <leader> key for keymaps mapped to <Space>
vim.keymap.set("n", "Y", "y$", { desc = "Copy whole line" })
vim.keymap.set("n", "<Esc>", "<cmd>nohlsearch<CR>")
vim.keymap.set("i", "<C-c>", "<esc>")
vim.keymap.set("i", "jk", "<ESC>")
vim.keymap.set("i", "kj", "<ESC>")
vim.keymap.set("n", "{", "<cmd>cprev<CR>") -- Quick fix list
vim.keymap.set("n", "}", "<cmd>cnext<CR>") -- Quickfix list
vim.keymap.set("i", "<C-Space>", "<C-x><C-o>")
vim.keymap.set("n", "<C-d>", "<C-d>zz")
vim.keymap.set("n", "<C-u>", "<C-u>zz")
vim.keymap.set("n", "<C-o>", "<C-o>zz")
vim.keymap.set("n", "<C-i>", "<C-i>zz")
vim.keymap.set("n", "n", "nzz")
vim.keymap.set("n", "N", "Nzz")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "<leader>i", "<cmd>edit $MYVIMRC<CR>")
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("n", "<M-Left>", "<c-w>5>")
vim.keymap.set("n", "<M-Right>", "<c-w>5<")
vim.keymap.set("n", "<M-Up>", "<C-W>+")
vim.keymap.set("n", "<M-Down>", "<C-W>-")
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", function() vim.cmd([[ wincmd w ]]) end)
vim.keymap.set("n", "<leader>l", vim.diagnostic.open_float, { desc = "Diagnostics: Open float window" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Set Local list" })
vim.keymap.set("n", "<leader>g", "<cmd>LazyGit<CR>", { desc = "Lazy Git" })
vim.keymap.set({ "n", "i", "t" }, "<C-h>", "<cmd>tabprev<CR>", {})
vim.keymap.set({ "n", "i", "t" }, "<C-l>", function()
    -- Get current tab number and total number of tabs
    local current_tab = vim.fn.tabpagenr()
    local total_tabs = vim.fn.tabpagenr('$')

    -- If current tab is the last one, create a new tab
    if current_tab == total_tabs then
        vim.cmd('tabnew')
    else
        -- Otherwise, go to next tab
        vim.cmd('tabnext')
    end
end, {})

vim.cmd([[ command! W :w ]])

local qflist = false
vim.keymap.set("n", "<C-q>", function()
    if qflist == true then
        qflist = not qflist
        vim.cmd([[ cclose ]])
    else
        qflist = not qflist
        vim.cmd([[ copen ]])
    end
end, { desc = "Open Quickfix list" })

vim.keymap.set("n", "<c-j>", function()
    vim.cmd.new()
    vim.cmd.wincmd "J"
    vim.api.nvim_win_set_height(0, 12)
    vim.wo.winfixheight = true
    vim.cmd [[ startinsert! ]]
    vim.cmd.term()
    vim.keymap.set({ "n", "i", "t" }, "<C-j>", "<cmd>quit<CR>", { buffer = true })
end)

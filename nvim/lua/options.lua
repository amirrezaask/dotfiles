vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true -- Relative line numbers
vim.opt.errorbells = false
vim.opt.smartindent = true
vim.opt.wrap = true
vim.opt.swapfile = false
vim.opt.backup = false
vim.opt.undofile = true
vim.opt.hlsearch = false
vim.opt.incsearch = true
vim.opt.termguicolors = true
vim.opt.signcolumn = "yes"
vim.opt.isfname:append "@-@"
vim.opt.updatetime = 50
vim.opt.shortmess:append "c" -- Don't pass messages to |ins-completion-menu|.
vim.opt.shortmess:append "I" -- No Intro message
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.cursorline = false
vim.opt.sw = 4
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.laststatus = 2
vim.opt.timeoutlen = 300
vim.opt.laststatus = 3

function Statusline() -- since this function is called from vimscript world it's simpler if it's global
    local branch = ""
    if vim.b.gitsigns_head then
        local signs = ""
        if vim.b.gitsigns_status and vim.b.gitsigns_status ~= "" then
            signs = " " .. vim.b.gitsigns_status
        end
        branch = string.format("[%s%s]", vim.b.gitsigns_head, signs)
    end
    return branch .. "%=[%m%r%h%w%q%F]%=[L:%l C:%c]"
end

vim.opt.statusline = "%!v:lua.Statusline()"

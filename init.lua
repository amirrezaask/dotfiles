--    ___         _                      ___       __
--   / _ | __ _  (_)__________ ___ ___ _/ _ | ___ / /__
--  / __ |/  ' \/ / __/ __/ -_)_ // _ `/ __ |(_-</  '_/
-- /_/ |_/_/_/_/_/_/ /_/  \__//__/\_,_/_/ |_/___/_/\_\
-- Minimal, fast configuration for neovim.

-- Plugins Installer
local plugins_path = vim.fn.stdpath("data") .. "/plugins"
plugins_report = {}
local installed_count = 0
local function install_plugins(plugins)
    for i, plugin in ipairs(plugins) do
        local spec = {}
        if type(plugin) == "table" then
            local segs = vim.split(plugin[1], "/")
            local name = segs[#segs]

            if plugin.as ~= nil then
                name = plugin.as
            end
            spec.name = name
            spec.repo = plugin[1]
        elseif type(plugin) == "string" then
            local segs = vim.split(plugin, "/")
            local name = segs[#segs]
            spec.name = name
            spec.repo = plugin
        else
            vim.fn.error("unsupported type for plugin")
        end

        if string.find(spec.name, ".nvim") then
            spec.name = string.sub(spec.name, 1, -6)
        end

        plugins[i] = {
            name = spec.name,
            repo = spec.repo,
            rtp = plugins_path .. "/" .. spec.name,
        }

        if not vim.loop.fs_stat(plugins[i].rtp) then
            local stdout = vim.uv.new_pipe()
            local stderr = vim.uv.new_pipe()
            vim.uv.spawn("git", {
                args = { "clone", "https://github.com/" .. spec.repo, spec.rtp },
                stdio = { nil, stdout, stderr },
            }, function(code, _)
                plugins[i].installed = code == 0
                if plugins[i].installed then
                    installed_count = installed_count + 1
                    print("Plugin " .. plugins[i].name .. " installed.")
                    if installed_count == #plugins then
                        print(string.format("[%d/%d] plugins installed", installed_count, #plugins))
                    end
                end
            end)
            print("Installing " .. spec.name .. " !")
            plugins_report[spec.repo] = { stdout = "", stderr = "" }


            vim.uv.read_start(stdout, function(_, data)
                if data then
                    plugins_report[spec.repo].stdout = plugins_report[spec.repo].stdout .. data
                end
            end)
            vim.uv.read_start(stderr, function(_, data)
                if data then
                    plugins_report[spec.repo].stderr = plugins_report[spec.repo].stderr .. data
                end
            end)
        else
            plugins[i].installed = true
            installed_count = installed_count + 1
            if installed_count == #plugins then
                print(string.format("[%d/%d] plugins installed", installed_count, #plugins))
            end
        end
    end


    for _, plugin in ipairs(plugins) do
        if plugin.installed then
            vim.opt.rtp:prepend(plugin.rtp)
        end
    end
end

install_plugins({
    -- UI
    'nvim-tree/nvim-web-devicons', -- Nice icons
    "catppuccin/nvim",
    "folke/tokyonight.nvim",
    'navarasu/onedark.nvim',
    'nvim-lualine/lualine.nvim',
    'stevearc/oil.nvim',
    "folke/ts-comments.nvim",
    "nvim-pack/nvim-spectre",
    "nvim-treesitter/nvim-treesitter", -- Treesitter
    -- Telescope
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "nvim-telescope/telescope-fzf-native.nvim",
    "nvim-telescope/telescope-ui-select.nvim",
    -- LSP
    "neovim/nvim-lspconfig",
    "folke/trouble.nvim",
    "williamboman/mason.nvim",
    "williamboman/mason-lspconfig.nvim",
})

vim.opt.wrap = true        -- Wrap long lines
vim.opt.breakindent = true -- Wrapped lines have same indentation as the actual line.
vim.opt.swapfile = false   -- No annoying swapfiles
vim.opt.backup = false     -- Disable Vim backups, we have Git :)
vim.opt.undofile = true    -- Save undo history
vim.opt.hlsearch = false   -- Highlight all matches of a search pattern.
vim.opt.incsearch = true   -- Match pattern while typing.
vim.opt.signcolumn = "yes" -- Keep signcolumn always visible
vim.opt.splitbelow = true  -- How new splits are created
vim.opt.splitright = true
vim.opt.sw = 4             -- TABs and indentation
vim.opt.ts = 4
vim.opt.expandtab = true
vim.g.netrw_browse_split = 0 -- minimal netrw (vim default file manager)
vim.g.netrw_banner = 0
vim.g.netrw_winsize = 25
vim.opt.timeoutlen = 300 -- vim update time
vim.opt.updatetime = 250
vim.opt.termsync = false
vim.opt.number = true -- Line numbers
vim.opt.relativenumber = true
vim.opt.mouse = "a"
vim.opt.clipboard = "unnamedplus" -- Clipboard
vim.opt.ignorecase = true         -- Case-insensitive searching UNLESS \C or capital in search
vim.opt.smartcase = true
vim.opt.completeopt = { 'menu', 'noinsert' }
vim.opt.inccommand = "split" -- Preview all substitutions(replacements).
vim.opt.scrolloff = 10       -- Minimal number of screen lines to keep above and below the cursor.
vim.opt.cursorline = true
vim.opt.laststatus = 3       -- Global statusline
IS_WINDOWS = vim.fn.has("win32") == 1
TRANSPARENT = false
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
vim.keymap.set("n", "<leader>v", "<cmd>vs<CR>")
vim.keymap.set("n", "<leader>s", "<cmd>sp<CR>")
vim.keymap.set("n", "Q", "<cmd>q<CR>")
vim.keymap.set("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
vim.keymap.set("n", "<leader>i", "<cmd>edit $MYVIMRC<CR>")
vim.keymap.set("n", "j", "gj")
vim.keymap.set("n", "k", "gk")
vim.keymap.set("t", "<esc>", [[<C-\><C-n>]])
vim.keymap.set("t", "<C-w><C-w>", function() vim.cmd([[ wincmd w ]]) end)
vim.keymap.set({ "i" }, "<C-a>", "<C-x><C-o>") -- simpler omnifunc completion
vim.keymap.set("n", "<leader>l", vim.diagnostic.open_float, { desc = "Diagnostics: Open float window" })
vim.keymap.set("n", "[[", vim.diagnostic.goto_prev, { desc = "Diagnostics: Next" })
vim.keymap.set("n", "]]", vim.diagnostic.goto_next, { desc = "Diagnostics: Previous" })
vim.keymap.set("n", "<leader>q", vim.diagnostic.setloclist, { desc = "Set Local list" })
vim.cmd([[ command! W :w ]])

-- Quickfix list
local qflist = false
function ToggleQFList()
    if qflist == true then
        qflist = not qflist
        vim.cmd([[ cclose ]])
    else
        qflist = not qflist
        vim.cmd([[ copen ]])
    end
end

vim.keymap.set("n", "<C-q>", ToggleQFList, { desc = "Open Quickfix list" })

-- Highlight on Yank
vim.api.nvim_create_autocmd("TextYankPost", {
    group = vim.api.nvim_create_augroup("YankHighlight", { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

-- Neovide
local font = 'Monaspace Neon'
local font_size = 17
vim.o.guifont = string.format('%s:h%d', font, font_size)

function SetFont()
    local fontfamily = ""
    local fontsize = ''

    vim.ui.input({
        prompt = "Font: ",
    }, function(selected_font)
        fontfamily = selected_font
    end)

    vim.ui.input({
        prompt = "Size: ",
    }, function(size)
        fontsize = size
    end)

    if fontfamily ~= "" and fontsize ~= "" then
        vim.o.guifont = string.format('%s:h%d', fontfamily, fontsize)
    end
end

vim.cmd [[
    command! Font :lua SetFont()<cr>
]]

vim.g.neovide_cursor_animation_length = 0.02
vim.g.neovide_cursor_trail_size = 0.0
vim.g.neovide_scroll_animation_length = 0.1
vim.g.neovide_input_macos_option_key_is_meta = 'both'

-- Terminal Emulator
local toggle_term_size_scale = 0.4
local toggle_term_buffer = 0
local toggle_term_direction = 'h'
local toggle_term_is_open = false
local function toggle_term()
    if toggle_term_is_open then
        toggle_term_is_open = not toggle_term_is_open
        vim.cmd [[ close ]]
    else
        toggle_term_is_open = not toggle_term_is_open
        if toggle_term_direction == 'h' then
            vim.cmd(string.format([[ split %d]], vim.o.lines * toggle_term_size_scale))
        elseif toggle_term_direction == 'v' then
            vim.cmd(string.format([[ vsplit %d]], vim.o.columns * toggle_term_size_scale))
        end
        if toggle_term_buffer == 0 then
            vim.cmd [[ term ]]
            toggle_term_buffer = vim.api.nvim_get_current_buf()
        else
            vim.api.nvim_win_set_buf(0, toggle_term_buffer)
        end

        vim.cmd [[ startinsert ]]
    end
end

vim.keymap.set({ "n", 'i', 't' }, '<C-j>', toggle_term)

-- Colors and UI setup
require("tokyonight").setup({ transparent = TRANSPARENT })
require("catppuccin").setup({ transparent_background = TRANSPARENT })
require("onedark").setup({ style = 'dark' })
require("lualine").setup()

vim.cmd.colorscheme("onedark")

-- Treesitter
require("nvim-treesitter.configs").setup({
    ensure_installed = { "lua", "go", "gomod", "markdown", "php", "c", "cpp" },
    highlight = { enable = true },
})


-- Telescope
require('telescope').load_extension('fzf')
require("telescope").load_extension("ui-select")
local telescope_keys = {
    ["<leader>p"] = "git_files",
    ["<leader><leader>"] = "find_files",
    ["??"] = "live_grep",
    ["<leader>h"] = "help_tags",
    ["<leader>b"] = "buffers",
    ["<leader>fs"] = "lsp_dynamic_workspace_symbols",

}
for k, v in pairs(telescope_keys) do
    vim.keymap.set("n", k, function()
        require "telescope.builtin"[v]({
            previewer = false
        })
    end, { desc = string.format("Telescope %s", v) })
end


-- LSP
require("mason").setup()
require("mason-lspconfig").setup({ ensure_installed = { "gopls", "rustـanalyzer", "lua_ls" } })
local lsp_servers = {
    gopls = {},
    intelephense = {},
    lua_ls = {
        settings = {
            Lua = {
                telemetry = { enable = false },
                diagnostics = {
                    globals = { "vim" },
                },
            },
        },
    },
    rust_analyzer = {},
    zls = {},
}

for server, config in pairs(lsp_servers) do
    require("lspconfig")[server].setup(config)
end

-- LspInfo window have rounded border
require("lspconfig.ui.windows").default_options.border = "single"
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover,
    { border = "rounded" })

vim.lsp.handlers["textDocument/signatureHelp"] =
    vim.lsp.with(vim.lsp.handlers.signature_help, { border = "rounded" })
vim.diagnostic.config({
    float = {
        border = "rounded",
    },
})

vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(args)
        local bufnr = args.buf
        vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc",
            { buf = bufnr })

        local map = function(mode, key, fn, desc)
            vim.keymap.set(mode, key, fn, { buffer = bufnr, desc = "LSP: " .. desc })
        end
        local references = vim.lsp.buf.references
        local implementation = vim.lsp.buf.implementation
        local has_tele, tele = pcall(require, "telescope.builtin")
        if has_tele then
            references = tele.lsp_references
            implementation = tele.lsp_implementations
        end
        map("n", "<leader>e", ":Trouble diagnostics toggle<CR>", "Trouble Toggle")
        map("n", "<C-e>", ":Trouble diagnostics toggle<CR>", "Trouble Toggle")

        map("n", "gd", vim.lsp.buf.definition, "[g]oto [d]efinition")
        map("n", "gD", vim.lsp.buf.declaration, "[g]oto [D]eclaration")
        map("n", "gI", implementation, "[g]oto [i]mplementation")
        map("n", "gr", references, "[g]oto [r]eferences")
        map("n", "R", vim.lsp.buf.rename, "Rename")
        map("n", "K", vim.lsp.buf.hover, "Hover")
        map("n", "C", vim.lsp.buf.code_action, "Code Actions")
        map("n", "<leader>f", vim.lsp.buf.format, "Format")
        map({ "n", "i" }, "<C-s>", vim.lsp.buf.signature_help, "Signature Help")
        vim.diagnostic.config({ virtual_text = false })

        vim.api.nvim_create_autocmd("BufWritePre", {
            buffer = bufnr,
            callback = function()
                local original_notify = vim.notify
                vim.notify = function(msg, log_level, opts)
                    if msg ~= "No code actions available" then
                        original_notify(msg, log_level, opts)
                    end
                end
                vim.lsp.buf.code_action({ context = { only = { "source.organizeImports" } }, apply = true })
                vim.notify = original_notify
                vim.lsp.buf.format()
            end
        })
    end,
})

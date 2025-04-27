local g, o, keymap = vim.g, vim.o, vim.keymap.set

g.mapleader = " "
g.maplocalleader = ","
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"

if not (vim.uv or vim.loop).fs_stat(lazypath) then
  local lazyrepo = "https://github.com/folke/lazy.nvim.git"
  local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
  if vim.v.shell_error ~= 0 then
    vim.api.nvim_echo({
      { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
      { out, "WarningMsg" },
      { "\nPress any key to exit..." },
    }, true, {})
    vim.fn.getchar()
    os.exit(1)
  end
end

o.rtp = o.rtp .. "," .. lazypath

require("lazy").setup({
  { "amirrezaask/nvim-gruvbuddy.lua" }, -- Colorscheme, inspired by great @tjdevries.

  { "supermaven-inc/supermaven-nvim", opts = {} }, -- AI Apocalypse
  { "MagicDuck/grug-far.nvim", opts = {} }, -- Find/Replace project wide.
  { "stevearc/oil.nvim", dependencies = { "nvim-tree/nvim-web-devicons" }, opts = {} }, -- File manager done right.
  { "williamboman/mason.nvim", opts = {} }, -- Package manager for your system inside neovim.

  { -- Blazingly fast autocomplete
    "saghen/blink.cmp",
    tag = "v1.1.1",
    opts = {
      keymap = { preset = "enter" },
      cmdline = { enabled = false },
    },
  },

  { -- Autoformat/fixes
    "stevearc/conform.nvim",
    config = function()
      require("conform").setup({
        formatters_by_ft = {
          lua = { "stylua" },
          go = { "goimports" },
          php = {},
        },
      })
      vim.api.nvim_create_autocmd("BufWritePre", {
        pattern = { "*.lua", "*.go" },
        callback = function(args)
          require("conform").format({ bufnr = args.buf })
        end,
      })
    end,
  },

  {
    "ibhagwan/fzf-lua",
    dependencies = {
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      Fzf = require("fzf-lua")
      Fzf.setup {
        fzf_colors = true,
        keymap = {
          fzf = {
            ["ctrl-q"] = "select-all+accept", -- Select all items and send to quickfix
          },
        },
      }

      vim.api.nvim_set_hl(0, "FzfLuaNormal", { link = "NormalFloat" })
      vim.api.nvim_set_hl(0, "FzfLuaBorder", { link = "NormalFloat" })

      Fzf.register_ui_select()

      vim.keymap.set("n", "<leader><leader>", Fzf.files, { desc = "Find Files" })
      vim.keymap.set("n", "<C-r>", Fzf.files, { desc = "Find Files" })
      vim.keymap.set("n", "<leader>b", Fzf.buffers, { desc = "Find Buffers" })
      vim.keymap.set("n", "<leader>h", Fzf.helptags, { desc = "Vim Help Tags" })
      vim.keymap.set("n", "<C-p>", Fzf.git_files, { desc = "Git Files" })
      vim.keymap.set("n", "??", Fzf.live_grep, { desc = "Live Grep" })
      vim.keymap.set("n", "<leader>fw", Fzf.grep, { desc = "Grep" })
      vim.keymap.set("v", "??", Fzf.grep_cword, { desc = "Grep word under cursor" })
      vim.keymap.set("n", "<leader>o", Fzf.lsp_document_symbols, { desc = "LSP Document Symbols" })
      vim.keymap.set("n", "<leader>O", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<M-o>", Fzf.lsp_live_workspace_symbols, { desc = "LSP Workspace Symbols" })
      vim.keymap.set("n", "<leader>fd", function()
        Fzf.files({ cwd = "~/.dotfiles" })
      end, { desc = "Find Dotfiles" })
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter",
    dependencies = { "nvim-treesitter/nvim-treesitter-textobjects" },
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = "all",
        highlight = { enable = true },
        auto_install = true,
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<C-space>",
            node_incremental = "<C-space>",
            scope_incremental = false,
            node_decremental = "<bs>",
          },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = { query = "@class.inner", desc = "Select inner part of a class region" },
              ["as"] = { query = "@local.scope", query_group = "locals", desc = "Select language scope" },
            },
            selection_modes = {
              ["@parameter.outer"] = "v", -- charwise
              ["@function.outer"] = "V", -- linewise
              ["@class.outer"] = "<c-v>", -- blockwise
            },
            include_surrounding_whitespace = true,
          },
        },
      }
    end,
  },

  {
    "lewis6991/gitsigns.nvim",
    config = function()
      require("gitsigns").setup {
        signs = {
          add = { text = "+" },
          change = { text = "~" },
          delete = { text = "_" },
          topdelete = { text = "‾" },
          changedelete = { text = "~" },
        },
      }
    end,
  },
})

vim.g.gruvbuddy_style = "dark"
vim.cmd.colorscheme("gruvbuddy")

o.wrap = true -- Wrap long lines.
o.breakindent = true -- Indent wrapped lines.
o.signcolumn = "yes" -- Show signcolumn.
o.swapfile = false -- Disable swapfile.
o.undofile = true -- Store undo history on disk
o.splitbelow = true -- Split windows below the current windows
o.splitright = true -- Split windows right to the current windows
o.showmode = false -- Don't show Vim mode in the command line.
o.clipboard = "unnamedplus" -- Copy/Cut/Paste to system clipboard
o.ignorecase = true -- Search case insensitive...
o.smartcase = true -- ... but not if it contains caps
o.cursorline = true -- Highlight current line
o.guicursor = o.guicursor .. ",t:ver25"
o.updatetime = 100 -- Faster completion
o.laststatus = 3 -- Single Statusline for all windows
o.timeoutlen = 300 -- Faster completion
o.number = true -- Line numbers
o.termguicolors = true -- Enable 24-bit RGB colors
o.winborder = "rounded" -- All floating windows will have rounded borders
o.inccommand = "split" -- Show partial commands in the command line
o.relativenumber = true -- Relative line numbers
o.scrolloff = 10 -- Scroll when cursor is 8 lines away from screen edge
o.list = true -- Show whitespace
o.listchars = "tab:» ,trail:·,extends:<,precedes:>,eol:↲,conceal:┊,nbsp:␣"

_G.statusline_filetype_icon = function()
  local filetype = vim.bo.filetype or "Unknown"
  local icon
  pcall(function()
    icon = require("nvim-web-devicons").get_icon(filetype)
  end)
  return icon or ""
end
function _G.statusline_mode()
  local mode = vim.api.nvim_get_mode().mode
  local mode_map = {
    ["n"] = "Normal",
    ["i"] = "Insert",
    ["v"] = "Visual",
    ["V"] = "Visual Line",
    ["\22"] = "Visual Block", -- \22 is Ctrl-V
    ["c"] = "Command",
    ["R"] = "Replace",
    ["s"] = "Select",
    ["S"] = "Select Line",
    ["\19"] = "Select Block", -- \19 is Ctrl-S
    ["t"] = "Terminal",
    ["no"] = "Operator Pending",
    ["niI"] = "Normal (Insert)",
    ["niR"] = "Normal (Replace)",
    ["niV"] = "Normal (Virtual Replace)",
    ["nt"] = "Normal (Terminal)",
    ["rm"] = "More Prompt",
    ["r?"] = "Confirm",
    ["!"] = "Shell",
  }

  return mode_map[mode] or "Unknown"
end

o.statusline =
  "[%{v:lua.statusline_mode()}]%#StatusLine#  %{get(b:,'gitsigns_head','')} [%{get(b:,'gitsigns_status','')}] %= %{v:lua.statusline_filetype_icon()} %F%m %=[%l:%c]%y"

keymap("n", "Y", "^v$y", { desc = "Copy whole line" })
keymap("t", "<esc>", [[<C-\><C-n>]])
keymap("i", "<C-c>", "<esc>")
keymap("n", "<C-d>", "<C-d>zz")
keymap("n", "<C-u>", "<C-u>zz")
keymap({ "n" }, "<C-j>", "<C-w>j") -- Window navigation
keymap({ "n" }, "<C-k>", "<C-w>k") -- Window navigation
keymap({ "n" }, "<C-h>", "<C-w>h") -- Window navigation
keymap({ "n" }, "<C-l>", "<C-w>l") -- Window navigation

keymap({ "t" }, "<C-j>", "<C-\\><C-n><C-w>j", { noremap = true }) -- Window navigation
keymap({ "t" }, "<C-k>", "<C-\\><C-n><C-w>k", { noremap = true }) -- Window navigation
keymap({ "t" }, "<C-h>", "<C-\\><C-n><C-w>h", { noremap = true }) -- Window navigation
keymap({ "t" }, "<C-l>", "<C-\\><C-n><C-w>l", { noremap = true }) -- Window navigation
keymap("n", "n", "nzz")
keymap("n", "N", "Nzz")
keymap("i", "jk", "<ESC>")
keymap("i", "kj", "<ESC>")
-- keymap("t", "jk", "<C-\\><C-n>", { noremap = true })
-- keymap("t", "kj", "<C-\\><C-n>", { noremap = true })
keymap("n", "<CR>", [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], { expr = true })
keymap("n", "j", "gj")
keymap("n", "k", "gk")
keymap("n", "{", "<cmd>cprev<CR>")
keymap("n", "}", "<cmd>cnext<CR>")
keymap({ "n", "t" }, "<M-k>", "<cmd>wincmd q<CR>")
keymap("n", "<C-q>", function()
  local wins = vim.api.nvim_list_wins()
  for _, win in ipairs(wins) do
    local buf = vim.api.nvim_win_get_buf(win)
    if vim.api.nvim_get_option_value("buftype", { buf = buf }) == "quickfix" then
      vim.cmd.cclose()
      return
    end
  end
  vim.cmd.copen()
end)

keymap({ "n", "t" }, "<C-s>", function() -- Toggle terminal at the bottom of the screen, why install a plugin for this ?
  -- We have a valid buffer showing the terminal
  if not vim.g.bottom_terminal_buffer or not vim.api.nvim_buf_is_valid(vim.g.bottom_terminal_buffer) then
    vim.g.bottom_terminal_buffer = vim.api.nvim_create_buf(false, true)
  end
  for _, win in ipairs(vim.api.nvim_list_wins()) do -- Toggle if a window showing terminal is open
    local win_buf = vim.api.nvim_win_get_buf(win)
    if win_buf == vim.g.bottom_terminal_buffer then
      vim.api.nvim_win_hide(win)
      return
    end
  end

  local win = vim.api.nvim_open_win(vim.g.bottom_terminal_buffer, true, {
    win = -1,
    split = "below",
    height = math.floor(vim.o.lines * 0.3),
    width = vim.o.columns,
  })
  vim.wo[win].winfixheight = true

  if vim.bo[vim.g.bottom_terminal_buffer].buftype ~= "terminal" then
    vim.cmd.term()
    vim.g.bottom_terminal_buffer = vim.api.nvim_get_current_buf()
  end

  vim.cmd.startinsert()
end)

vim.diagnostic.config({ virtual_text = true })

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(args)
    vim.keymap.set("n", "[[", function()
      vim.diagnostic.jump({ count = -1 })
    end, { buffer = args.buf })
    keymap("n", "]]", function()
      vim.diagnostic.jump({ count = 1 })
    end, { buffer = args.buf })
    keymap("n", "C-]", vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gd", Fzf.lsp_definitions or vim.lsp.buf.definition, { buffer = args.buf })
    keymap("n", "gD", Fzf.lsp_declaration or vim.lsp.buf.declaration, { buffer = args.buf })
    keymap("n", "gr", Fzf.lsp_references or vim.lsp.buf.references, { buffer = args.buf })
    keymap("n", "gi", Fzf.lsp_implementation or vim.lsp.buf.implementation, { buffer = args.buf })
    keymap("n", "R", vim.lsp.buf.rename, { buffer = args.buf })
    keymap("n", "K", vim.lsp.buf.hover, { buffer = args.buf })
    keymap("n", "C", vim.lsp.buf.code_action, { buffer = args.buf })
    keymap("n", "<leader>s", vim.lsp.buf.signature_help, { buffer = args.buf })
    keymap("n", "<leader>l", vim.diagnostic.open_float, { buffer = args.buf })
    keymap("n", "<leader>q", vim.diagnostic.setloclist, { buffer = args.buf })
  end,
})

vim.lsp.config("gopls", {
  cmd = { "gopls" },
  filetypes = { "go" },
  root_markers = { "go.mod", "go.sum", ".git" },
})
vim.lsp.config("intelephense", {
  cmd = { "intelephense", "--stdio" },
  filetypes = { "php" },
  root_markers = { "composer.json", ".git" },
})

vim.lsp.config("lua_ls", {
  cmd = { "lua-language-server" },
  filetypes = { "lua" },
  root_markers = { ".git" },
  settings = {
    Lua = {
      workspace = {
        userThirdParty = { os.getenv("HOME") .. ".local/share/LuaAddons" },
        checkThirdParty = "Apply",
      },
      diagnostics = {
        globals = { "vim" },
      },
    },
  },
})
vim.lsp.enable({ "gopls", "intelephense", "lua_ls" })

-- Programming languages setup
vim.api.nvim_create_autocmd("FileType", {
  pattern = "go",
  callback = function(args)
    vim.bo[args.buf].sw = 4
    vim.bo[args.buf].ts = 4
    vim.bo[args.buf].expandtab = false
    vim.bo[args.buf].shiftwidth = 4

    local function run_go_command_in_split(command_with_opts)
      return function()
        print("running " .. table.concat(command_with_opts, " "))
        local cwd = vim.fn.getcwd()

        vim.system(command_with_opts, { cwd = cwd }, function(obj)
          vim.schedule(function()
            local msg = ""
            if obj.code ~= 0 then
              msg = "Go Build Failed!"
              vim.notify(msg, vim.log.levels.ERROR)
            else
              msg = "Go Build Succeded!"
              vim.notify(msg, vim.log.levels.INFO)
            end

            if not vim.g.go_build_buffer or not vim.api.nvim_buf_is_valid(vim.g.go_build_buffer) then
              vim.g.go_build_buffer = vim.api.nvim_create_buf(false, true)
            end

            vim.api.nvim_buf_set_option(vim.g.go_build_buffer, "modifiable", true)
            local lines = vim.split(obj.stdout .. obj.stderr, "\n", { trimempty = true })
            table.insert(lines, 1, "Go Build Output:")
            table.insert(lines, #lines + 1, msg)

            vim.api.nvim_buf_set_lines(vim.g.go_build_buffer, 0, -1, false, lines)
            vim.api.nvim_buf_set_option(vim.g.go_build_buffer, "buftype", "nofile")
            vim.api.nvim_buf_set_option(vim.g.go_build_buffer, "bufhidden", "wipe")
            vim.api.nvim_buf_set_option(vim.g.go_build_buffer, "swapfile", false)
            vim.api.nvim_buf_set_option(vim.g.go_build_buffer, "modifiable", false)

            for _, win in ipairs(vim.api.nvim_list_wins()) do -- Toggle if a window showing terminal is open
              local win_buf = vim.api.nvim_win_get_buf(win)
              if win_buf == vim.g.go_build_buffer then
                return
              end
            end

            vim.api.nvim_open_win(vim.g.go_build_buffer, true, {
              split = "right",
              width = math.floor(vim.o.columns * 0.3),
            })
          end)
        end)
      end
    end

    vim.keymap.set("n", "<C-enter>", run_go_command_in_split({ "go", "build", "-v", "./..." }), { buffer = args.buf })

    vim.keymap.set("n", "<M-enter>", run_go_command_in_split({ "go", "test", "-v", "./..." }), { buffer = args.buf })
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "lua",
  callback = function(args)
    vim.keymap.set("n", "<C-enter>", ":so %<CR>", { buffer = args.buf })
    vim.bo[args.buf].sw = 2
    vim.bo[args.buf].ts = 2
    vim.bo[args.buf].expandtab = true
    vim.bo[args.buf].shiftwidth = 2
  end,
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "php",
  callback = function(args)
    vim.bo[args.buf].sw = 4
    vim.bo[args.buf].ts = 4
    vim.bo[args.buf].expandtab = false
    vim.bo[args.buf].shiftwidth = 4

    vim.diagnostic.config({ virtual_text = false })
  end,
})

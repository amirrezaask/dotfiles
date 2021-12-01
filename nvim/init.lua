--     ___              _                            ___         __
--    /   |  ____ ___  (_)____________  ____  ____ _/   |  _____/ /__
--   / /| | / __ `__ \/ / ___/ ___/ _ \/_  / / __ `/ /| | / ___/ //_/
--  / ___ |/ / / / / / / /  / /  /  __/ / /_/ /_/ / ___ |(__  ) ,<
-- /_/  |_/_/ /_/ /_/_/_/  /_/   \___/ /___/\__,_/_/  |_/____/_/|_|

-- Install package manager
local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim" } -- Plugin manager
    use { "navarasu/onedark.nvim" }
    use { "amirrezaask/nline.nvim", requires = { "nvim-lua/plenary.nvim", "nvim-lua/lsp-status.nvim" } } -- Statusline
    use { "nvim-telescope/telescope.nvim", requires = { "nvim-lua/plenary.nvim" } } -- UI to search for things
    use { "tpope/vim-surround" } -- Vim surround objects
    use { "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } } -- Gitsigns
    use { "tpope/vim-commentary" } -- Comment codes at ease
    use { "neovim/nvim-lspconfig" } -- LSP configurations
    use { "norcalli/nvim-colorizer.lua", branch = "color-editor" }
    use { "honza/dockerfile.vim" } -- Dockerfile
    use { "hashivim/vim-terraform" } -- Terraform
    use { "LnL7/vim-nix" } -- Nix
    use { "dag/vim-fish" } -- Fish
    use { "cespare/vim-toml" } -- Toml
    use { "elixir-editors/vim-elixir" } -- Elixir
    use { "pearofducks/ansible-vim" } -- Ansible
    use { "Glench/Vim-Jinja2-Syntax" } -- Jinja2
    use { "amirrezaask/actions.nvim" } -- Define IDE like actions.
    use { "hrsh7th/nvim-cmp" } -- completion popup
    use { "hrsh7th/cmp-buffer" } -- source for completion from words in current buffer
    use { "hrsh7th/cmp-nvim-lua" } -- source for completion from neovim stuff
    use { "hrsh7th/cmp-nvim-lsp" } -- source for completion from lsp
    use { "hrsh7th/cmp-path" } -- source for completion from fs path
    use { "rust-lang/rust.vim", ft = "rust" } -- rust syntax
    use { "nvim-treesitter/nvim-treesitter" } -- treesitter integration
    use { "nvim-treesitter/nvim-treesitter-textobjects" } -- more text objects for treesitter
    use { "mfussenegger/nvim-dap" } -- debug adapter protocol
    use { "theHamsta/nvim-dap-virtual-text" } -- debug adapter protocol virtual text
    use { "kyazdani42/nvim-web-devicons" } -- Icons
    use { "yamatsum/nvim-nonicons" } -- better Icons
    use { "folke/todo-comments.nvim", requires = "nvim-lua/plenary.nvim" } -- Highlight todo and etc...
    use { "godlygeek/tabular" } -- beautify text
    use { "tjdevries/nlua.nvim" } -- Better lua dev for neovim
    use { "milisims/nvim-luaref" } -- lua reference as vim help
    use { "nanotee/luv-vimdocs" } -- luv reference as vim help
    use { "lukas-reineke/indent-blankline.nvim" }
  end,
}
-- Basic vim options
vim.opt.smartcase = true -- care about case of chars when we have capital ones in search.
vim.opt.equalalways = false -- don't change windows size after closing one
vim.opt.modeline = true
vim.opt.autoread = true
vim.opt.compatible = false -- no compatibility with vim.
vim.opt.encoding = "utf-8" -- default encoding
vim.opt.hlsearch = true -- highlight matched when searching
vim.opt.history = 700
vim.opt.tabpagemax = 100
vim.opt.ruler = true -- show line/col in statusbar
vim.opt.mouse = "a" -- enable mouse
vim.opt.wrap = true
vim.opt.autoindent = true
vim.opt.termguicolors = true
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false
vim.opt.writebackup = false -- no vim backup file
vim.opt.swapfile = false -- disable vim swap files
vim.opt.splitright = true -- always split window to right
vim.opt.splitbelow = true -- always split to below
vim.opt.cursorline = true -- highlight current line
vim.opt.relativenumber = true -- relative line numbers
vim.opt.number = true -- show current line number
vim.opt.pumblend = 5
vim.opt.showmode = false
vim.opt.clipboard = "unnamedplus"
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.wildmode = { "longest", "list", "full" }
vim.opt.wildmode = vim.opt.wildmode - "list"
vim.opt.wildmode = vim.opt.wildmode + { "longest", "full" }

local nnoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("n", lhs, rhs, { silent = true, noremap = true })
end
local tnoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("t", lhs, rhs, { silent = true, noremap = true })
end
local inoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("i", lhs, rhs, { silent = true, noremap = true })
end
local vnoremap = function(lhs, rhs)
  vim.api.nvim_set_keymap("v", lhs, rhs, { silent = true, noremap = true })
end

-- Map leader to <Space>
vim.g.mapleader = " "

-- Simpler split movement
nnoremap("Q", "<NOP>")
nnoremap(";", ":")
nnoremap("q;", "q:")

-- Window resizes
nnoremap("<Left>", ":vertical resize -5<CR>")
nnoremap("<Right>", ":vertical resize +5<CR>")
nnoremap("<Up>", ":resize +5<CR>")
nnoremap("<Down>", ":resize -5<CR>")

nnoremap("j", "gj")
nnoremap("k", "gk")

tnoremap("<Esc>", "<C-\\><C-n>")
tnoremap("jk", "<C-\\><C-n>")
tnoremap("kj", "<C-\\><C-n>")

inoremap("jk", "<esc>")
inoremap("kj", "<esc>")

-- Move lines jetbrains style -> Thanks to TJ again
nnoremap("<M-j>", ":m .+1<CR>==")
nnoremap("<M-k>", ":m .-2<CR>==")

inoremap("<M-j>", "<Esc>:m .+1<CR>==gi")
inoremap("<M-k>", "<Esc>:m .-2<CR>==gi")

vnoremap("<M-j>", ":m '>+1<CR>gv=gv")
vnoremap("<M-k>", ":m '<-2<CR>gv=gv")

nnoremap("Y", "y$")
nnoremap("n", "nzz")
nnoremap("N", "Nzz")

-- Thanks to TJ again
nnoremap("<expr><CR>", '{-> v:hlsearch ? ":nohl<CR>" : "<CR>"}')

-- Colorscheme
vim.g.onedark_style = "deep"
vim.cmd [[ colorscheme onedark ]]

-- highlight on yank
vim.cmd [[
  augroup YankHighlight
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank()
  augroup end
]]
-- Indent line
vim.g.indent_blankline_char = "┊"
vim.g.indent_blankline_filetype_exclude = { "help", "packer" }
vim.g.indent_blankline_buftype_exclude = { "terminal", "nofile" }
vim.g.indent_blankline_show_trailing_blankline_indent = false
vim.g.indent_blankline_show_current_context = true

-- Color Picker
function ColorPicker()
  _PICKER_ASHKAN_KIANI_COPYRIGHT_2020_LONG_NAME_HERE_ = nil
  require("colorizer").color_picker_on_cursor()
end
vim.cmd [[ autocmd BufEnter * ColorizerAttachToBuffer ]]
vim.cmd [[ command! ColorPicker lua ColorPicker ]] -- On an color code run ColorPicker command

-- Git signs and popups
require("gitsigns").setup {
  signs = {
    add = { text = "|", numhl = "GitSignsAddNr" },
    change = { text = "|", numhl = "GitSignsChangeNr" },
    delete = { text = "_", numhl = "GitSignsDeleteNr" },
    topdelete = { text = "‾", numhl = "GitSignsDeleteNr" },
    changedelete = { text = "~-", numhl = "GitSignsChangeNr" },
  },
  numhl = false,
  current_line_blame = false,
  current_line_blame_opts = {
    delay = 800,
    virt_text_pos = "eol",
  },
}
local telescope_actions = require "telescope.actions"
-- Telescope
require("telescope").setup {
  defaults = {
    mappings = {
      n = {
        ["<ESC>"] = telescope_actions.close,
        ["<C-c>"] = telescope_actions.close,
        ["jk"] = telescope_actions.close,
        ["kj"] = telescope_actions.close,
      },
      i = {
        ["<C-c>"] = telescope_actions.close,
        ["<C-q>"] = telescope_actions.send_to_qflist,
        ["<C-j>"] = telescope_actions.move_selection_next,
        ["<C-k>"] = telescope_actions.move_selection_previous,
      },
    },
  },
}

local ivy = require("telescope.themes").get_ivy
local current_theme = ivy { layout_config = { height = 0.4 } }
local theme = function(opts)
  return vim.tbl_extend("force", current_theme, opts)
end

function telescope_wrap(fn, opts)
  opts = opts or {}
  if type(fn) == "function" then
    fn(theme(opts))
  elseif type(fn) == "string" then
    require("telescope.builtin")[fn](theme(opts))
  end
end
-- some mappings for searching using telescope
vim.api.nvim_set_keymap(
  "n",
  "<leader><leader>",
  '<cmd>lua telescope_wrap("find_files", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>fp",
  '<cmd>lua telescope_wrap("find_files", {hidden = true, cwd = "~/.local/share/nvim/site/pack/packer"})<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>ps",
  '<cmd>lua telescope_wrap("find_files", {hidden = true, cwd = "~/src/gitlab.snapp.ir/" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<C-q>",
  '<cmd>lua telescope_wrap("quickfix", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "??",
  '<cmd>lua telescope_wrap("live_grep", {hidden = true })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "<leader>en",
  '<cmd>lua telescope_wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)
vim.api.nvim_set_keymap(
  "n",
  "gd",
  '<cmd>lua telescope_wrap("find_files", { hidden = true, cwd = "~/.config/nvim" })<CR>',
  { silent = true, noremap = true }
)
-- LSP
local lspconfig = require "lspconfig"
local on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  local opts = { noremap = true, silent = true }
  vim.api.nvim_set_keymap(
    "n",
    "gd",
    '<cmd>lua telescope_wrap("lsp_definitions", {hidden = true })<CR>',
    { silent = true, noremap = true }
  )
  vim.api.nvim_set_keymap(
    "n",
    "gi",
    '<cmd>lua telescope_wrap("lsp_implementations", {hidden = true })<CR>',
    { silent = true, noremap = true }
  )
  vim.api.nvim_set_keymap(
    "n",
    "gr",
    '<cmd>lua telescope_wrap("lsp_references", {hidden = true })<CR>',
    { silent = true, noremap = true }
  )
  vim.api.nvim_set_keymap(
    "n",
    "?d",
    '<cmd>lua telescope_wrap("lsp_document_symbols", {hidden = true })<CR>',
    { silent = true, noremap = true }
  )
  vim.api.nvim_set_keymap(
    "n",
    "?w",
    '<cmd>lua telescope_wrap("lsp_workspace_symbols", {hidden = true })<CR>',
    { silent = true, noremap = true }
  )
  vim.api.nvim_set_keymap(
    "n",
    "?c",
    '<cmd>lua telescope_wrap("lsp_code_actions", {hidden = true })<CR>',
    { silent = true, noremap = true }
  )

  vim.api.nvim_buf_set_keymap(bufnr, "n", "R", "<cmd>lua vim.lsp.buf.rename()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua <CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-s>", "<cmd>lua <CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "<c-d>", "<cmd>lua <CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "[d", "<cmd>lua <CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "]d", "<cmd>lua <CR>", opts)

  vim.cmd [[ command! Format execute '<cmd>lua vim.lsp.buf.formatting()' ]]

  if client.resolved_capabilities.document_highlight then -- highlight current symbol usages in code
    vim.cmd [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
  ]]
  end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

local servers = { "clangd", "rust_analyzer", "gopls", "intelephense" }

for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup {
    on_attach = on_attach,
    capabilities = capabilities,
  }
end

local sumneko_root = "/home/amirreza/.local/lua-language-server"
local sumneko_binary = sumneko_root .. "/bin/Linux/lua-language-server"

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

require("lspconfig").sumneko_lua.setup {
  cmd = { sumneko_binary, "-E", sumneko_root .. "/main.lua" },
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
        -- Setup your lua path
        path = runtime_path,
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}
-- IDE like actions
local actions = require "actions"
local utils = require "actions.utils"

actions:setup {
  mappings = {
    ["n ,ab"] = "build",
    ["n ,at"] = "test_all",
    ["n ,tt"] = "test_this",
    ["n ,ar"] = "run",
    ["n ,af"] = "format",
  },
  {
    predicate = utils.make_language_predicate "vim",
    actions = {
      run = function(_)
        vim.cmd [[ so % ]]
      end,
    },
  },
  {
    predicate = utils.compose(utils.make_language_predicate "lua", utils.make_path_predicate "plugins.lua"),
    actions = {
      run = function(bufnr)
        vim.c.luafile(vim.api.nvim_buf_get_name(bufnr))
        vim.cmd [[ PackerInstall ]]
      end,
    },
  },
  {
    predicate = utils.make_language_predicate "lua",
    actions = {
      run = function(bufnr)
        vim.cmd [[ luafile(vim.api.nvim_buf_get_name(bufnr)) ]]
      end,
      format = function(bufnr)
        require("stylua"):run(bufnr)
      end,
    },
  },
  {
    predicate = utils.make_language_predicate "rust",
    actions = {
      run = function(_)
        vim.cmd [[ vnew | term cargo run ]]
      end,
      build = function(_)
        vim.cmd [[ vnew | term cargo check ]]
      end,
      test_all = function(_)
        vim.cmd [[ RustTest! ]]
      end,
      test_this = function(_)
        vim.cmd [[ RustTest ]]
      end,
    },
  },
  {
    predicate = utils.make_path_predicate "gitlab.snapp.ir",
    actions = {
      format = function() end,
    },
  },
  {
    predicate = utils.make_language_predicate "go",
    actions = {
      format = function(bufnr)
        bufnr = bufnr or 0
        vim.cmd [[ write ]]
        local job = require("plenary.job"):new {
          "goimports",
          vim.api.nvim_buf_get_name(0),
        }

        local output = job:sync()

        if job.code ~= 0 then
          return
        end

        vim.api.nvim_buf_set_lines(bufnr, 0, -1, false, output)
      end,
      build = function(_)
        vim.cmd [[ vnew | term go build ]]
      end,
      run = function()
        vim.cmd [[ vnew | term go run *.go ]]
      end,
      test_all = function(_)
        vim.cmd [[ vnew | term go test -v ./... ]]
      end,
      test_this = function(_)
        local function go_current_test()
          local linenr = vim.fn.search("func \\(Test\\|Example\\)", "bcnW")
          if linenr == 0 then
            return
          end
          local linetext = vim.fn.getline(linenr)
          local test_name = vim.split(linetext, " ")[2]
          local start, _ = string.find(test_name, "%(")
          test_name = string.sub(test_name, 1, start - 1)
          return test_name
        end
        local current_test = go_current_test()
        vim.cmd(string.format([[ vnew | term go test -v -run %s ]], current_test))
      end,
    },
  },
}

vim.cmd [[ autocmd BufWritePre * lua Actions:exec(0, 'format') ]] -- auto format files on save

-- Statusline
local nline = require "nline"
local nline_vim = require "nline.parts.vim"
local git = require "nline.parts.git"
local lsp = require "nline.parts.lsp"
local wrappers = require "nline.wrappers"
local icons = require "nline.parts.icons"

local tj = {
  nline_vim.mode {
    texts = {
      normal = "Normal",
      visual = "Visual",
      visual_block = "VisualBlock",
      insert = "Insert",
      insert_complete = "IComplete",
      command = "Command",
      terminal = "Terminal",
    },
  },
  nline_vim.space(),
  icons.git_branch,
  nline_vim.space(),
  git.branch(),

  nline_vim.seperator(),

  icons.file,
  nline_vim.space(),
  nline_vim.filename { shorten = true },
  nline_vim.modified(),

  nline_vim.seperator(),

  -- wrappers.square_brackets(lsp.current_function()),
  wrappers.square_brackets(git.changes()),
  wrappers.square_brackets(lsp.diagnostics()),
  lsp.progress(),
  wrappers.square_brackets(nline_vim.line() .. nline_vim.space() .. nline_vim.colon() .. nline_vim.col()),
  nline_vim.filetype(),
}

nline.make(tj)

-- Auto complete
vim.opt.completeopt = { "menuone", "noselect" }

-- Don't show the dumb matching stuff.
vim.opt.shortmess:append "c"

local cmp = require "cmp"
cmp.setup {
  snippet = {},

  -- You can set mapping if you want.
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Insert,
      select = true,
    },
  },

  -- You should specify your *installed* sources.
  sources = {
    { name = "buffer" },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}

-- Debugger
local dap = require "dap"
vim.g.dap_virtual_text = true
dap.adapters.go = function(callback, _)
  local port = 38697
  local handle
  handle, _ = vim.loop.spawn("dlv", {
    args = { "dap", "-l", "127.0.0.1:" .. port },
    detached = true,
  }, function(code)
    handle:close()
    print("Delve exited with exit code: " .. code)
  end)
  vim.defer_fn(function()
    dap.repl.open()
    callback { type = "server", host = "127.0.0.1", port = port }
  end, 100)
  callback { type = "server", host = "127.0.0.1", port = port }
end
dap.configurations.go = {
  {
    type = "go",
    name = "Debug",
    request = "launch",
    program = "${file}",
  },
}

-- Commands
vim.cmd [[ command! DapToggleBreakpoint lua require("dap").toggle_breakpoint) ]]
vim.cmd [[ command! DapReplOpen lua  require("dap").repl.open) ]]
vim.cmd [[ command! DapContinue lua  require("dap").continue) ]]
vim.cmd [[ command! DapStepInto lua  require("dap").step_into) ]]
vim.cmd [[ command! DapStepOver lua  require("dap").step_over) ]]
vim.cmd [[ command! DapStepOut lua require("dap").step_out) ]]
vim.cmd [[ command! DapHover lua require("dap.ui.variables").hover) ]]

-- Mappings
vim.cmd [[ map <F3> lua require("dap").toggle_breakpoint ]]
vim.cmd [[ map <F4> lua require("dap").repl.open ]]
vim.cmd [[ map <F9> lua require("dap").continue ]]
vim.cmd [[ map <F7> lua require("dap").step_into ]]
vim.cmd [[ map <F8> lua require("dap").step_over ]]
-- vim.cmd [[ map <F9> lua require("dap").step_out ]]
vim.cmd [[ map <F10> lua require("dap.ui.variables").hover ]]

-- Treesitter, better syntax highlight
require("nvim-treesitter.configs").setup {
  highlight = {
    enable = true, -- false will disable the whole extension
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "gnn",
      node_incremental = "grn",
      scope_incremental = "grc",
      node_decremental = "grm",
    },
  },
  indent = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
    move = {
      enable = true,
      set_jumps = true, -- whether to set jumps in the jumplist
      goto_next_start = {
        ["]m"] = "@function.outer",
        ["]]"] = "@class.outer",
      },
      goto_next_end = {
        ["]M"] = "@function.outer",
        ["]["] = "@class.outer",
      },
      goto_previous_start = {
        ["[m"] = "@function.outer",
        ["[["] = "@class.outer",
      },
      goto_previous_end = {
        ["[M"] = "@function.outer",
        ["[]"] = "@class.outer",
      },
    },
  },
}

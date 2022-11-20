local function packer_ensure()
  local fn = vim.fn
  local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
  if fn.empty(fn.glob(install_path)) > 0 then
    fn.system { "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path }
    vim.cmd [[packadd packer.nvim]]
    return true
  end
  return false
end

-- If packer.nvim is not installed, install it.
packer_ensure()

-- Now we install plugins

-- [[ Plugins
require("packer").startup {
  function(use)
    use "wbthomason/packer.nvim"

    -- Colorschemes
    use "eemed/sitruuna.vim"
    use "sainnhe/sonokai"
    use "folke/tokyonight.nvim"
    use "ellisonleao/gruvbox.nvim"
    use "bluz71/vim-nightfly-colors"

    use "junegunn/goyo.vim"

    -- UI enhancements
    use "nvim-tree/nvim-web-devicons"
    use "yamatsum/nvim-nonicons"
    use {
      "stevearc/dressing.nvim",
    }
    use {
      "folke/which-key.nvim",
    }

    -- LSP
    use {
      "j-hui/fidget.nvim",
    }
    use {
      "williamboman/mason.nvim",
    }
    use "williamboman/mason-lspconfig.nvim"
    use "jayp0521/mason-nvim-dap.nvim"

    use "neovim/nvim-lspconfig"
    use {
      "ray-x/lsp_signature.nvim",
    }
    use {
      "glepnir/lspsaga.nvim",
      branch = "main",
    }
    use "onsails/lspkind.nvim"

    -- JSON Scheumas
    use "b0o/schemastore.nvim"

    -- Git stuff
    use "tpope/vim-fugitive"
    use "junegunn/gv.vim"
    use "cohama/agit.vim"
    use {
      "lewis6991/gitsigns.nvim",
    }
    use {
      "TimUntersberger/neogit",
      requires = "nvim-lua/plenary.nvim",
    }

    -- Snippets
    use { "L3MON4D3/LuaSnip", tag = "v<CurrentMajor>.*" }
    use "rafamadriz/friendly-snippets"

    -- Completion
    use "hrsh7th/nvim-cmp"
    use "saadparwaiz1/cmp_luasnip" -- cmp luasnip source
    use "hrsh7th/cmp-buffer" -- cmp buffer source
    use "hrsh7th/cmp-nvim-lua" -- cmp neovim lua source
    use "hrsh7th/cmp-nvim-lsp" -- cmp neovim builtin lsp
    use "hrsh7th/cmp-path" -- cmp path source

    -- Treesitter
    use "nvim-treesitter/nvim-treesitter"
    use "nvim-treesitter/nvim-treesitter-textobjects"
    use "p00f/nvim-ts-rainbow"

    -- Telescope
    use { "nvim-telescope/telescope.nvim", requires = "nvim-lua/plenary.nvim" }
    use { "nvim-telescope/telescope-file-browser.nvim" }
    use {
      "nvim-telescope/telescope-fzf-native.nvim",
      run = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build",
    }

    use "sheerun/vim-polyglot"

    use "towolf/vim-helm"

    use "pbrisbin/vim-mkdir"

    use "tpope/vim-surround"

    use "junegunn/vim-easy-align"

    use "fladson/vim-kitty"

    use {
      "folke/todo-comments.nvim",
      requires = "nvim-lua/plenary.nvim",
    }

    use "ziglang/zig.vim"

    use "jansedivy/jai.vim"

    use { "ckipp01/stylua-nvim", run = "cargo install stylua" }

    -- Rust
    use "rust-lang/rust.vim"
    use "simrat39/rust-tools.nvim"

    use "folke/neodev.nvim"

    use "cuducos/yaml.nvim"

    use "Glench/Vim-Jinja2-Syntax"

    -- statusline
    use "nvim-lualine/lualine.nvim"

    -- more convinient terminal emulator
    use {
      "akinsho/toggleterm.nvim",
      tag = "*",
    }
    -- dashboard or start screen
    use {
      "goolord/alpha-nvim",
      requires = { "kyazdani42/nvim-web-devicons" },
    }
    -- Commenting plugin
    use {
      "numToStr/Comment.nvim",
    }
    -- Integrate Tmux and neovim pane and window switching
    use "christoomey/vim-tmux-navigator"
    -- Toggle window zoom like Tmux
    use "szw/vim-maximizer"
  end,
  config = {
    display = {
      open_fn = require("packer.util").float,
    },
  },
}
-- ]]

-- [[ Basic Options
vim.opt.ignorecase = true -- ignore case when searching
vim.opt.smartcase = true --don't ignore case in search when there is uppercase letter
vim.opt.equalalways = false -- don't resize windows on changing window state ( closing or splitting )
vim.opt.modeline = true --
vim.opt.autoread = true -- If you detect a file change read it automatically
vim.opt.compatible = false -- no need legacy VI compatibility
vim.opt.encoding = "utf-8" -- Default file encoding
vim.opt.hlsearch = true -- Highlight search matches
vim.opt.history = 700 -- Number of stored history items
vim.opt.tabpagemax = 100 -- Max number of tabs
vim.opt.ruler = true -- Show line and column in statusline
vim.opt.mouse = "a" -- enable mouse support for all modes
vim.opt.autoindent = true -- do auto indent when going to new line
vim.opt.cindent = true
vim.opt.wrap = false -- no line wrapping
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.softtabstop = 4
vim.opt.expandtab = true
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.swapfile = false
vim.opt.splitright = true -- vertical split creates a new window right of current window
vim.opt.splitbelow = true -- horizontal split creates a new window below current window
vim.opt.cursorline = true -- highlight current line
vim.opt.relativenumber = true -- show relative line numbers
vim.opt.number = true -- show current line number
vim.opt.showmode = true -- show current vim mode down of the screen
vim.opt.showcmd = true -- show commands as they are being typed
vim.opt.hidden = true
vim.opt.updatetime = 100
vim.opt.incsearch = true -- Continue search as I type characters
vim.opt.guioptions = "egmrti"
vim.opt.backspace = "indent,eol,start"
vim.opt.complete = vim.opt.complete + "i" -- don't search for all included files
vim.opt.wildmenu = true
vim.opt.wildoptions = "tagfile"
vim.opt.updatetime = 300
vim.opt.pumheight = 10 -- Completion window max size
vim.opt.conceallevel = 2 -- Concealed text is completely hidden
vim.opt.shortmess = vim.opt.shortmess + "c" -- Shut off completion messages
vim.opt.belloff = vim.opt.belloff + "ctrlg" -- If Vim beeps during completion
vim.opt.termguicolors = true -- true color support, needs terminal support as well
vim.opt.list = true -- use list characters that are defined below
vim.opt.listchars:append "tab:<->"
vim.opt.listchars:append "eol:↲"
vim.opt.listchars:append "trail:·"
vim.opt.listchars:append "lead:·"
vim.opt.timeoutlen = 500 -- when a key is pressed timeout until it's forgotten
vim.opt.clipboard:append "unnamedplus" -- use system clipboard by default

if vim.version().major >= 0 and vim.version().minor >= 8 then
  vim.opt.laststatus = 3 -- if supported use global statusline
end

-- ]]

local window_height = function()
  return vim.api.nvim_win_get_height(0)
end

local window_width = function()
  return vim.api.nvim_win_get_width(0)
end

-- Keymap helper
-- Set leader to <Space>
vim.g.mapleader = " "

local function bind(spec)
  for mode, keys in pairs(spec) do
    for key, binding in pairs(keys) do
      if type(binding) == "string" or type(binding) == "function" then
        vim.keymap.set(mode, key, binding)
      else
        if type(binding) == "table" then
          -- { function or string, doc }
          local handler = binding[1]
          table.remove(binding, 1)
          vim.keymap.set(mode, key, handler, binding)
        end
      end
    end
  end
end

-- [[ Basic keymaps
bind {
  n = {
    ["Q"] = "<NOP>",
    [";,"] = ":",
    ["q;"] = "q:",
    ["{"] = ":cprev<CR>",
    ["}"] = ":cnext<CR>",
    ["Y"] = "y$",
    ["n"] = "nzz",
    ["N"] = "Nzz",

    ["<M-j>"] = ":m .+1<CR>==",
    ["<M-k>"] = ":m .-2<CR>==",

    ["j"] = "gj",
    ["k"] = "gk",
    ["<leader>c"] = ":e ~/.config/nvim/init.lua<CR>",
    ["<CR>"] = { [[ {-> v:hlsearch ? ':nohl<CR>' : '<CR>'}() ]], expr = true },
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

-- ]]

-- [[ Colorscheme
vim.cmd [[ colorscheme nightfly ]]
-- ]]

local function onsave(pattern, callback)
  local augroup_name = ""
  if type(pattern) == "table" then
    augroup_name = table.concat(pattern, ",") .. "-onsave"
  end
  if type(pattern) == "string" then
    augroup_name = pattern .. "-onsave"
  end
  vim.api.nvim_create_autocmd("BufWritePost", {
    group = vim.api.nvim_create_augroup(augroup_name, {}),
    pattern = pattern,
    callback = callback,
  })
end
-- [[ Git
require("neogit").setup()
require("gitsigns").setup()
-- ]]

require("dressing").setup()

-- [[ Mason
require("mason").setup()
require("mason-nvim-dap").setup {
  automatic_installation = false,
}
-- ]]

-- [[ LSP
require("mason-lspconfig").setup {
  automatic_installation = true,
}

require("fidget").setup()
local function lsp_on_attach(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  bind {
    n = {
      gd = { vim.lsp.buf.definition, desc = "Goto definition", buffer = bufnr },
      gi = { vim.lsp.buf.implementation, desc = "Goto implementations", buffer = bufnr },
      gr = { vim.lsp.buf.references, desc = "Goto references", buffer = bufnr },
      R = { vim.lsp.buf.rename, desc = "Rename symbol under cursor", buffer = bufnr },
      K = { vim.lsp.buf.hover, desc = "Hover docs under cursor", buffer = bufnr },
      -- ["<c-d>"] = { vim.diagnostic.open_float, desc = "Show current line diagnostics", buffer = bufnr },
      ["[d"] = { vim.diagnostic.goto_prev, desc = "Goto previous diagnostic", buffer = bufnr },
      ["]d"] = { vim.diagnostic.goto_next, desc = "Goto next diagnostic", buffer = bufnr },
      ["C"] = { vim.lsp.buf.code_action, desc = "Code actions", buffer = bufnr },
      ["<C-s>"] = { vim.lsp.buf.signature_help, desc = "Toggle Signature help", buffer = bufnr },
    },
    i = {
      ["<C-s>"] = { vim.lsp.buf.signature_help, desc = "Toggle Signature help", buffer = bufnr },
    },
  }
  require("lsp_signature").on_attach({}, bufnr)
end

local border = {
  { "🭽", "FloatBorder" },
  { "▔", "FloatBorder" },
  { "🭾", "FloatBorder" },
  { "▕", "FloatBorder" },
  { "🭿", "FloatBorder" },
  { "▁", "FloatBorder" },
  { "🭼", "FloatBorder" },
  { "▏", "FloatBorder" },
}

local orig_util_open_floating_preview = vim.lsp.util.open_floating_preview
function vim.lsp.util.open_floating_preview(contents, syntax, opts, ...)
  opts = opts or {}
  opts.border = opts.border or border
  return orig_util_open_floating_preview(contents, syntax, opts, ...)
end
require("lspsaga").init_lsp_saga {
  symbol_in_winbar = {
    enable = true,
  },
}

-- ]]
-- [[ Lua
require("neodev").setup {}

local runtime_path = vim.split(package.path, ";")
table.insert(runtime_path, "lua/?.lua")
table.insert(runtime_path, "lua/?/init.lua")

require("nvim-treesitter.install").ensure_installed "lua"

onsave("*.lua", function()
  require("stylua-nvim").format_file()
end)

vim.api.nvim_create_autocmd("BufEnter", {
  pattern = "*.lua",
  callback = function()
    vim.cmd [[ setlocal sts=2 sw=2 ]]
  end,
})

-- JSON
require("lspconfig").jsonls.setup {
  on_attach = lsp_on_attach,
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
}

require("lspconfig").sumneko_lua.setup {
  on_attach = lsp_on_attach,
  commands = {
    Format = {
      function()
        require("stylua-nvim").format_file()
      end,
    },
  },
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
        checkThirdParty = false,
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
}
-- ]]

-- [[ Go
require("nvim-treesitter.install").ensure_installed "go"

require("lspconfig").gopls.setup {
  on_attach = lsp_on_attach,
}

onsave("*.go", function()
  vim.lsp.buf.format()
end)
-- ]]

-- [[ PHP
require("nvim-treesitter.install").ensure_installed "php"
require("lspconfig").intelephense.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ C/C++
require("nvim-treesitter.install").ensure_installed "c"
require("nvim-treesitter.install").ensure_installed "cpp"

require("lspconfig").clangd.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Elixir
require("nvim-treesitter.install").ensure_installed "elixir"
require("lspconfig").elixirls.setup {
  on_attach = lsp_on_attach,
  cmd = { os.getenv "HOME" .. "/.local/elixir-ls/language_server.sh" },
}
-- ]]

-- [[ Rust
local rt = require "rust-tools"

require("nvim-treesitter.install").ensure_installed "rust"

rt.setup {
  server = {
    on_attach = function(_, bufnr)
      lsp_on_attach(_, bufnr)
      bind {
        n = {
          C = { rt.hover_actions.hover_actions, desc = "Hover code actions", buffer = bufnr },
          ga = { rt.code_action_group.code_action_group, desc = "Code actions", buffer = bufnr },
        },
      }
    end,
  },
}

onsave("*.rs", function()
  vim.lsp.buf.format()
end)
-- ]]

-- [[ Purescript
require("lspconfig").purescriptls.setup {
  on_attach = lsp_on_attach,
}
--]]

-- [[ Zig
require("nvim-treesitter.install").ensure_installed "zig"
require("lspconfig").zls.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Haskell
require("nvim-treesitter.install").ensure_installed "haskell"
require("lspconfig").hls.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Python
require("lspconfig").jedi_language_server.setup {
  on_attach = lsp_on_attach,
}
-- ]]

-- [[ Yaml
require("nvim-treesitter.install").ensure_installed "yaml"
require("lspconfig").yamlls.setup {
  on_attach = lsp_on_attach,
}
vim.cmd [[
    autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
    autocmd FileType yaml setlocal cursorcolumn
]]
-- ]]

-- [[ JSON
require("nvim-treesitter.install").ensure_installed "json"
require("lspconfig").jsonls.setup {
  settings = {
    json = {
      schemas = require("schemastore").json.schemas(),
      validate = { enable = true },
    },
  },
}

-- ]]

-- [[ Lualine
require("lualine").setup {
  options = {
    icons_enabled = true,
    theme = "auto",
    component_separators = { left = "", right = "" },
    section_separators = { left = "", right = "" },
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    },
  },
  sections = {
    lualine_a = { "mode" },
    lualine_b = { "branch", "diff", "diagnostics" },
    lualine_c = { "filename" },
    lualine_x = { "encoding", "fileformat", "filetype" },
    lualine_y = { "progress" },
    lualine_z = { "location" },
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { "filename" },
    lualine_x = { "location" },
    lualine_y = {},
    lualine_z = {},
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {},
}

if vim.version().major >= 0 and vim.version().minor >= 8 then
  vim.opt.laststatus = 3
end
-- ]]

-- [[ nvim-cmp
local cmp = require "cmp"

cmp.setup {
  snippet = {
    expand = function(args)
      require("luasnip").lsp_expand(args.body)
    end,
  },
  formatting = {
    format = require("lspkind").cmp_format {
      mode = "symbol", -- show only symbol annotations
      maxwidth = 50, -- prevent the popup from showing more than provided characters (e.g 50 will not show more than 50 characters)
      ellipsis_char = "...", -- when popup menu exceed maxwidth, the truncated part would show ellipsis_char instead (must define maxwidth first)
    },
  },
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-d>"] = cmp.mapping.scroll_docs(-4),
    ["<C-f>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<CR>"] = cmp.mapping.confirm {
      behavior = cmp.ConfirmBehavior.Insert,
      select = false,
    },
    ["<Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      else
        fallback()
      end
    end,
    ["<S-Tab>"] = function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      else
        fallback()
      end
    end,
  },

  sources = {
    { name = "buffer" },
    { name = "luasnip" },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "nvim_lua" },
  },
}
-- ]]

-- [[ Telescope
local dropdown = require("telescope.themes").get_dropdown()
local ivy = require("telescope.themes").get_ivy()

local function get_default_telescope_picker_opts()
  return {
    find_files = {
      preview = false,
      theme = dropdown,
      layout_config = {
        height = math.ceil(window_height() * 0.7),
      },
    },
    oldfiles = {
      preview = false,
      theme = dropdown,
      layout_config = {
        height = math.ceil(window_height() * 0.7),
      },
    },
    git_files = {
      preview = false,
      theme = dropdown,
      layout_config = {
        height = math.ceil(window_height() * 0.7),
      },
    },
    live_grep = {
      preview = true,
      theme = dropdown,
      layout_config = {
        height = math.ceil(window_height() * 0.7),
      },
    },
    help_tags = {
      preview = false,
      theme = dropdown,
      layout_config = {
        height = math.ceil(window_height() * 0.7),
      },
    },
    commands = {
      preview = false,
      theme = dropdown,
      layout_config = {
        height = math.ceil(window_height() * 0.7),
      },
    },
  }
end

require("telescope").setup {
  defaults = {
    preview = false,
    prompt_prefix = "🔍 ",
  },
  extensions = {
    file_browser = {
      -- disables netrw and use telescope-file-browser in its place
      hijack_netrw = false,
      mappings = {
        ["i"] = {
          -- your custom insert mode mappings
        },
        ["n"] = {
          -- your custom normal mode mappings
        },
      },
    },
    fzf = {
      fuzzy = true, -- false will only do exact matching
      override_generic_sorter = true, -- override the generic sorter
      override_file_sorter = true, -- override the file sorter
      case_mode = "smart_case", -- or "ignore_case" or "respect_case"
      -- the default case_mode is "smart_case"
    },
  },
}
require("telescope").load_extension "fzf"

local function telescope_wrap(builtin, picker_opts)
  return function()
    picker_opts = picker_opts or {}
    local opts = get_default_telescope_picker_opts()[builtin] or {}
    local theme = opts.theme or {}
    require("telescope.builtin")[builtin](vim.tbl_extend("keep", opts, theme, picker_opts))
  end
end

bind {
  n = {
    ["<leader><leader>"] = { telescope_wrap "find_files", desc = "Find Files" },
    ["<leader>ff"] = { telescope_wrap "find_files", desc = "Find Files" },
    ["<leader>fd"] = { telescope_wrap("find_files", { cwd = "~/dev/dotfiles" }), desc = "Find Dotfile" },
    ["<leader>fg"] = { telescope_wrap "git_files", desc = "Git Files" },
    ["<leader>fr"] = { telescope_wrap "oldfiles", desc = "Recent Files" },
    ["<leader>fh"] = { telescope_wrap "help_tags", desc = "Help" },
    ["<leader>fc"] = { telescope_wrap "commands", desc = "Commands" },

    ["??"] = telescope_wrap "live_grep",
  },
}

require("telescope").load_extension "file_browser"
-- ]]

-- [[ Treesitter
require("nvim-treesitter.configs").setup {
  highlight = {
    enable = true,
  },
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = nil, -- Do not enable for files with more than n lines, int
  },
  textobjects = {
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
    select = {
      enable = true,

      -- Automatically jump forward to textobj, similar to targets.vim
      lookahead = true,

      keymaps = {
        -- You can use the capture groups defined in textobjects.scm
        ["af"] = "@function.outer",
        ["if"] = "@function.inner",
        ["ac"] = "@class.outer",
        ["ic"] = "@class.inner",
      },
    },
  },
}

-- ]]

-- [[ ToggleTerm
require("toggleterm").setup {
  size = function(term)
    if term.direction == "horizontal" then
      return 20
    elseif term.direction == "vertical" then
      return vim.o.columns * 0.4
    end
  end,
  direction = "horizontal",
  float_opts = {
    border = "single",
    height = math.ceil(window_height() * 0.5),
    width = math.ceil(window_width() * 0.3),
    winblend = 3,
  },
}

-- bind {
--   n = {
--     ["<C-1>"] = "<cmd>ToggleTerm<CR>",
--   },
-- }

-- ]]

-- [[ Dashboard

local asciiarts = {
  vim.split(
    [[
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢠⣾⣦⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣴⣿⣿⣿⣿⣦⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⡿⢿⣿⣿⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⢀⣠⣤⣶⣶⣷⣶⣦⣤⣀⠘⢿⣿⡇⢸⣿⡿⠃⣀⣤⣴⣶⣶⣶⣶⣤⣄⡀⠀
⠀⠈⢿⣿⣿⣿⣿⣿⡿⣿⣿⣷⣄⠘⠇⠸⠁⣠⣾⣿⣿⢿⣿⣿⣿⣿⣿⡿⠁⠀
⠀⠀⠈⢻⣿⣿⣿⣿⣦⣤⣈⡉⠛⢁⣤⣤⡈⠛⢉⣁⣤⣼⣿⣿⣿⣿⠟⠀⠀⠀
⠀⠀⠀⠀⠉⠛⠿⠿⢿⠿⠋⣉⣀⠸⣿⣿⠇⠸⠿⣿⣿⡿⠿⠿⠛⠁⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⢀⣴⣿⣿⠋⣠⠀⣤⡄⠐⣶⣤⣤⡀⠀⠀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⢀⣴⣿⣿⡿⢁⣴⣿⠀⢿⣿⣦⡘⢿⣿⣿⣦⡀⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣼⣿⣿⣿⣿⣿⣿⡿⠁⠘⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⣿⡿⠁⠀⠀⠈⢿⣿⣿⣿⣿⣿⣿⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⢹⣿⠿⠿⠛⠉⠀⠀⠀⠀⠀⠀⠉⠛⠿⠿⣿⡟⠀⠀⠀⠀⠀⠀
⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀
]],
    "\n"
  ),

  vim.split(
    [[

                                                                                                           
                                                                                                           
          JJJJJJJJJJJ                            VVVVVVVV           VVVVVVVV iiii                          
          J:::::::::J                            V::::::V           V::::::Vi::::i                         
          J:::::::::J                            V::::::V           V::::::V iiii                          
          JJ:::::::JJ                            V::::::V           V::::::V                               
            J:::::J  aaaaaaaaaaaaa      ssssssssssV:::::V           V:::::Viiiiiii    mmmmmmm    mmmmmmm   
            J:::::J  a::::::::::::a   ss::::::::::sV:::::V         V:::::V i:::::i  mm:::::::m  m:::::::mm 
            J:::::J  aaaaaaaaa:::::ass:::::::::::::sV:::::V       V:::::V   i::::i m::::::::::mm::::::::::m
            J:::::j           a::::as::::::ssss:::::sV:::::V     V:::::V    i::::i m::::::::::::::::::::::m
            J:::::J    aaaaaaa:::::a s:::::s  ssssss  V:::::V   V:::::V     i::::i m:::::mmm::::::mmm:::::m
JJJJJJJ     J:::::J  aa::::::::::::a   s::::::s        V:::::V V:::::V      i::::i m::::m   m::::m   m::::m
J:::::J     J:::::J a::::aaaa::::::a      s::::::s      V:::::V:::::V       i::::i m::::m   m::::m   m::::m
J::::::J   J::::::Ja::::a    a:::::assssss   s:::::s     V:::::::::V        i::::i m::::m   m::::m   m::::m
J:::::::JJJ:::::::Ja::::a    a:::::as:::::ssss::::::s     V:::::::V        i::::::im::::m   m::::m   m::::m
 JJ:::::::::::::JJ a:::::aaaa::::::as::::::::::::::s       V:::::V         i::::::im::::m   m::::m   m::::m
   JJ:::::::::JJ    a::::::::::aa:::as:::::::::::ss         V:::V          i::::::im::::m   m::::m   m::::m
     JJJJJJJJJ       aaaaaaaaaa  aaaa sssssssssss            VVV           iiiiiiiimmmmmm   mmmmmm   mmmmmm
                                                                                                           
                                                                                                           
                                                                                                           
                                                                                                           
                                                                                                           
                                                                                                           
                                                                                                           

  ]],
    "\n"
  ),
}
math.randomseed(os.clock() * 100000000000)
local button = require("alpha.themes.dashboard").button
local myconfig = {
  layout = {
    { type = "padding", val = 2 },
    {
      type = "text",
      val = asciiarts[math.ceil(math.random(1, 100) % 2) + 1],
      opts = { position = "center", hl = "Type" },
    },
    { type = "padding", val = 2 },
    { type = "text", val = "JasVim", opts = { position = "center", hl = "Type" } },
    { type = "padding", val = 1 },
    { type = "text", val = "0.1", opts = { position = "center", hl = "Type" } },
    { type = "padding", val = 2 },
    {
      type = "group",
      val = {
        button("SPC SPC", "  Find file"),
        button("SPC f g", "  Git files"),
        button("??", "  Live Grep"),
        button("SPC f r", "  Recent Files"),
        button("SPC p s", "  Sync Packages", "<cmd>PackerSync<Cr>"),
      },
      opts = { spacing = 1, hl = "Include" },
    },
    { type = "padding", val = 2 },
    {
      type = "text",
      val = string.format("Neovim version %s.%s", vim.version().major, vim.version().minor),
      opts = { position = "center", hl = "Type" },
    },
  },
}
require("alpha").setup(myconfig)
-- ]]

-- [[ Comment
require("todo-comments").setup()
require("Comment").setup {
  ---Add a space b/w comment and the line
  padding = true,
  ---Whether the cursor should stay at its position
  sticky = true,
  ---Lines to be ignored while (un)comment
  ---LHS of toggle mappings in NORMAL mode
  toggler = {
    ---Line-comment toggle keymap
    line = "gcc",
    ---Block-comment toggle keymap
    block = "gbc",
  },
  ---LHS of operator-pending mappings in NORMAL and VISUAL mode
  opleader = {
    ---Line-comment keymap
    line = "gc",
    ---Block-comment keymap
    block = "gb",
  },
  ---LHS of extra mappings
  extra = {
    ---Add comment on the line above
    above = "gcO",
    ---Add comment on the line below
    below = "gco",
    ---Add comment at the end of line
    eol = "gcA",
  },
  ---Enable keybindings
  mappings = {
    ---Operator-pending mapping; `gcc` `gbc` `gc[count]{motion}` `gb[count]{motion}`
    basic = true,
    ---Extra mapping; `gco`, `gcO`, `gcA`
    extra = true,
  },
}
-- ]]

-- [[ Snippets
require("luasnip.loaders.from_vscode").lazy_load()
-- ]]

-- [[ vim-maximizer
bind {
  n = {
    [",z"] = { "<cmd>MaximizerToggle<cr>", desc = "Toggle zoom on current window" },
  },
}
-- ]]

-- [[ Which-key
-- remember we should always register which key after all keybindings are registered
require("which-key").setup()
-- ]]

local font_family = "Fira Code"
local font_size = 15
vim.g.neovide_scroll_animation_length = 0.00
vim.g.neovide_cursor_animation_length = 0.00
vim.g.neovide_cursor_vfx_mode = ""

function Font(font, size)
    font_family = font
    font_size = size
    vim.opt.guifont = string.format("%s:h%d", font, size)
end

function FontSizeInc()
    font_size = 1 + font_size
    Font(font_family, font_size)
end

function FontSizeDec()
    font_size = font_size - 1
    Font(font_family, font_size)
end

function FontSize(size)
    font_size = size
    Font(font_family, font_size)
end

vim.api.nvim_create_user_command("FontSizeInc", function(_)
    FontSizeInc()
end, {})

vim.api.nvim_create_user_command("FontSizeDec", function(_)
    FontSizeDec()
end, {})

vim.api.nvim_create_user_command("FontSize", function(opts)
    FontSize(tonumber(opts.fargs[1]))
end, { nargs = 1 })


vim.keymap.set({ "n", "i", "v", "x", "t" }, "<C-=>", FontSizeInc, {})
vim.keymap.set({ "n", "i", "v", "x", "t" }, "<C-->", FontSizeDec, {})

vim.api.nvim_create_user_command("Font", function(opts)
    local splitted = vim.split(opts.args, ":")
    if #splitted < 2 then
        error("Font command input should be in [FontName]:[FontSize] format")
    end
    Font(splitted[1], splitted[2])
end, { nargs = "*" })

Font("Jetbrains Mono", 16)

return {
    "junegunn/fzf.vim",
    dependencies = { { "junegunn/fzf", build = ":execute fzf#install()" } },
    config = function() vim.g.fzf_preview_window = {} end,
}

local bind = vim.keymap.set
return {
    nnoremap = function(key, fn)
        bind('n', key, fn)
    end,

    inoremap = function(key, fn)
        bind('i', key, fn)
    end,

    tnoremap = function(key, fn)
        bind('t', key, fn)
    end,

    vnoremap = function(key, fn)
        bind('v', key, fn)
    end
}



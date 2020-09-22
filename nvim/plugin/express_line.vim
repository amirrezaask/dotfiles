if exists('g:loaded_express_line_config') | finish | endif
lua << END
    local el_config = require'el_config'
    el_config.setup{}
END

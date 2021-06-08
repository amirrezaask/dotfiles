if get(g:, 'transparent', v:false)
    echo "print"
    lua require('colors.transparent')
endif

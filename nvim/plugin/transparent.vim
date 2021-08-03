if get(g:, 'transparent', v:false)
    lua require('colors.transparent')
    highlight LineNr guifg=#5eacd3
endif


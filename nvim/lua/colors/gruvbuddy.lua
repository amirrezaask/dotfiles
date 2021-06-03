local hl = require('palette').highlight

-- Extracted from https://github.com/tjdevries/gruvbuddy.nvim
hl { "Normal"              , bg='#282c34'          , fg='#e0e0e0' }

hl { "InvNormal"           , bg="#c5c8c6"          , fg="#282c34" }
hl { "NormalFloat"         , bg="#111317"          , fg="#fafafa" }
hl { "LineNr"              , bg="#282a2e"          , fg="#969896" }
hl { "EndOfBuffer"         , fg="#969896"}
hl { "SignColumn"          , bg="#282a2e"          , fg="#969896" }
hl { "Visual"              , bg="#38556d" }
hl { "VisualMode"          , bg="#38556d" }
hl { "VisualLineMode"      , bg="#38556d" }
hl { "Cursor"              , bg="#e0e0e0"          , fg="#282c34" }
hl { "CursorLine"          , bg="#333842"}
hl { "PMenu"               , bg="#373b41"          , fg="#b4b7b4"}
hl { "PMenuSel"            , bg="#fbffad"          , fg="#282c34"}
hl { "PMenuSbar"           , bg="#282c34"}
hl { "PMenuThumb"          , bg="#b4b7b4"}
hl { "Search"              , bg="#f8fe7a"          , fg="#282a2e"}
hl { "TabLine"             , bg="#282a2e"          , fg="#5f89ad"}
hl { "TabLineFill"         , bg="#969896"          , fg="#ebdbb2"}
hl { "TabLineSel"          , bg="#282a2e"          , fg="#ffffff"}
hl { "ColorColumn"         , bg="#81a2be"          , fg="#ffffff"}
hl { "qfFileName"          , fg="#f8fe7a" }

hl { "Special"             , fg="#aa92cd" }

hl { "SpecialChar"         , fg="#a3685a" }
hl { "NonText"             , fg="#4e545c" }
hl { "WhiteSpace"          , fg="#8e6fbd" }
hl { "Conceal"             , bg="#4e545c"          , fg="#282c34" }

hl { "StatusLine"          , bg="#81a2be"          , fg="#373b41"      , styles = {} }

hl { "StatusLineNC"        , bg="#3f4349"          , fg="#969896" }

hl { "User1"               , bg="#f8fe7a"          , fg="#ffffff" }
hl { "User2"               , bg="#cc6666"          , fg="#ffffff" }
hl { "User3"               , bg="#99cc99"          , fg="#ffffff" }
hl { "CommandMode"         , link = "Constant" }
hl { "VisualMode"          , link = "StatusLine" }
hl { "NormalMode"          , link = "Function" }
hl { "InsertMode"          , link = "StatusLine" }
hl { "TerminalMode"        , link = "Constant" }

-- Booleans
hl { "Boolean"             , fg="#7fbfff" }
hl { "TSBoolean"           , link = "Boolean" }

hl { "Error"               , fg="#d98c8c" }
hl { { "Label"             , "TSLabel" }           , fg="#cc6666" }
hl { { "Operator"          , "TSOperator" }        , fg="#e6b3b3" }
hl { "PreProc"             , fg="#f8fe7a" }
hl { { "Repeat"            , "TSRepeat" }          , fg="#cc6666" }
hl { "Repeat"              , fg="#cc6666" }
hl { "Statement"           , fg="#c04040" }
hl { "StorageClass"        , fg="#f8fe7a" }
hl { { "String"            , "TSString" }          , fg="#99cc99" }
hl { "Structure"           , fg="#b294bb" }
hl { { "Tag"               , "TSTag" }             , fg="#f8fe7a" }
hl { "Todo"                , fg="#f8fe7a" }
hl { "Typedef"             , fg="#f8fe7a" }
hl { { "Type"              , "TSType" }            , fg="#b294bb" }
hl { "Folded"              , bg="#4e545c"          , fg="#7c7f7c" }
hl { { "vimFunction"       , "Function"            , "TSFunction"      , "pythonBuiltinFunc" } , fg="#f8fe7a" }

hl { { "Number"            , "Float"               , "TSNumber"        , "TSFloat" }           , fg="#cc6666" }
hl { "MatchParen"          , fg="#8abeb7" }
hl { { "TSCharacter"       , "Character" }         , fg="#cc6666" }
hl { { "TSComment"         , "Comment" }           , fg="#969896"      , styles={"italic"}}
hl { { "Conditional"       , "TSConditional" }     , fg="#f8fe8a"      , styles={"bold"}}
hl { { "Identifier"        , "TSConstant"          , "Constant" }      , fg="#cc6666" }

hl { { "Define"            , "TSInclude"           , "Include" }       , fg="#8abeb7" }
hl { { "vimCommand"        , "vimNotFunc"          , "TSConstant"      , "vimLet" }            , fg="#81a2be" }
hl { "TSVariable"          , fg="#e0e0e0" }
hl { "TSFunction"          , fg="#f8fe7a"          , styles={"bold"}}
hl { { "TSVariableBuiltin" , "TSConstantBuiltin" } , fg="#f8fe7a" }
hl { { "vimIsCommand"      , "vimFuncVar"          , "TSKeyword"       , "Keyword" }           , fg="#b294bb"   , styles={"bold"}}
hl { "TSLabel"             , fg="#cc6666"}


hl { "HelpDoc"             , bg="#698b69"          , fg="#ffffff" }
hl { "HelpIgnore"          , fg="#99cc99" }

hl { "gitDiff"             , fg="#c7c7c7" }
hl { "DiffChange"          , bg="#2800" }
hl { "DiffText"            , bg="#8e00" }
hl { "DiffAdd"             , bg="#2800" }
hl { "DiffDelete"          , bg="#0" }
hl { "DiffRemoved"         , fg="#cc6666" }
hl { "DiffAdded"           , fg="#99cc99" }

hl { "VimwikiBold"         , fg="#cc6666" }
hl { "TelescopeMatching"   , fg="#f2904b"          , styles={"bold"} }

hl { "StartifyBracket"     , fg="#cc6666" }
hl { "StartifyFile"        , fg="#c04040" }
hl { "StartifyNumber"      , fg="#81a2be" }
hl { "StartifyPath"        , fg="#77bb77" }
hl { "StartifySlash"       , fg="#8abeb7" }
hl { "StartifySection"     , fg="#fbffad" }
hl { "StartifySpecial"     , fg="#de935f" }
hl { "StartifyHeader"      , fg="#de935f" }
hl { "StartifyFooter"      , fg="#373b41" }

hl { "foldbraces"          , fg="#f2e5bc" }

hl { "markdownH1"          , fg="#81a2be" }
hl { "markdownH2"          , fg="#a3bbd0" }
hl { "markdownH3"          , fg="#c5d4e1" }

hl { "vimCommentTitle"     , fg="#cc6666" }
hl { "vimMapModKey"        , fg="#8abeb7" }
hl { "vimNotation"         , fg="#8abeb7" }
hl { "vimMapLHS"           , fg="#f8fe7a" }
hl { "vimNotation"         , fg="#8abeb7" }
hl { "vimBracket"          , fg="#96535c" }
hl { "vimMap"              , fg="#698b69" }
hl { "nvimMap"             , fg="#698b69" }
hl { "vimAutoloadFunction" , fg="#f3fe14" }

-- Git signs
hl { "GitSignsAddNr"       , bg='green'}

vim.g.transparent = true

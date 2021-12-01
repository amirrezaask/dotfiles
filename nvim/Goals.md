# What we do in an IDE

## Abilities
- writing the code ( neovim )
    - autocomplete ( LSP )
- Navigation
    - smart language based navigation ( goto definition, ...) ( LSP )
    - simple navigation ( vim motions, ... ) ( neovim )
- Tasks/Actions
    - building ( actions.nvim )
    - testing ( actions.nvim )
    - formatting ( actions.nvim + LSP + ...)
    - ...
- Refactor
    - Rename (LSP)
    - Extract ( variables, classes, types , ...) ( refactor.nvim ? )
- Git ?
    - git status
    - files diff
    - stage chunks
    - commit
    - push
    - pull
    - fetch
- Debuging ( dap )
    - Step Over
    - Step Into
    - Resume
    - Inspect state of program

## Visual
### Main editor view
- my code :)
- highlight stuff ( TODOS, ... )
- git signs for lines ( gitsigns.nvim )
- line numbers
### statusline
- filename
- git branch
- line number, col
- file type

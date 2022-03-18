[ -f "$HOME/.env" ] && source $HOME/.env
source $DOTFILES/zsh/antigen.zsh

antigen use oh-my-zsh

antigen bundle git

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle dracula/zsh

antigen theme dracula/zsh dracula
antigen apply

alias luamake=/Users/amirreza/.local/lua-language-server/3rd/luamake/luamake
[ -f "/Users/amirreza/.ghcup/env" ] && source "/Users/amirreza/.ghcup/env" # ghcup-env

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"



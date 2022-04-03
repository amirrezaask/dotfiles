[ -f "$HOME/env" ] && source $HOME/env
source $DOTFILES/zsh/antigen.zsh

antigen use oh-my-zsh

antigen bundle git

antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle dracula/zsh

antigen theme robbyrussell
antigen apply

# Install starship
command -v 'starship' > /dev/null

if [ "$?" != '0' ]; then
 sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi

alias luamake=/Users/amirreza/.local/lua-language-server/3rd/luamake/luamake
[ -f "/Users/amirreza/.ghcup/env" ] && source "/Users/amirreza/.ghcup/env" # ghcup-env

test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"



[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS='--height 20%'
export FZF_DEFAULT_COMMAND='rg --files'


eval "$(starship init zsh)"

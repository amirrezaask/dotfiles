# DEPREACATED: I use fish
ZSH_CONFIG_BASE_DIR="$HOME/.config/zsh"
source "${ZSH_CONFIG_BASE_DIR}/antigen.zsh"

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions

source ~/env

source "${ZSH_CONFIG_BASE_DIR}/vars.zsh"

source "$ZSH_CONFIG_BASE_DIR/git.zsh"
export PROMPT_ENGINE="oh-my-posh" # "custom" or "starship" or "oh-my-posh
source "$ZSH_CONFIG_BASE_DIR/prompt.zsh"

antigen apply
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

alias luamake=/home/amirreza/.local/lua-language-server/3rd/luamake/luamake

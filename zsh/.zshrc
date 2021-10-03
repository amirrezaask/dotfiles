# DEPREACATED: I use fish
ZSH_CONFIG_BASE_DIR="$HOME/.config/zsh"
source "${ZSH_CONFIG_BASE_DIR}/antigen.zsh"

antigen use oh-my-zsh
antigen bundle git
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
antigen apply

source ~/env

source "${ZSH_CONFIG_BASE_DIR}/vars.zsh"

source "$ZSH_CONFIG_BASE_DIR/git.zsh"
export PROMPT_ENGINE="starship" # "custom" or "starship" or "oh-my-posh
source "$ZSH_CONFIG_BASE_DIR/prompt.zsh"

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

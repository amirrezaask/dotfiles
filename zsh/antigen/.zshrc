ZSH_CONFIG_BASE_DIR="$HOME/.config/zsh"

exec_exists() {
  command -v "$1" >/dev/null 2>&1
}
source "${ZSH_CONFIG_BASE_DIR}/antigen.zsh"

antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle git

# Syntax highlighting bundle.
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-autosuggestions
# antigen bundle zsh-users/zsh-history-substring-search

# Load the theme.
# antigen theme spaceship-prompt/spaceship-prompt
# antigen theme robbyrussell
# Tell Antigen that you're done.
antigen apply

source ~/env

source "${ZSH_CONFIG_BASE_DIR}/vars.zsh"

if ! command -v starship &> /dev/null
then
    sh -c "$(curl -fsSL https://starship.rs/install.sh)"
fi

eval "$(starship init zsh)"

ZSH_CONFIG_BASE_DIR="$HOME/.config/zsh"
ZSH_PLUGINS_DIR="$HOME/.local/zsh/plugins"
ZSH_INCLUDE_DIR="$ZSH_CONFIG_BASE_DIR/include"

PLUGINS=(
    zsh-autosuggestions
    zsh-syntax-highlighting
)

# Load plugins
for plugin in $PLUGINS; do
    source "$ZSH_PLUGINS_DIR/$plugin/$plugin.zsh"
done

# Source all files in auto include dir
for file in $(find "$ZSH_INCLUDE_DIR" -iname "*zsh" -type f); do
    source $file
done


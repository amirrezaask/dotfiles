ZSH_CONFIG_BASE_DIR="$HOME/.config/zsh"
ZSH_PLUGINS_DIR="$HOME/.local/zsh/plugins"
ZSH_INCLUDE_DIR="$ZSH_CONFIG_BASE_DIR/include"

PLUGINS=(
    zsh-autosuggestions
    zsh-syntax-highlighting
    spaceship-prompt
)

SPACESHIP_PROMPT_ORDER=(
    time          # Time stamps section
    user          # Username section
    dir           # Current directory section
    host          # Hostname section
    git           # Git section (git_branch + git_status)
    golang        # Go section
    php           # PHP section
    rust          # Rust section
    docker        # Docker section
    venv          # virtualenv section
    dotnet        # .NET section
    kubectl       # Kubectl context section
    terraform     # Terraform workspace section
    exec_time     # Execution time
    line_sep      # Line break
    jobs          # Background jobs indicator
    exit_code     # Exit code section
    char          # Prompt character
)

# Load plugins
for plugin in $PLUGINS; do
    source "$ZSH_PLUGINS_DIR/$plugin/$plugin.zsh"
done

# Source all files in auto include dir
for file in $(find "$ZSH_INCLUDE_DIR" -iname "*zsh" -type f); do
    source $file
done


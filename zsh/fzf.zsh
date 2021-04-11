# Setup fzf
# ---------
if [[ ! "$PATH" == */home/amirreza/src/github.com/junegunn/fzf/bin* ]]; then
  export PATH="${PATH:+${PATH}:}/home/amirreza/src/github.com/junegunn/fzf/bin"
fi

# Auto-completion
# ---------------
[[ $- == *i* ]] && source "/home/amirreza/src/github.com/junegunn/fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/home/amirreza/src/github.com/junegunn/fzf/shell/key-bindings.zsh"

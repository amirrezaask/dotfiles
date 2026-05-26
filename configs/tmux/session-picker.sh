#!/usr/bin/env zsh
PROJECTS_DIR="$HOME/dev"

# Build list: scratch option + git projects
scratch_option="scratch"
projects=$(find "$PROJECTS_DIR" -maxdepth 3 -name ".git" -type d 2>/dev/null | sed 's|/.git$||' | sed "s|^$PROJECTS_DIR/||")
selected=$(echo -e "$scratch_option\n$projects" | fzf --prompt "Session: ")
[ -z "$selected" ] && exit 0

# Handle scratch session
if [ "$selected" = "$scratch_option" ]; then
  session_name="scratch"
  target_dir="$HOME/scratch"
  mkdir -p "$target_dir"
else
  session_name=$(basename "$selected" | tr . _)
  target_dir="$PROJECTS_DIR/$selected"
fi

if tmux has-session -t="$session_name" 2>/dev/null; then
  tmux switch-client -t="$session_name"
else
  tmux new-session -ds "$session_name" -n neovim -c "$target_dir"

  tmux new-window -t "$session_name" -n zsh -c "$target_dir"

  tmux new-window -t "$session_name" -n agent -c "$target_dir"

  tmux select-window -t "$session_name:1"
  tmux switch-client -t="$session_name"
fi

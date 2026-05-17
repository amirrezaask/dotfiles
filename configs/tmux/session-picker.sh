#!/usr/bin/env bash
PROJECTS_DIR="$HOME/dev"

selected=$(find "$PROJECTS_DIR" -maxdepth 3 -name ".git" -type d 2>/dev/null | sed 's|/.git$||' | sed "s|^$PROJECTS_DIR/||" | fzf --reverse --prompt "Project: ")
[ -z "$selected" ] && exit 0

session_name=$(basename "$selected" | tr . _)

if tmux has-session -t="$session_name" 2>/dev/null; then
  tmux switch-client -t="$session_name"
else
  tmux new-session -ds "$session_name" -c "$PROJECTS_DIR/$selected"
  tmux switch-client -t="$session_name"
fi
